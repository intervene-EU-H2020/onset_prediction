#' Fit a Cox Proportional Hazards Model
#'
#' This function fits a Cox Proportional Hazards Model for a 
#' given surivival analysis setup and study, using the [survival::coxph] function.
#' 
#' For i.e. a `study` with endpoint `J10_ASTHMA` and `surv_ana` with covs
#' `covs = c("SEX", "YEAR_OF_BIRTH")` the model would be 
#' `Surv(J10_ASTHMA_AGE_DAYS, J10_ASTHMA) ~ SCORE + SEX +
#' YEAR_OF_BIRTH`.
#' 
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#' @param study An S4 study object. The current study.
#' 
#' @return A Cox-PH model object or `NULL` if the model couldn't be fit.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_coxph_mdl <- function(surv_ana,
                          study) {
    coxph_mdl <- NULL

    if(nrow(study@study_data) > 0) {
        coxph_formula <- get_coxph_formula(preds=surv_ana@preds, endpt=study@endpt)  
        study <-  Istudy::updateStudyData(study, scale_preds(preds=surv_ana@preds, study_data=study@study_data))
        coxph_mdl <- tryCatch({
            suppressWarnings(survival::coxph(formula=coxph_formula, 
                                             data=study@study_data,
                                             # Larger fit object but no need for
                                             # other functions to reconstruct
                                             # which fails in this setup
                                             model=TRUE))
                      }, error = function(e) {
                                write_to_error_file(surv_ana@error_file, msg=paste0(e, collapse="\n"))
                                return(NULL)})
        write_coxph_to_log(coxph_mdl, study, surv_ana) 
    }

    return(coxph_mdl)
}

#' Creates the forumla string for the Cox-PH model
#' 
#' Will be i.e. 
#' `Surv(J10_ASTHMA_AGE_FOR_BASE, J10_ASTHMA) ~ SEX + YEAR_OF_BIRTH`
#' for endpoint `J10_ASTHMA` and predictors `SEX` and `YEAR_OF_BIRTH`.
#' 
#' @param preds A string (vector). The predictors for the model.
#' @param endpt A string. The current endpoint. 
#' 
#' @return The formula for the Cox-PH model.
#' 
#' @export 
#' 
#' @return A string. The formula string for the Cox-PH model.
get_coxph_formula <- function(preds,
                              endpt) {
    pred_string <- paste0(preds, collapse="+")
    #pred_string <- paste0(pred_string, " + tt(YEAR_OF_BIRTH)")
    coxph_formula <- stats::as.formula(paste0("survival::Surv(", endpt, "_AGE_FROM_BASE, ",  endpt, ") ~ ",  pred_string))
    return(coxph_formula)
}

add_time_interact_terms <- function(coxph_formula,
                                    signf_zph_preds,
                                    endpt) {
    signf_zph_preds <- signf_zph_preds[signf_zph_preds != "GLOBAL"]
    # New interaction term
    interact_terms <- paste0("tt(", signf_zph_preds, ")", collapse=" + ")
    # Getting predictor part of formula
    preds <- stringr::str_split(deparse(coxph_formula), "~")[[1]][2]
    surv_part <- stringr::str_split(deparse(coxph_formula), "~")[[1]][1]

    # Removing predictors that now is part of the interaction term
    preds <- paste0(stringr::str_remove_all(unlist(stringr::str_split(preds, "\\+")), " "), collapse="+")
    #preds <- preds[!(preds %in% signf_zph_preds)]
    
    # Creating new formula
    coxph_formula <- as.formula(paste0(surv_part, " ~ ", preds, " + ", interact_terms))
    print(deparse(coxph_formula))
    return(coxph_formula)
}

write_coxph_to_log <- function(coxph_mdl,
                              study,
                              surv_ana) { 
    if(!is.null(coxph_mdl)) {
        file_path <- get_full_file_name_path(res_type="log",
                                            study_setup=study@study_setup,
                                            endpt=study@endpt,
                                            surv_ana=surv_ana)
        readr::write_file(paste0("\nSummary cox-PH model:\n\nNo of cases:", summary(coxph_mdl)$nevent, "\nNo of ctrls:", summary(coxph_mdl)$n-summary(coxph_mdl)$nevent, "\n\nzph original model:\n\n"),
                        file_path)
        zph_res <-  tryCatch({
                            tibble::as_tibble(survival::cox.zph(coxph_mdl)$table, rownames="PRED")
                        }, error = function(e) {
                            return("Couldn't calculate zph")
                        })
        if(is.data.frame(zph_res)) {
            readr::write_delim(zph_res,
                               file=file_path,
                               append=TRUE,
                               col_names=TRUE)
        } else {
            readr::write_file(zph_res,
                              file=file_path,
                              append=TRUE)
        }
    }
}

#' Adding new zph analysis of the updated coxph model with interaction terms to the log
#' 
#' @param coxph_mdl The updated coxph model
#' @param study The study object
#' @param surv_ana The survival analysis object
update_coxph_log <- function(coxph_mdl, study, surv_ana) {
    file_path <- get_full_file_name_path(res_type="log",
                                         study_setup=study@study_setup,
                                         endpt=study@endpt,
                                         surv_ana=surv_ana)
    readr::write_file("\n\n zph updated model:\n\n", file=file_path, append=TRUE)
    zph_res <-  tryCatch({
                        tibble::as_tibble(survival::cox.zph(coxph_mdl)$table, rownames="PRED")
                    }, error = function(e) {
                        return("Couldn't calculate zph")
                    })
    if(is.data.frame(zph_res)) {
        readr::write_delim(zph_res,
                           file_path,
                           append=TRUE,
                           col_names=TRUE)
    } else {
        readr::write_file(zph_res,
                          file=file_path,
                          append=TRUE)
    }
}


#' Scales the given variables in the score data
#'
#' @param preds A string vector of the variables to be scaled
#' @param study_data A data frame with the variables to be scaled
#'
#' @return A data frame with the scaled variables
#'
#' @export
scale_preds <- function(preds,
                        study_data) {
    for(pred in preds) {
        if(pred %in% colnames(study_data)) {
            if(pred %in% c("PRS", "CCI", "EI", "YEAR_OF_BIRTH", "MED", "EDU_cont", "PheRS", "Prob")) {
                study_data[,pred] <- scale(study_data[,pred])
            } 
            if(pred %in% c("EDU", "ZIP")) {
                study_data[,pred] <- as.factor(dplyr::pull(study_data, pred))
            }
        }
    }

    study_data$SEX <- factor(study_data$SEX, levels=c("female", "male"))
    return(study_data)
}

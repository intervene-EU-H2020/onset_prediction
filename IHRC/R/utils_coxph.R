#' Makes all character covariates a factor
#'  
#' @param study_data A data.frame with all eligible individuals under 
#'                  the chosen study setup and their scores. Needs at 
#'                  least the columns defines in `covs`.
#' @param covs A vector of characters. The column names of the covariates 
#'              to add to the predictor of the Cox-PH model.
#' 
#' @author Kira E. Detrois
make_covs_fctrs <- function(study_data,
                            covs) {
    for(cov in covs) {
        if(is.character(study_data[,cov])) {
            study_data <- dplyr::mutate_at(
                                    .tbl=study_data, 
                                    .vars=cov, 
                                    .funs=as.factor)
        }
    }
    return(study_data)
}

#' Scales the given variables in the score data
#'
#' @param preds A character vector of the variables to be scaled
#' @param study_data A data frame with the variables to be scaled
#'
#' @return A data frame with the scaled variables
#'
#' @export
scale_preds <- function(preds,
                        study_data) {
    for(pred in preds) {
        if(pred %in% colnames(study_data)) {
            if(pred %in% c("PRS", "CCI", "EI", "YEAR_OF_BIRTH", "MED", "EDU", "PheRS")) {
                study_data[,pred] <- scale(study_data[,pred])
            }
        }
    }
    study_data$SEX <- factor(study_data$SEX, levels=c("female", "male"))
    return(study_data)
}

#' Creates the forumla string for the Cox-PH model
#' 
#' @inheritParams get_coxph_mdl
#' 
#' @export 
#' 
#' @return A character. The formula string for the Cox-PH model.
get_coxph_formula <- function(preds,
                              endpt) {
    pred_string <- paste0(preds, collapse="+")
    stats::as.formula(paste0("survival::Surv(", endpt, "_AGE_FROM_BASE, ",  endpt, ") ~ ",  pred_string))
}

#' Creates a survival object for the selected endpoint
#' 
#' Creates a survival object for the selected endpoint. With time
#' being the age in years until onset or the age at the end of the
#' observation period.
#' 
#' @inheritParams make_covs_fctrs
#' @inheritParams get_n_group_cases
#' 
#' @author Kira E. Detrois
#' 
#' @export
get_surv_obj <- function(study_data,
                         endpt) {
    
    survival::Surv(time=dplyr::pull(study_data, 
                                    paste0(endpt, "_AGE_FROM_BASE")),
                   event=dplyr::pull(study_data, 
                                     endpt))
}
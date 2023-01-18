#' Get All Preds Sorted
#' 
#' Returns a sorted character vector of all predictors being used in the survival 
#' analysis. Interaction predictors are sorted separately from non-interaction 
#' predictors.
#' 
#' @inheritParams run_surv_studies
#' 
#' @return A character vector of all predictors being used in the survival analysis, 
#' sorted by predictor name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_all_preds_sorted <- function(score_type="", 
                                 covs) {
    interact_preds <- score_type[stringr::str_detect(score_type, "[*]")]

    if(all(score_type != "")) {
        if(length(interact_preds) > 0) {
            non_interact_preds <- score_type[!stringr::str_detect(score_type, "[*]")]
            non_interact_preds <- non_interact_preds[order(non_interact_preds)]
            interact_preds <- interact_preds[order(interact_preds)]
            score_type <- c(non_interact_preds, interact_preds)
        } else {            
            score_type <- score_type[order(score_type)]
        }
    }
    covs <- covs[order(covs)]
    if(all(score_type != "")) {
        all_preds <- c(score_type, covs)
    } else {
        all_preds <- covs
    }
    return(all_preds)
}


#' Creates directory name for the downsampling factor
#' 
#' @inheritParams run_surv_studies
#' 
#' @author Kira E. Detrois
get_down_dir <- function(down_fctr) {
    ifelse(all(is.na(down_fctr)),
           "no_down/",
           paste0("down_", down_fctr, "/")) 
}

#' Adds downsampling and filter information to results directory path
get_full_res_path <- function(write_res,
                              res_dir,
                              down_fctr,
                              study_type,
                              score_type) {
    score_type_dir_name <- get_score_type_dir_name(score_type)

    if(write_res) {
        res_dir <- paste0(res_dir, study_type, "/", get_down_dir(down_fctr), score_type_dir_name, "/")
    } else {
        res_dir <- NULL
    }
}

#' Creats the file directory and name for the different result types
#' 
#' @param res_type A character. The results type.
#' 
#' @return A character. The file name.
#' 
#' @author Kira E. Detrois
check_and_get_file_path <- function(res_type,
                                    study_setup,
                                    endpt,
                                    surv_ana) {
    if(surv_ana@write_res) {
        crnt_res_dir <- paste0(surv_ana@res_dir, res_type, "/")
        # Make the folder if it doesn't exist yet
        if(check_res_dir(surv_ana@write_res, crnt_res_dir)) {
            res_file_end <- dplyr::case_when(
                                res_type == "HR" ~ "_HRs.png",
                                res_type == "coxph" ~ "_coxph.tsv",
                                res_type == "cidx" ~ "_cidx.tsv",
                                res_type == "pheno_score" ~ "_elig_indv.tsv",
                                res_type == "log" ~ "_log.txt"
                            )
            file_name <- get_file_name(res_type=res_type,
                                       study_setup=study_setup,
                                       endpt=endpt,
                                       surv_ana=surv_ana)
            file_path <- paste0(crnt_res_dir,
                                file_name, 
                                res_file_end)
        }        
        return(file_path)
    }
    return(NA_character_)
}

#' Creats the file name for the HR plots with continuous scores
#' 
#' @return A character. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_file_name <- function(res_type,
                          study_setup,
                          endpt,
                          surv_ana) {
    # Add date or endpoint infor
    if(study_setup@study_type == "forward" | 
            res_type %in% c("log", "pheno_score")) {
        file_name <- endpt
    } else {
        file_name <- study_setup@obs_end_date
    }

    # Add study setup info
    file_name <- paste0(file_name, "_", Istudy::get_ewo_file_name(
                                                study_setup@study_type,
                                                study_setup@exp_len,
                                                study_setup@wash_len,
                                                study_setup@obs_len))

    # Add age range info
    file_name <- paste0(file_name, "_", 
                        paste0(study_setup@obs_age_range, collapse="_"))

    # Add Predictors
    if(res_type == "HR") {
        file_name <- paste0(file_name,  "_", get_preds_file_name(surv_ana@plot_preds))
        if(!(length(surv_ana@plot_preds) == length(surv_ana@preds))) {
            file_name <- paste0(file_name, "_p_", get_preds_file_name(setdiff(surv_ana@preds, surv_ana@plot_preds)))
        }
    } else {
        file_name <- paste0(file_name,  "_", get_preds_file_name(surv_ana@preds))
    }

    # Add extra predictors
    if(surv_ana@res_descr != "") {
        file_name <- paste0(file_name, "_", surv_ana@res_descr)
    }
    return(file_name)
}

get_preds_file_name <- function(preds) {
    preds <- stringr::str_replace_all(preds, " ", "_")
    preds <- stringr::str_replace_all(preds, "[*]", "i")
    preds <- stringr::str_replace_all(preds, "YEAR_OF_BIRTH", "YOB")
    preds <- stringr::str_replace_all(preds,  "EDU", "Edu")

    n_pcs <- sum(stringr::str_count(preds, "PC"))
    if(n_pcs > 1) {
        preds <- preds[!stringr::str_detect(preds, "PC")]
        preds <- c(preds, "PCs")
    } 
    file_name <- paste0(preds, collapse="_")
    return(file_name)
}


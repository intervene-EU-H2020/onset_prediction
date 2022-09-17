#' Creates directory name for the downsampling factor
#' 
#' @inheritParams run_surv_studies
#' 
#' @author Kira E. Detrois
get_down_dir <- function(downsample_fctr) {
    ifelse(all(is.na(downsample_fctr)),
           "no_down/",
           paste0("down_", downsample_fctr, "/")) 
}

#' Creats the file directory and name for the different result types
#' 
#' @param res_type A character. The results type.
#' 
#' @return A character. The file name.
#' 
#' @author Kira E. Detrois
check_and_get_file_path <- function(surv_ana,
                                    res_type) {
    if(surv_ana@write_res) {
        # Results type specific folder
        curnt_res_dir <- paste0(surv_ana@res_dir, paste0(res_type, "/", surv_ana@study@study_type, "/"))
        if(res_type == "HR") {
            curnt_res_dir <- paste0(curnt_res_dir, get_preds_file_name(surv_ana@preds), "/")
        }
        # Make the folder if it doesn't exist yet
        if(Istudy::check_res_dir(surv_ana@write_res, curnt_res_dir)) {
            res_file_end <- dplyr::case_when(
                                res_type == "HR" ~ "HRs.png",
                                res_type == "coxph" ~ "coxph.tsv",
                                res_type == "cidx" ~ "cidx.tsv"
                            )
            file_path <- paste0(curnt_res_dir, get_file_name(surv_ana, res_type), "_", res_file_end)
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
get_file_name <- function(surv_ana,
                          res_type) {
    if(surv_ana@study@study_type == "forward") {
        file_name <- surv_ana@study@endpt
    } else {
        file_name <- surv_ana@study@obs_end_date
    }
    file_name <- paste0(file_name, "_", get_ewo_file_name(surv_ana), "_")
    if(res_type == "HR") {
        file_name <- paste0(file_name, get_preds_file_name(surv_ana@plot_preds))
    } else {
        file_name <- paste0(file_name, get_preds_file_name(surv_ana@preds))
    }
    return(file_name)
}

get_preds_file_name <- function(preds) {
    preds <- stringr::str_replace_all(preds, " ", "_")
    preds <- stringr::str_replace_all(preds, "[*]", "i")
    preds <- stringr::str_replace_all(preds, "YEAR_OF_BIRTH", "YOB")
    n_pcs <- sum(stringr::str_count(preds, "PC"))
    if(n_pcs > 1) {
        preds <- preds[!stringr::str_detect(preds, "PC")]
        preds <- c(preds, "PCs")
    } 
    file_name <- paste0(preds, collapse="_")
    return(file_name)
}

get_ewo_file_name <- function(surv_ana) {
    if(surv_ana@study@study_type == "foward") {
        paste0("e", surv_ana@study@exp_len, "_w", surv_ana@study@wash_len, "_o", surv_ana@study@obs_len)
    } else {
        if(length(surv_ana@study@exp_len) == 1) {
            paste0("o", surv_ana@study@obs_len, "_w", surv_ana@study@wash_len, )
        }
        else {
            paste0("o", surv_ana@study@obs_len, "_w", surv_ana@study@wash_len, "_e", surv_ana@study@exp_len)
        }
    }
}
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
                              study_type) {
    if(write_res) {
        res_dir <- paste0(res_dir, study_type, "/", get_down_dir(down_fctr))
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
check_and_get_file_path <- function(ana_details,
                                    res_type) {
    if(ana_details$write_res) {
        # Results type specific folder
        if(!("full_res_dir" %in% names(ana_details))) {
            curnt_res_dir <- paste0(ana_details$res_dir, res_type, "/")
        } else {
            curnt_res_dir <- ana_details$full_res_dir
        }

        # Make the folder if it doesn't exist yet
        if(Istudy::check_res_dir(ana_details$write_res, curnt_res_dir)) {
            res_file_end <- dplyr::case_when(
                                res_type == "HR" ~ "_HRs.png",
                                res_type == "coxph" ~ "_coxph.tsv",
                                res_type == "cidx" ~ "_cidx.tsv"
                            )
            if("res_file_name" %in% names(ana_details)) {
                file_name <- ana_details$res_file_name
            } else {
                file_name <- get_file_name(ana_details,
                                           res_type)
            }
            file_path <- paste0(curnt_res_dir, 
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
get_file_name <- function(ana_details,
                          res_type) {
    if(ana_details$study_type == "forward") {
        file_name <- ana_details$endpt
    } else {
        file_name <- ana_details$obs_end_date
    }
    file_name <- paste0(file_name, "_", Istudy::get_ewo_file_name(ana_details$study_type,
                                                                  ana_details$exp_len,
                                                                  ana_details$wash_len,
                                                                  ana_details$obs_len))
    file_name <- paste0(file_name, "_", paste0(ana_details$obs_age_range, collapse="_"))
    if(res_type == "HR") {
        file_name <- paste0(file_name,  "_", get_preds_file_name(ana_details$plot_preds))
        if(!(length(ana_details$plot_preds) == length(ana_details$preds))) {
            file_name <- paste0(file_name, "_p_", get_preds_file_name(setdiff(ana_details$preds, ana_details$plot_preds)))
        }
    } else {
        file_name <- paste0(file_name,  "_", get_preds_file_name(ana_details$preds))
    }
    if(ana_details$res_descr != "") {
        file_name <- paste0(file_name, "_", ana_details$res_descr)
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


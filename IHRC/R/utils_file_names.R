#' Get All Preds Sorted
#' 
#' Returns a sorted character vector of all predictors being used in the survival 
#' analysis. Interaction predictors are sorted separately from non-interaction 
#' predictors.
#' 
#' @inheritParams run_surv_studies
#' 
#' @return A string vector of all predictors being used in the survival analysis, 
#' sorted by predictor name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_all_preds_sorted <- function(score_type, 
                                 covs=NULL) {
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
    if(!is.null(covs) & !(all(covs == ""))) {
        covs <- covs[order(covs)]
        if(all(score_type != "")) {
            score_type <- c(score_type, covs)
        } else {
            score_type <- covs
        }
    }  
    return(score_type)
}

#' Adds downsampling and filter information to results directory path
#' 
#' @param write_res A boolean. Defines whether to save the results to files.
#' @param res_dir A string. The directory to write the results and log to.
#' @param down_fctr A numeric. Defines how many controls there should be for every case.
#' @param study_type A string. Can be either `forward` or `backward`. 
#' @param score_type A string (vector). The score types used in the analysis.
#' @param res_descr A string. An addition to add to the results directory.
#' 
#' @return A string. The updated results directory path.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_full_res_path <- function(write_res,
                              res_dir,
                              down_fctr,
                              study_type,
                              score_type,
                              res_descr) {
 
    if(write_res) {
        if(all(score_type != "")) {
            score_type_dir_name <- get_score_type_dir_name(score_type, res_descr)
        } else {
            score_type_dir_name <- "baseline"
        }
        res_dir <- paste0(res_dir, score_type_dir_name, "/")
    } else {
        res_dir <- NULL
    }
}

#' Creates directory name for the downsampling factor
#' 
#' If the downsampling factor is NA then the direcotry is 
#' named `no_down`.
#' 
#' @param down_fctr A numeric. Defines how many controls there should be for every case.
#' 
#' @return A string. The directory name describing the downsampling factor used.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_down_dir <- function(down_fctr) {
    ifelse(all(is.na(down_fctr)),
           "no_down/",
           paste0("down_", down_fctr, "/")) 
}

#' Turns the score types and results description into a directory name
#' 
#' @param score_type A string (vector). The score types used in the analysis.
#' @param res_descr A string. An addition to add to the results directory.
#' 
#' @return A string. The directory name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_score_type_dir_name <- function(score_type, res_descr=NULL) {
    dir_name <- paste0(score_type[order(score_type)], collapse="_")
    if(!is.null(res_descr)) {
        dir_name <- paste0(dir_name, "_", res_descr)
    }
    return(dir_name)
}


#' Creats the file directory and name for the different result types
#' 
#' @param res_type A string. The results type.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param endpt A string. The current endpoint.
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#' 
#' @return A string. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_full_file_name_path <- function(res_type,
                                    study_setup,
                                    endpt=NULL,
                                    surv_ana) {
    if(res_type %in% c("pheno_score", "log")) crnt_res_dir <- paste0(surv_ana@res_dir, res_type, "/")
    else crnt_res_dir <- surv_ana@res_dir
    check_res_dir(surv_ana@write_res, crnt_res_dir)

    # Make the folder if it doesn't exist yet
    res_file_end <- dplyr::case_when(
                                res_type == "cor" ~ "_cors.tsv",
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
    return(file_path)
}

#' Creats the file name for the HR plots with continuous scores
#' 
#' @param res_type A string. The results type.
#' @param study_setup An S4 `study_setup` object. The current study setup. 
#'                      See class definition [Istudy::study_setup].
#' @param endpt A string. The current endpoint.
#' @param surv_ana An S4 `surv_ana` object. The current survival analysis setup. 
#'                      See class definition [IHRC::surv_ana].
#'  
#' @return A string. The file name.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
get_file_name <- function(res_type,
                          study_setup,
                          endpt=NULL,
                          surv_ana) {
    # Add date or endpoint information
    if(res_type %in% c("pheno_score", "log")) {
        file_name <- paste0(endpt, "_", study_setup@obs_end_date)
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
    if(res_type == "log") {
        file_name <- paste0(file_name,  "_", get_preds_file_name(surv_ana@preds))
    }

    return(file_name)
}

#' Renames predictors for nicer files names and creates file name
#' 
#' @param preds A string (vector). The predictors to rename.
#' 
#' @return A string. The file name.
#' 
#' @author Kira E. Detrois
#' 
#' @export 
get_preds_file_name <- function(preds) {
    preds <- stringr::str_replace_all(preds, " ", "_")
    preds <- stringr::str_replace_all(preds, "[*]", "i")
    preds <- stringr::str_replace_all(preds, "YEAR_OF_BIRTH", "Age")
    preds <- stringr::str_replace_all(preds, "SEX", "Sex")
    preds <- stringr::str_replace_all(preds,  "EDU", "Edu")

    n_pcs <- sum(stringr::str_count(preds, "PC"))
    if(n_pcs > 1) {
        preds <- preds[!stringr::str_detect(preds, "PC")]
        preds <- c(preds, "PCs")
    } 
    file_name <- paste0(preds, collapse="_")
    return(file_name)
}


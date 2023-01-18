#' Runs standard setup survival model
#' 
#' Depending on the score types selected sets the covariants. All models
#' use the covariates sex and year of birth. Analyses with `PRS´, 
#' additionally use the PCs, and Batch. 
#' 
#' The exposure period is always 10 years, if washout is selected the 
#' period is 2 years and the observation period 8 years. Otherwise, the 
#' observation period is 10 years. 
#' 
#' If `PheRS` selected as a score, also gets the test individuals matrix 
#' for each endpoints. 
#' 
#' @param score_type 
#' 
#' @author Kira E. Detrois
run_ana_setup_file <- function(setup_file_path) {
    setup <- read_setup_file(setup_file_path)
    print(as.Date(setup$obs_end_date))
    data <- IUtils::get_all_data(score_type=setup$score_type,
                                endpts=setup$endpts,
                                pheno_file_path=setup$pheno_file_path,
                                icd_file_path=setup$icd_file_path,
                                prs_dir_path=setup$prs_dir_path,
                                phers_dir_path=setup$phers_dir_path,
                                phers_study_descr=get_phers_file_descr(study_type=setup$study_type,
                                                                       obs_end_date=as.Date(setup$obs_end_date),
                                                                       exp_len=setup$exp_len,
                                                                       wash_len=setup$wash_len,
                                                                       obs_len=setup$obs_len))
    covs <- get_crnt_covs(setup$score_type, setup$covs)
    if(FinnGen %in% names(setup) & any(stringr::str_detect(score_type, "PRS"))) {
        print("oh dear")
        #endpt_indvs_mat <- create_endpt_indvs_mat(data$pheno, endpts, setup$FinnGen)
    }
    if(any(stringr::str_detect(score_type, "PheRS"))) {
        endpt_indvs_mat <-  IUtils::read_phers_endpt_indvs_mat(
                                                phers_dir_path,
                                                indvs_ids=data$pheno$ID,
                                                endpts=setup$endpts,
                                                prev_endpt_idnvs_mat=endpt_indvs_mat)
    }

    # }
    # res <- IHRC::run_surv_studies(pheno_data=data$pheno, 
    #                               endpt_indvs_mat=endpt_indvs_mat,
    #                               icd_data=data$icd,
    #                               prs_data=data$prs,
    #                               phers_data=data$phers,
    #                               score_type=score_type,
    #                               study_type="backward",
    #                               endpts=endpts,
    #                               exp_len=10,
    #                               wash_len=2,
    #                               obs_len=8,
    #                               obs_end=obs_end_date,
    #                               down_fctr=down_fctr,
    #                               ancs="EUR",
    #                               obs_age_range=c(32,70),
    #                               covs=crnt_covs,
    #                               write_res=TRUE)
} 

get_crnt_covs <- function(score_type, covs) {
    if(is.null(covs)) {
        if(any(string::str_detect(score_type, "PRS"))) {
            covs <- c("SEX", "YEAR_OF_BIRTH", paste0("PC", 1:10), "BATCH")
        } else {
            covs <- c("SEX", "YEAR_OF_BIRTH")
    } 
    return(covs)
}

read_setup_file <- function(setup_file_path) {
    setup_tib <- readr::read_delim(setup_file_path, col_names=FALSE, show_col_types=FALSE)
    setup <- as.list(setup_tib$X2)
    names(setup) <- setup_tib$X1

    for(elem_name in names(setup)) {
        if(setup[elem_name] == "NULL") {
            setup[elem_name] <- NULL
        } else if(stringr::str_detect(setup[[elem_name]], ", ")) {
            setup[elem_name] <- as.vector(stringr::str_split(setup[[elem_name]], pattern=", "))
        } else if(stringr::str_detect(setup[[elem_name]], ",")) {
            setup[elem_name] <- as.vector(stringr::str_split(setup[[elem_name]], pattern=","))
        } else if(!stringr::str_detect(setup[[elem_name]], "\\D")) {
            setup[elem_name] <- as.numeric(setup[[elem_name]])
        } else if(stringr::str_detect(setup[[elem_name]], "^(TRUE|FALSE)$")) {
            setup[elem_name] <- as.logical(setup[[elem_name]])
        }
    }
    return(setup)
}
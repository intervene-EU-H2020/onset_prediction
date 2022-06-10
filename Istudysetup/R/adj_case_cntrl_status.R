adj_case_cntrl_status <- function(pheno_data,
                                  endpt) {
                                      
    test_endpt_input_correct(as.list(environment()))
    cases_to_cntrls <- dplyr::filter(pheno_data, 
                                     get(paste0(endpt, "_DATE")) > 
                                        lubridate::int_end(STUDY_TIME))
    pheno_data[pheno_data$ID %in% cases_to_cntrls$ID, endpt] = rep(0, nrow(cases_to_cntrls))

    return(pheno_data)
}
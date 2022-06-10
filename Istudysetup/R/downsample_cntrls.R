downsample_cntrls <- function(pheno_data,
                              endpt,
                              downsample_fctr=4) {
    test_endpt_input_correct(as.list(environment()))

    n_cases <- sum(pheno_data[[endpt]])
    n_cntrls <- downsample_fctr*n_cases

    cntrl_idxs <- which(pheno_data[[endpt]] == 0)
    case_idxs <- which(pheno_data[[endpt]] == 1)

    cntrl_idxs_down <- sample(cntrl_idxs, size=n_cntrls, replace=FALSE)
    select_idxs <- sort(c(cntrl_idxs_down, case_idxs))
    pheno_data <- pheno_data[select_idxs,]
    return(pheno_data)
}
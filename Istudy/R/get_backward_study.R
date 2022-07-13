#' @export 
get_backward_study <- function(pheno_data,
                               endpt="I9_VTE",
                               wash_len=2,
                               obs_len=8,
                               obs_end=NULL,
                               downsample_fctr=NA_real_,
                               ancs=NA_character_) {
    study <- methods::new("study",
                          endpt=endpt,
                          exp_age=0,
                          exp_len=get_indv_exp_len(pheno_data, wash_len, obs_len, obs_end),
                          wash_len=wash_len,
                          obs_len=obs_len,
                          downsample_fctr=downsample_fctr,
                          ancs=ancs)
    return(study)
}

#' @importFrom lubridate %m-%
get_indv_exp_len <- function(pheno_data,
                             wash_len=2,
                             obs_len=8,
                             obs_end=NULL) {
    if(is.null(obs_end)) {
        obs_end <- get_max_date(pheno_data)
    }

    start_obs <- obs_end %m-% lubridate::years(wash_len + obs_len)
    exp_dur <- lubridate::as.duration(pheno_data$DATE_OF_BIRTH %--% start_obs)
    indv_exp_len <- lubridate::time_length(exp_dur, "years")
    
    return(indv_exp_len)
}

#' @export 
get_max_date <- function(pheno_data) {
    pheno_data <- filter_not_all_na_cols(pheno_data)
    all_dates <- dplyr::select(pheno_data, 
                               dplyr::matches("(*.)_DATE$"))
    column_max <- all_dates %>%
                    dplyr::summarise_all(.funs=function(x) {max(x, na.rm=TRUE)})
    max_date <- as.Date(apply(column_max, 1, max))
    return(max_date)
}

filter_not_all_na_cols <- function(pheno_data) {
    pheno_data[,!(colSums(is.na(pheno_data)) == nrow(pheno_data))]

}
#' Creates a data.frame for testing purposes.
#' 
#' @param n_indv An integer. Number of individuals to simulate.
#' 
#' @return a data.frame with the simulated data.
#' 
#' @export 
#' 
#' @author Kira E. Detrois
create_test_df <- function(n_indv=10) {
    ids <- create_indv_ids(n_indv)
    sex_samples <- draw_sex_samples(n_indv)
    bd_samples <- draw_bd_samples(n_indv)
    sf_samples <- draw_sf_samples(n_indv, bd_samples)
    ef_samples <- draw_ef_samples(n_indv, sf_samples)

    anc_samples <- draw_anc_samples(n_indv) 
    breast_cancer_samples <- draw_sex_spec_cases(n_indv, sex_samples, "female", 0.07, sf_samples)
    t2d_samples <- draw_cases(n_indv, 0.15, 0.03, sf_samples)
    prost_cancer_samples <- draw_sex_spec_cases(n_indv, sex_samples, "male", 0.08, sf_samples)

    asthma_samples <- draw_cases(n_indv, 0.16, 0.33, sf_samples)
    vte_samples <- draw_cases(n_indv, 0.4, 0, sf_samples)
    cov_samples <- draw_cases(n_indv, 0.34, 0.12, sf_samples)
    study_data <- tibble::tibble(
        ID=ids,
        SEX=sex_samples,
        DATE_OF_BIRTH=bd_samples,
        START_OF_FOLLOWUP=sf_samples,
        END_OF_FOLLOWUP=ef_samples,
        ANCESTRY=anc_samples,
        C3_BREAST=breast_cancer_samples$draws,
        C3_BREAST_DATE=breast_cancer_samples$dates,
        T2D=t2d_samples$draws,
        T2D_DATE=t2d_samples$dates,
        C3_PROSTATE=prost_cancer_samples$draws,
        C3_PROSTATE_DATE=prost_cancer_samples$dates,
        COVHOSP=cov_samples$draws,
        COVHOSP_DATE=cov_samples$dates,
        I9_VTE=vte_samples$draws,
        I9_VTE_DATE=vte_samples$dates,
        J10_ASTHMA=asthma_samples$draws,
        J10_ASTHMA_DATE=asthma_samples$dates,
        PC1=0.2,
        PC2=0.01,
        EDU=create_EDU_data(n_indv),
        COHORT="TODO",
        CHIP="TODO",
        BATCH="TODO",
        R3=0
    )

    study_data <- adjust_followup_time(study_data)
    return(study_data)
}

create_EDU_data <- function(n_indv) {
    EDU_samples <- sample(c(1,3,4, 5, 6, 7), n_indv, replace=TRUE)
    return(EDU_samples)
}

adjust_followup_time <- function(study_data) {
    date_cols = colnames(dplyr::select(study_data, 
                                       dplyr::matches("(*.)_DATE$")))
    followup_interval <- get_followup_time(study_data)
    for(date_col in date_cols) {
        not_nas = !is.na(study_data[[date_col]])
        selector = !(study_data[[date_col]] %within% followup_interval)
        selector[is.na(selector)] = FALSE
        study_data$END_OF_FOLLOWUP[selector] = NA
    }
    return(study_data)
}

add_case_dates <- function(all_samples, start_dates) {
    # Init
    date_samples <- as.Date(rep(NA, length(all_samples)))
    n_cases <- sum(all_samples == 1, na.rm=TRUE)

    # Sampling
    if(n_cases > 0) {
        # Careful NAs will be considered as 1s
        case_dates <- draw_case_dates(n_cases, start_dates[all_samples == 1 & !is.na(all_samples)])
        date_samples[all_samples == 1 & !is.na(all_samples)] <- case_dates
    }
    return(date_samples)
}

draw_sex_spec_cases <- function(n_indv, sex_samples, sex, case_prob, start_dates) {
    # Initializing
    all_samples <- rep(0, n_indv)
    n_sex_spec_indv <- sum(sex_samples == sex)

    # Drawing
    sex_spex_case_draws <- stats::rbinom(n_sex_spec_indv, 1, case_prob)
    all_samples[sex_samples == sex] <- sex_spex_case_draws
    date_samples <- add_case_dates(all_samples, start_dates)

    return(list(draws=all_samples, dates=date_samples))
}

draw_case_dates <- function(n_indv, start_dates=as.Date("1887/01/01")) {
    draw_dates_unif(n_indv, start_dates)
}

draw_cases <- function(n_indv, case_prob, na_prob, bd_samples) {
    # Initalizing
    all_samples <- rep(NA, n_indv)

    # Drawing
    na_draws <- stats::rbinom(n_indv, 1, na_prob)
    case_draws <- stats::rbinom(n_indv-sum(na_draws), 1, case_prob)
    all_samples[!na_draws] <- case_draws
    date_samples <- add_case_dates(all_samples, bd_samples)

    return(list(draws=all_samples, dates=date_samples))
}

draw_anc_samples <- function(n_indv) {
    bin_samples <- stats::rbinom(n_indv, 3, prob = 0.1)
    ifelse(bin_samples == 0, "EUR",
        ifelse(bin_samples == 1, "AMR",
        ifelse(bin_samples == 2, "EAS", "AFR")))
}

draw_dates_unif <- function(n_indv, start_dates=as.Date("1887/01/01")) {
    # Initializing
    end_date = lubridate::today()
    #min_start_date = min(start_dates)
    dates_seq = seq(min(start_dates), end_date, by="day")
    end_num = which(dates_seq == end_date)

    # Sampling
    if(length(start_dates) > 1) {
        # Have to sample each separately to be above the individuals
        # start date
        starts_num <- sapply(start_dates, function(start_date) which(dates_seq==start_date))
        num_samples <- sapply(starts_num, function(start_num) sample(start_num:end_num, 1))
    } else {
        start_num <- which(dates_seq == start_dates)
        num_samples <- sample(start_num:end_num, n_indv, replace=TRUE)
    }
    dates_seq[num_samples]
}

draw_bd_samples <- function(n_indv) {
    draw_dates_unif(n_indv)
}

draw_ef_samples <- function(n_indv, sf_samples) {
    draw_dates_unif(n_indv, sf_samples)
}

draw_sf_samples <- function(n_indv, bd_samples) {
    draw_dates_unif(n_indv, bd_samples)
}

draw_sex_samples <- function(n_indv) {
    bin_samples <- stats::rbinom(n_indv, 1, prob = 0.5)
    ifelse(bin_samples, "male", "female")
}

create_indv_ids <- function(n_indv) {
    id_nums <- seq(n_indv)
    paste0("KT00000", id_nums)
}

#' Gets the follow-up period
#' 
#' Gets the follow-up period for each individual as a lubridate 
#' \code{\link[lubridate]{interval}}. 
#' 
#' Currently, if there is NAs in the `END_OF_FOLLOWUP` column replaces 
#' it with the current date. This assumes that the missingness comes 
#' from the fact that the followup has not yet ended and is not dues 
#' some other reason.
#' 
#' @param study_data A data.frame with at least the columns:
#'                   `END_OF_FOLLOWUP` and `START_OF_FOLLOWUP`.
#' 
#' @importFrom lubridate %--%
#' 
#' @author Kira E. Detrois
get_followup_time <- function(study_data) {
    # Replacing NAs with current date
    followup_complete = study_data$END_OF_FOLLOWUP
    followup_complete[is.na(followup_complete)] = lubridate::today()

    # Creating intervals
    followup_time <- study_data$START_OF_FOLLOWUP %--% followup_complete

    return(followup_time)
}
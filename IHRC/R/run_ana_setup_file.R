#' Runs analysis defined in a setup file
#'
#' Reads in all the data, using function [IUtils::get_all_data] and then runs function
#' [IHRC::run_surv_studies].
#'
#' @param setup_file_path A string. The path to the setup file.
#'
#' @author Kira E. Detrois
#'
#' @export
run_ana_setup_file <- function(setup_file_path) {
    setup <- IUtils::get_setup(setup_file_path)
    data <- IUtils::get_all_data(
        score_type = setup$score_type,
        endpts = setup$endpts,
        pheno_file_path = setup$pheno_file_path,
        icd_file_path = setup$icd_file_path,
        atc_file_path = setup$atc_file_path,
        prs_dir_path = setup$prs_dir_path,
        phers_dir_path = setup$phers_dir_path,
        phers_transfer_dir_path = setup$phers_transfer_dir_path,
        phers_study_descr = IUtils::get_phers_file_descr(
            study_type = setup$study_type,
            obs_end_date = setup$obs_end_date,
            exp_len = setup$exp_len,
            wash_len = setup$wash_len,            obs_len = setup$obs_len
        ),
        zip_dir_path = setup$zip_dir_path,
        prs_file_end = setup$prs_file_end,
        prs_id_col_name = setup$prs_id_col_name,
        prs_score_col_name = setup$prs_score_col_name,
        tuomo_file_append = setup$tuomo_file_append,
        exp_len_transfer = setup$exp_len_transfer
    )
    IHRC::run_surv_studies(
        pheno_data = data$pheno,
        endpts_indvs_mat = IUtils::get_endpts_indvs_mat(setup, data$pheno),
        icd_data = data$icd,
        atc_data = data$atc,
        prs_data = data$prs,
        phers_data = data$phers,
        zip_data = data$zip,
        score_type = setup$score_type,
        create_score_combos = setup$create_score_combos,
        bunch_phenos = setup$bunch_phenos,
        study_type = setup$study_type,
        endpts = setup$endpts,
        exp_len = setup$exp_len,
        wash_len = setup$wash_len,
        obs_len = setup$obs_len,
        obs_end_date = setup$obs_end_date,
        down_fctr = setup$down_fctr,
        ancs = setup$ancs,
        obs_age_range = setup$obs_age_range,
        covs = setup$covs,
        filter_1998 = setup$filter_1998,
        min_indvs = setup$min_indvs,
        write_res = TRUE,
        res_dir = setup$res_dir,
        res_descr = setup$res_descr,
        write_progress = setup$write_progress,
        is_first_endpt=setup$is_first_endpt
    )
}

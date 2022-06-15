create_surv_res_dt <- function(coxph_res,
                                endpt,
                                score_type,
                                N_controls,
                                N_cases) {

    coxph_res <- extract_surv_res(coxph_res)
    print(coxph_res)
}
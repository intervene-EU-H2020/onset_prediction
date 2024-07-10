
#' writing zph results of coxph model to log
#' 
#' @param coxph_mdl The coxph model
#' @param study The study object
#' @param surv_ana The survival analysis object
write_coxph_to_log <- function(coxph_mdl,
                              study,
                              surv_ana) { 
    if(!is.null(coxph_mdl)) {
        file_path <- get_full_file_name_path(res_type="log",
                                            study_setup=study@study_setup,
                                            endpt=study@endpt,
                                            surv_ana=surv_ana)
        readr::write_file(paste0("\nSummary cox-PH model:\n\nNo of cases:", summary(coxph_mdl)$nevent, "\nNo of ctrls:", summary(coxph_mdl)$n-summary(coxph_mdl)$nevent, "\n\nzph original model:\n\n"),
                        file_path)
        zph_res <-  tryCatch({
                            tibble::as_tibble(survival::cox.zph(coxph_mdl)$table, rownames="PRED")
                        }, error = function(e) {
                            return("Couldn't calculate zph")
                        })
        if(is.data.frame(zph_res)) {
            readr::write_delim(zph_res,
                               file=file_path,
                               append=TRUE,
                               col_names=TRUE)
        } else {
            readr::write_file(zph_res,
                              file=file_path,
                              append=TRUE)
        }
    }
}
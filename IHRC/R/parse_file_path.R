parse_file_path <- function(file_path) {
    ana_details <- list()

    file_path_parts <- unlist(stringr::str_split(file_path, "/"))
    res_dir_idx <- seq_len(length(file_path_parts))[stringr::str_detect(file_path_parts, "results")]
    ana_details$res_dir <- paste0(paste0(file_path_parts[1:res_dir_idx], collapse="/"), "/")

    file_path_parts <- file_path_parts[(res_dir_idx+1):length(file_path_parts)]

    ana_details$down_fctr <- ifelse(file_path_parts[1] == "no_down",  
                                    NA_integer_,  
                                    as.numeric(stringr::str_extract(file_path_parts[1], "[0-9]")))
    ana_details$filter_1998 = ifelse(file_path_parts[2] == "f1998", TRUE, FALSE)
    ana_details$study_type = file_path_parts[4]

    file_name = file_path_parts[5]
    file_name_parts <- unlist(stringr::str_split(file_name, "_"))
    if(ana_details$study_type == "backward") {
        ana_details$obs_end_date <- as.Date(file_name_parts[1])
        ana_details$endpt <- "None"
        file_name_parts <- file_name_parts[-1]

    } else {
        ana_details$obs_end_date <- as.Date("2021/01/01")
        ana_details$endpt <- paste0(file_name_parts[1], "_", file_name_parts[2])
        file_name_parts <- file_name_parts[-1]
        file_name_parts <- file_name_parts[-1]
    }


    exp_bool <- stringr::str_detect(file_name_parts, "e[0-9]+")
    ana_details$exp_len <- as.numeric(stringr::str_extract(file_name_parts[exp_bool], "[0-9]+"))
    file_name_parts <- file_name_parts[!exp_bool]

    wash_bool <- stringr::str_detect(file_name_parts, "w[0-9]+")
    ana_details$wash_len <- as.numeric(stringr::str_extract(file_name_parts[wash_bool], "[0-9]+"))
    file_name_parts <- file_name_parts[!wash_bool]

    obs_bool <- stringr::str_detect(file_name_parts, "o[0-9]+")
    ana_details$obs_len <- as.numeric(stringr::str_extract(file_name_parts[obs_bool], "[0-9]+"))
    file_name_parts <- file_name_parts[!obs_bool]

    preds <- file_name_parts[!stringr::str_detect(file_name_parts, "coxph")]

    ana_details$preds <- back_parse_preds(preds)
    ana_details$res_dir <- get_full_res_path(ana_details$res_dir, ana_details$down_fctr, ana_details$filter_1998)
    return(ana_details)
}

back_parse_preds <- function(preds) {
    preds <- stringr::str_replace_all(preds,  "YOB", "YEAR_OF_BIRTH")
    preds <- stringr::str_replace_all(preds, "i", "*")
    if(any(stringr::str_detect(preds, "PCs"))) {
        preds <- preds[!stringr::str_detect(preds, "PCs")]
        preds <- c(preds,  paste0("PC", seq_len(10)))
    }
    return(preds)
}
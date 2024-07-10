

#' Scales the given variables in the score data
#'
#' @param preds A string vector of the variables to be scaled
#' @param study_data A data frame with the variables to be scaled
#'
#' @return A data frame with the scaled variables
#'
#' @export
scale_preds <- function(preds,
                        study_data) {
    study_data$ID <- as.character(study_data$ID)

    for(pred in preds) {
        if(pred %in% colnames(study_data)) {
            if(pred %in% c("YEAR_OF_BIRTH", "BMI") | stringr::str_detect(pred, "PC")) {
                study_data[,pred] <- scale(study_data[,pred])[,1]
            } 
            # if(pred %in% c("EDU", "ZIP", "SMOKING", "EDUCATION_11", "EDUCATION_97", "EDU_UKBB")) {
            #     study_data[,pred] <- as.factor(dplyr::pull(study_data, pred))
            # }
            # if(pred %in% c("EDUCATION_11")) {
            #     study_data <- dplyr::mutate(study_data, EDUCATION_11=case_when(EDUCATION_11 <= 1 ~ "0", EDUCATION_11 > 1 & EDUCATION_11 < 5 ~ "1", EDUCATION_11 >= 5 ~ "2"))
            #     study_data[,pred] <- factor(as.character(dplyr::pull(study_data, pred)), levels=c("1", "0", "2"))
            # }

            if(pred %in% c("EDU","EDUCATION_97")) {
                study_data[,pred] <- factor(as.character(ifelse(study_data[,pred] == "Advanced", "1", "0")), levels=c("1", "0"))
            }

            if(stringr::str_detect(pred, "_group")) {
                if(pred %in% c("PheRS_group", "PRS_group"))
                    study_data[,pred] <- factor(as.character(dplyr::pull(study_data, pred)), levels=c("4","1","2","3","5","6","7"))
                if(pred == "CCI_group")
                    study_data[,pred] <- factor(as.character(dplyr::pull(study_data, pred)), levels=c("0", "1"))
                if(pred == "EDU_cont_group")
                    study_data[,pred] <- factor(as.character(dplyr::pull(study_data, pred)), levels=c("1", "0"))
            }
        }
    }

    if(class(study_data$SEX) == "character") {
        study_data$SEX <- factor(study_data$SEX, levels=c("female", "male"))
    } else {
        study_data$SEX <- factor(study_data$SEX, levels=c(0, 1))
    }
    return(study_data)
}


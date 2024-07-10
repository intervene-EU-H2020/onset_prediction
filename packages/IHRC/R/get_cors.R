
write_cor_data <- function(cor_data, study_setup, surv_ana) {
    res_path <- get_full_file_name_path(res_type="cor",
                            study_setup=study_setup,
                            surv_ana=surv_ana)
    readr::write_delim(cor_data, res_path, delim="\t")
}


#' Very inelegant way to get the pearsons' correlation between variables depending on modles run
get_cor_data <- function(pheno_score_data,
                               score_type,
                               endpt,
                               cor_data) {
    pheno_score_data$YEAR_OF_BIRTH <- lubridate::year(pheno_score_data$DATE_OF_BIRTH)
    pheno_score_data$SEX <- ifelse(pheno_score_data$SEX == "male", 0, 1)
    test_data <- dplyr::filter(pheno_score_data, TRAIN_STATUS==0)

    if("PheRS" %in% score_type & "PRS" %in% score_type) {
        cor <- cor.test(test_data$PheRS, test_data$PRS)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="Test",
                                                VAR_1="PheRS", 
                                                VAR_2="PRS", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$PheRS, pheno_score_data$PRS)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PheRS", 
                                                VAR_2="PRS", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }
    if("PheRS" %in% score_type) {
        cor <- cor.test(pheno_score_data$PheRS_orig, pheno_score_data$PheRS)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PheRS_orig", 
                                                VAR_2="PheRS", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$PheRS_orig, pheno_score_data$YEAR_OF_BIRTH)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PheRS_orig", 
                                                VAR_2="Age", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$PheRS, pheno_score_data$YEAR_OF_BIRTH)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PheRS", 
                                                VAR_2="Age", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$PheRS_orig, pheno_score_data$SEX)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PheRS_orig", 
                                                VAR_2="SEX", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$PheRS, pheno_score_data$SEX)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PheRS", 
                                                VAR_2="SEX", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }


    if("CCI" %in% score_type & "PheRS" %in% score_type) {
        cor <- cor.test(pheno_score_data$PheRS, as.numeric(pheno_score_data$CCI_group))
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PheRS", 
                                                VAR_2="CCI_group", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(test_data$PheRS, as.numeric(test_data$CCI_group))
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="Test",
                                                VAR_1="PheRS", 
                                                VAR_2="CCI_group", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }


    if("CCI" %in% score_type) {
        cor <- cor.test(pheno_score_data$CCI, pheno_score_data$YEAR_OF_BIRTH)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="CCI", 
                                                VAR_2="Age", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$CCI_orig, pheno_score_data$YEAR_OF_BIRTH)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="CCI_orig", 
                                                VAR_2="Age", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$CCI, pheno_score_data$SEX)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="CCI", 
                                                VAR_2="SEX", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$CCI_orig, pheno_score_data$SEX)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="CCI_orig", 
                                                VAR_2="SEX", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }


    if("EDU" %in% score_type) {
        pheno_score_data <- dplyr::mutate(pheno_score_data, EDU=case_when(EDU == "Basic" ~ 1, EDU == "Intermediate" ~ 1, EDU == "Advanced" ~ 2))
        cor <- cor.test(pheno_score_data$EDU, pheno_score_data$YEAR_OF_BIRTH)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="EDU", 
                                                VAR_2="Age", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
        cor <- cor.test(pheno_score_data$EDU, pheno_score_data$SEX)

        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="EDU", 
                                                VAR_2="SEX", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }
    if("EDU_cont" %in% score_type) {
        cor <- cor.test(pheno_score_data$EDU_cont, pheno_score_data$YEAR_OF_BIRTH)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="EDU_cont", 
                                                VAR_2="Age", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$EDU_cont, pheno_score_data$SEX)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="EDU_cont", 
                                                VAR_2="SEX", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(as.numeric(pheno_score_data$EDU_cont_group), pheno_score_data$YEAR_OF_BIRTH)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="EDU_cont_group", 
                                                VAR_2="Age", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))


        cor <- cor.test(as.numeric(pheno_score_data$EDU_cont_group), pheno_score_data$SEX)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="EDU_cont_group", 
                                                VAR_2="SEX", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }

    if("PheRS" %in% score_type & "PheRS_transfer" %in% score_type) {
        cor <- cor.test(pheno_score_data$PheRS, pheno_score_data$PheRS_transfer)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PheRS", 
                                                VAR_2="PheRS_transfer", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(test_data$PheRS, test_data$PheRS_transfer)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="Test",
                                                VAR_1="PheRS", 
                                                VAR_2="PheRS_transfer", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }

    if("PRS" %in% score_type) {
        cor <- cor.test(pheno_score_data$PRS_orig, pheno_score_data$PRS)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PRS_orig", 
                                                VAR_2="PRS", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
        cor <- cor.test(pheno_score_data$PRS, pheno_score_data$YEAR_OF_BIRTH)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PRS", 
                                                VAR_2="Age", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$PRS_orig, pheno_score_data$YEAR_OF_BIRTH)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PRS_orig", 
                                                VAR_2="Age", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
        cor <- cor.test(pheno_score_data$PRS, pheno_score_data$SEX)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PRS", 
                                                VAR_2="SEX", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$PRS_orig, pheno_score_data$SEX)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PRS_orig", 
                                                VAR_2="SEX", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
        cor <- cor.test(pheno_score_data$PRS, pheno_score_data$PC1)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PRS", 
                                                VAR_2="PC1", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))

        cor <- cor.test(pheno_score_data$PRS_orig, pheno_score_data$PC1)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PRS_orig", 
                                                VAR_2="PC1", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }

    if("CCI" %in% score_type & "PheRS" %in% score_type) {
        cor <- cor.test(pheno_score_data$PheRS, pheno_score_data$CCI)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PheRS", 
                                                VAR_2="CCI", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }


    if("EDU_cont" %in% score_type & "PheRS" %in% score_type) {
        cor <- cor.test(pheno_score_data$PheRS, pheno_score_data$EDU_cont)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PheRS", 
                                                VAR_2="Education", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }

    if("CCI" %in% score_type & "PRS" %in% score_type) {
        cor <- cor.test(pheno_score_data$PRS, pheno_score_data$CCI)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PRS", 
                                                VAR_2="CCI", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }


    if("EDU_cont" %in% score_type & "PRS" %in% score_type) {
        cor <- cor.test(pheno_score_data$PRS, pheno_score_data$EDU_cont)
        cor_data <- rbind(cor_data, tibble::tibble(ENDPOINT=endpt, 
                                                SET="All",
                                                VAR_1="PRS", 
                                                VAR_2="Education", 
                                                COR=cor$estimate, 
                                                CIneg=cor$conf.int[1], 
                                                CIpos=cor$conf.int[2], 
                                                P_value=cor$p.value))
    }

    return(cor_data)
}


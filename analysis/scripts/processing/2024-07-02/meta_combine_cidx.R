source("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/scripts/utils.R")


#############################################
############ HAZARD RATIOS ##################
#############################################

############ PCs ##################
###################################
############ DATA ##################
# FinnGen
all_fin <- readr::read_delim("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/fg/2019-01-01_o8_w2_e10_32_70_cidx.tsv")
all_est <- readr::read_delim("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/estb/2019-01-01_o8_w2_e6_32_70_cidx.tsv")
all_uk <- readr::read_delim("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-07-02/bbs/ukb/2019-01-01_o8_w2_e10_32_70_cidx.tsv")

all_cidx <- rbind(all_fin, all_est, all_uk)
#all_cidx <- rbind(all_fin,  all_uk)

base <- "Surv~Sex+Age"
sex <- "Surv~Sex"
age <- "Surv~Age"
prs <- "Surv~PRS"
prs_group <- "Surv~PRS_group"
prs_pcs <- "Surv~PRS+PCs"
prs_base <- "Surv~PRS+Sex+Age"
phers <- "Surv~PheRS"
phers_group <- "Surv~PheRS_group"
phers_base <- "Surv~PheRS+Sex+Age"
phers_prs <- "Surv~PheRS+PRS"
phers_prs_base <- "Surv~PheRS+PRS+Sex+Age"
pheno <- "Surv~CCI_group+Edu+PheRS+Sex+Age"
cci_base <- "Surv~CCI+Sex+Age" 
cci <- "Surv~CCI" 
cci_group_base <- "Surv~CCI_group+Sex+Age" 
cci_group <- "Surv~CCI_group" 
cci_edu <- "Surv~CCI_group+Edu+Sex+Age"
edu_base <- "Surv~Edu+Sex+Age" 
edu <- "Surv~Edu" 
full_base <- "Surv~CCI_group+Edu+PheRS+PRS+Sex+Age"
full <- "Surv~CCI_group+Edu+PheRS+PRS+Sex+Age"

##### PRS
meta <- meta_cidx(all_cidx, prs) 

crnt_meta <- meta_cidx(all_cidx, prs_base)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, prs_group)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, sex)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, phers_prs_base)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, phers_prs)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, age)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, base)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, phers)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, phers_base)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, phers_group)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, cci)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, edu)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, cci_group)
meta <- bind_rows(meta, crnt_meta)
crnt_meta <- meta_cidx(all_cidx, cci_edu)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, full)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, cci_group_base)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, pheno)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, full_base)
meta <- bind_rows(meta, crnt_meta)

##### Bind all
all_cidx$N_BBs <- 1
meta_combo <- bind_rows(meta, all_cidx) %>% dplyr::distinct() %>% dplyr::arrange(Endpoint, SURV_MODEL, Biobank) # no duplicates at the moment 2024-05-30
meta_combo <- dplyr::select(meta_combo, Endpoint, SURV_MODEL, Biobank, N_BBs, N_CASES, N_CONTROLS, C_IDX, SE, P_VAL, C_IDX_CI_NEG, C_IDX_CI_POS, I2, I2_CIneg, I2_CIpos, QHet, QHet_CIneg, QHet_CIpos, Q_pval) %>%
                        dplyr::arrange(SURV_MODEL, Endpoint, Biobank)


readr::write_delim(meta_combo, "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-07-02/bbs/meta/2019-01-01_o8_w2_10_32_70_cidx.tsv", delim="\t")

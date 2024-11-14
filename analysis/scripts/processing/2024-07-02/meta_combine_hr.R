library(dplyr)
source("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/scripts/utils.R")


#############################################
############ HAZARD RATIOS ##################
#############################################

############ PCs ##################
###################################
############ DATA ##################
# FinnGen
all_fin <- readr::read_delim("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/fg/2019-01-01_o8_w2_e10_32_70_coxph.tsv")
all_est <- readr::read_delim("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/estb/2019-01-01_o8_w2_e6_32_70_coxph.tsv")
all_uk <- readr::read_delim("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-07-02/bbs/ukb/2019-01-01_o8_w2_e10_32_70_coxph.tsv")

all_hrs <- rbind(all_fin, all_est, all_uk)
#all_hrs <- rbind(all_fin,  all_uk)

############ Meta-analysis ##################
model_to_file_name <- function(name) stringr::str_remove_all(stringr::str_replace_all(name, "\\+", "-"), "Surv~")
model_count_sum <- function(data, model) return(data %>% filter(SURV_MODEL == model) %>% group_by(Endpoint) %>% summarise(N=length(unique(Biobank)), BBs=paste0(unique(Biobank), collapse=", ")))
##### Models
res_dir <- "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-07-02/bbs/meta/"
model_dir <- paste0(res_dir, "models/")

base <- "Surv~Sex+Age"
model_count_sum(all_hrs, base)
readr::write_delim(model_count_sum(all_hrs, base), file=paste0(model_dir, model_to_file_name(base), ".tsv"), delim="\t")

prs <- "Surv~PRS"
model_count_sum(all_hrs, prs)
readr::write_delim(model_count_sum(all_hrs, prs), file=paste0(model_dir, model_to_file_name(prs), ".tsv"), delim="\t")

prs_group <- "Surv~PRS_group"
model_count_sum(all_hrs, prs_group)
readr::write_delim(model_count_sum(all_hrs, prs_group), file=paste0(model_dir, model_to_file_name(prs_group), ".tsv"), delim="\t")

prs_pcs <- "Surv~PRS+PCs"
model_count_sum(all_hrs, prs_pcs)
readr::write_delim(model_count_sum(all_hrs, prs_pcs), file=paste0(model_dir, model_to_file_name(prs_pcs), ".tsv"), delim="\t")

phers <- "Surv~PheRS"
model_count_sum(all_hrs, phers)
readr::write_delim(model_count_sum(all_hrs, phers), file=paste0(model_dir, model_to_file_name(phers), ".tsv"), delim="\t")

phers_group <- "Surv~PheRS_group"
model_count_sum(all_hrs, phers_group)
readr::write_delim(model_count_sum(all_hrs, phers_group), file=paste0(model_dir, model_to_file_name(phers_group), ".tsv"), delim="\t")

phers_base <- "Surv~PheRS+Sex+Age"
model_count_sum(all_hrs, phers_base)
readr::write_delim(model_count_sum(all_hrs, phers_base), file=paste0(model_dir, model_to_file_name(phers_base), ".tsv"), delim="\t")

phers_prs <- "Surv~PheRS+PRS"
model_count_sum(all_hrs, phers_prs)
readr::write_delim(model_count_sum(all_hrs, phers_prs), file=paste0(model_dir, model_to_file_name(phers_prs), ".tsv"), delim="\t")

pheno <- "Surv~CCI_group+Edu+PheRS+Sex+Age"
model_count_sum(all_hrs, pheno)
readr::write_delim(model_count_sum(all_hrs, pheno), file=paste0(model_dir, model_to_file_name(pheno), ".tsv"), delim="\t")

cci_base <- "Surv~CCI+Sex+Age" 
model_count_sum(all_hrs, cci_base)
readr::write_delim(model_count_sum(all_hrs, cci_base), file=paste0(model_dir, model_to_file_name(cci_base), ".tsv"), delim="\t")

cci <- "Surv~CCI" 
model_count_sum(all_hrs, cci)
readr::write_delim(model_count_sum(all_hrs, cci), file=paste0(model_dir, model_to_file_name(cci), ".tsv"), delim="\t")

cci_group_base <- "Surv~CCI_group+Sex+Age" 
model_count_sum(all_hrs, cci_group_base)
readr::write_delim(model_count_sum(all_hrs, cci_group_base), file=paste0(model_dir, model_to_file_name(cci_group_base), ".tsv"), delim="\t")

cci_group <- "Surv~CCI_group" 
model_count_sum(all_hrs, cci_group)
readr::write_delim(model_count_sum(all_hrs, cci_group), file=paste0(model_dir, model_to_file_name(cci_group), ".tsv"), delim="\t")

cci_edu <- "Surv~CCI_group+Edu+Sex+Age"
model_count_sum(all_hrs, cci_edu)
readr::write_delim(model_count_sum(all_hrs, cci_edu), file=paste0(model_dir, model_to_file_name(cci_edu), ".tsv"), delim="\t")

edu_base <- "Surv~Edu+Sex+Age" 
model_count_sum(all_hrs, edu_base)
readr::write_delim(model_count_sum(all_hrs, edu_base), file=paste0(model_dir, model_to_file_name(edu_base), ".tsv"), delim="\t")

edu <- "Surv~Edu" 
model_count_sum(all_hrs, edu)
readr::write_delim(model_count_sum(all_hrs, edu), file=paste0(model_dir, model_to_file_name(edu), ".tsv"), delim="\t")

# #FUTURE
# pheno <- "Surv~CCI+Edu+PheRS+Sex+Age"
# model_count_sum(all_hrs, pheno)
# readr::write_delim(model_count_sum(all_hrs, pheno), file=paste0(model_dir, model_to_file_name(pheno), ".tsv"), delim="\t")


##### PRS
meta <- meta_hrs(all_hrs, "PRS", prs) 

crnt_meta <- meta_hrs(all_hrs, "PRS", prs_pcs)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PRS", prs_group, group = 7)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PRS", prs_group, group = 1)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PRS", phers_prs)
meta <- bind_rows(meta, crnt_meta)

##### PheRS
crnt_meta <- meta_hrs(all_hrs, "PheRS", phers)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PheRS", phers_base)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PheRS", phers_group, group = 7)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PheRS", phers_group, group = 1)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PheRS", phers_prs)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PheRS", pheno)
meta <- bind_rows(meta, crnt_meta)

##### CCI
crnt_meta <- meta_hrs(all_hrs, "CCI", cci)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "CCI", cci_base)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "CCI", cci_group, group=1)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "CCI", cci_group_base, group=1)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "CCI", pheno, group=1)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "CCI", cci_edu, group=1)
meta <- bind_rows(meta, crnt_meta)

##### Education
crnt_meta <- meta_hrs(all_hrs, "Edu", edu_base, group = 0)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "Edu", edu, group = 0)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "Edu", pheno, group = 0)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "Edu", cci_edu, group=0)
meta <- bind_rows(meta, crnt_meta)

### Age
crnt_meta <- meta_hrs(all_hrs, "YEAR_OF_BIRTH", base)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "YEAR_OF_BIRTH", "Surv~Age")
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "YEAR_OF_BIRTH", phers_base)
meta <- bind_rows(meta, crnt_meta)

### Sex
crnt_meta <- meta_hrs(all_hrs, "Sex", base, group=0)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "Sex", "Surv~Sex", group=0)
meta <- bind_rows(meta, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "Sex", phers_base, group=0)
meta <- bind_rows(meta, crnt_meta)

#############################################
####### Log
log_file <- paste0(res_dir, "README.txt")
log_file
message <- paste0("All data from the different cohorts, as well as meta-analysis for the HRs.", "\n",
                  "FinnGen: see bbs/fg/README.txt\nUKB: see /bbs/ukb/README.txt\nEstB: see bbs/estb/README.txt\n", 
                  "\n\n###### Overall Endpoints ######\n\n", paste(unique(meta$Endpoint), collapse = "\n"),
                  "\n\n###### Meta-analyzed Vars - Models ######\n\n", meta %>% select(VAR, SURV_MODEL) %>% distinct() %>% dplyr::arrange(VAR, SURV_MODEL) %>% mutate(COMBO=paste0(VAR, " - ", SURV_MODEL)) %>% pull(COMBO) %>% paste0(collapse="\n"))
message
readr::write_file(message, log_file)

##### Bind all
#meta <- dplyr::mutate(meta, HR=ifelse(VAR %in% c("EDU_cont"), 1+(1-HR), HR))
#meta <- dplyr::mutate(meta, CI_NEG=ifelse(VAR %in% c("EDU_cont"), 1+(1-CI_NEG), CI_NEG))
#meta <- dplyr::mutate(meta, CI_POS=ifelse(VAR %in% c("EDU_cont"), 1+(1-CI_POS), CI_POS))
all_hrs$N_BBs <- 1
meta_combo <- bind_rows(meta, all_hrs) %>% dplyr::distinct() %>% dplyr::arrange(Endpoint, SURV_MODEL, Biobank) # no duplicates at the moment 2024-05-30
meta_combo <- dplyr::select(meta_combo, Endpoint, SURV_MODEL, Biobank, N_BBs, N_CASES, N_CONTROLS, VAR, GROUP, BETA, BETA_SE, P_VAL, HR, CI_NEG, CI_POS, I2, I2_CIneg, I2_CIpos, QHet, QHet_CIneg, QHet_CIpos, Q_pval) %>%
                        dplyr::arrange(SURV_MODEL, VAR, Endpoint, Biobank)

meta_combo <- dplyr::mutate(meta_combo,
                                HR=ifelse(VAR == "YEAR_OF_BIRTH", 1+(1-HR), HR),
                                CI_NEG=ifelse(VAR == "YEAR_OF_BIRTH", 1+(1-CI_NEG), CI_NEG),
                                CI_POS=ifelse(VAR == "YEAR_OF_BIRTH", 1+(1-CI_POS), CI_POS))

readr::write_delim(meta_combo, "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-07-02/bbs/meta/2019-01-01_o8_w2_10_32_70_coxph.tsv", delim="\t")

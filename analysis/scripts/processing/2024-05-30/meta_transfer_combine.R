
source("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/scripts/utils.R")

############ Transfer ###################
#########################################

############ DATA ##################
all_hrs <- readr::read_delim("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/fg/transfer/2019-01-01_o8_w2_e10_32_70_coxph.tsv")
# all_fin_uk$SURV_MODEL <- stringr::str_remove_all(all_fin_uk$SURV_MODEL, "\\+PCs")
# ##### Models
phers_both_transfer <- "Surv~PheRS+PheRS_transfer"
phers_both_transfer_base <- "Surv~PheRS+PheRS_transfer+Sex+Age"

phers_transfer_base <- "Surv~PheRS_transfer+Sex+Age" 
phers_transfer <- "Surv~PheRS_transfer"

meta_transfer <- meta_hrs(all_hrs, "PheRS_transfer", phers_both_transfer, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")

crnt_meta <- meta_hrs(all_hrs, "PheRS_transfer", phers_both_transfer_base, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")
meta_transfer <- bind_rows(meta_transfer, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PheRS_transfer", phers_transfer_base, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")
meta_transfer <- bind_rows(meta_transfer, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PheRS_transfer", phers_transfer_base, group=7, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")
meta_transfer <- bind_rows(meta_transfer, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PheRS_transfer", phers_transfer_base, group=1, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")
meta_transfer <- bind_rows(meta_transfer, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PheRS_transfer", phers_transfer, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")
meta_transfer <- bind_rows(meta_transfer, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PheRS_transfer", phers_transfer, group=7, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")
meta_transfer <- bind_rows(meta_transfer, crnt_meta)

crnt_meta <- meta_hrs(all_hrs, "PheRS_transfer", phers_transfer, group=1, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")
meta_transfer <- bind_rows(meta_transfer, crnt_meta)

readr::write_delim(meta_transfer, "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/fg_transfer/meta_2019-01-01_o8_w2_10_32_70_coxph.tsv", delim="\t")

####### Log
log_file <- paste0("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/fg_transfer/meta_README.txt")
log_file
message <- paste0("Results in FinnGen with models trained in UKB (UKB->FinnGen) and EstB (EstB->FinnGen) meta-analyzed using fixed effect model.\n\n",
                    "\n\n###### Overall Endpoints ######\n\n", paste(unique(meta_transfer$Endpoint), collapse = "\n"),
                    "\n\n###### Variables ######\n\n", paste(unique(meta_transfer$VAR), collapse = "\n"),
                    "\n\n###### Models ######\n\n", paste(unique(meta_transfer$SURV_MODEL), collapse = "\n"))
message
readr::write_file(message, log_file)

############ Transfer ###################
#########################################

############ DATA ##################
source("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/scripts/utils.R")

all_cidx <- readr::read_delim("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/fg/transfer/2019-01-01_o8_w2_e10_32_70_cidx.tsv")

meta_transfer <- meta_cidx(all_cidx, phers_both_transfer, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")

crnt_meta <- meta_cidx(all_cidx, phers_both_transfer_base, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")
meta_transfer <- bind_rows(meta_transfer, crnt_meta)

crnt_meta <- meta_cidx(all_cidx, phers_transfer_base, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")
meta_transfer <- bind_rows(meta_transfer, crnt_meta)

crnt_meta <- meta_cidx(all_cidx,phers_transfer, bbs=c("UKB->FinnGen", "EstB->FinnGen"), crnt_method="FE")
meta_transfer <- bind_rows(meta_transfer, crnt_meta)

readr::write_delim(meta_transfer, "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/fg_transfer/meta_2019-01-01_o8_w2_10_32_70_cidx.tsv", delim="\t")
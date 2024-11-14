library(dplyr)
source("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/scripts/utils.R")

#### PATHS

pheno_dir_uk <- "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/orig/finngen/fg_down_2024-05-29/ukb/"
pheno_dir_est <- "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/orig/finngen/fg_down_2024-05-29/estb/"

res_dir <- "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/fg/transfer/"

file_names <- "2019-01-01_o8_w2_e10_32_70"
file_append <- paste0(file_names, "_coxph.tsv")

bb_uk <- "UKB->FinnGen"
bb_est <- "EstB->FinnGen"

####### DATA

#### Phenotype data
pheno_file <- paste0(pheno_dir_uk, file_append)
hrs_data_uk <- get_data(pheno_file, bb_uk)
hrs_data_uk
print(unique(hrs_data_uk$SURV_MODEL))

#### EstBB
pheno_file <- paste0(pheno_dir_est, file_append)
hrs_data_est <- get_data(pheno_file, bb_est)
hrs_data_est
print(unique(hrs_data_est$SURV_MODEL))

hrs_data <- rbind(hrs_data_uk, hrs_data_est)
hrs_data <- dplyr::rename(hrs_data, BETA_SE=SE)


##### Change variables
hrs_data$GROUP <- stringr::str_split(hrs_data$VAR, "_group", simplify = TRUE)[,2]
hrs_data$GROUP[hrs_data$VAR == "EDU0"] <- 0
hrs_data$VAR[hrs_data$VAR == "EDU0"] <- "Edu"
hrs_data$GROUP[hrs_data$VAR == "SEXmale"] <- 0
hrs_data$VAR[hrs_data$VAR == "SEXmale"] <- "Sex"

hrs_data$GROUP <- as.numeric(ifelse(hrs_data$GROUP != "", hrs_data$GROUP, -1))
hrs_data$GROUP
hrs_data$VAR <- stringr::str_split(hrs_data$VAR, "_group", simplify = TRUE)[,1]

res_file <- paste0(res_dir, file_append)
res_file
readr::write_delim(hrs_data, res_file, delim = "\t")

#### C-index
file_append <- paste0(file_names, "_cidx.tsv")

####### DATA

#### Phenotype data
pheno_file <- paste0(pheno_dir_uk, file_append)
cidx_data_uk <- get_data(pheno_file, bb_uk)
cidx_data_uk
print(unique(cidx_data_uk$SURV_MODEL))
cidx_data_uk <- cidx_data_uk %>% dplyr::distinct() %>% dplyr::arrange(Endpoint, SURV_MODEL, Biobank)
cidx_data_uk

## EstBB

####### DATA

#### Phenotype data
pheno_file <- paste0(pheno_dir_est, file_append)
cidx_data_est <- get_data(pheno_file, bb_est)
cidx_data_est
print(unique(cidx_data_est$SURV_MODEL))
cidx_data_est <- cidx_data_est %>% dplyr::distinct() %>% dplyr::arrange(Endpoint, SURV_MODEL, Biobank)
cidx_data_est 

cidx_data <- rbind(cidx_data_uk, cidx_data_est)

#### Write
res_file <- paste0(res_dir, file_append)
res_file
readr::write_delim(cidx_data, res_file, delim = "\t")

####### Log
log_file <- paste0(res_dir, "README.txt")
log_file
message <- paste0("The files are a combination of the files from\n(UKBB) ", pheno_dir_uk, "\n(UKBB) ", pheno_dir_est, "\n(EstBB) ",
                    "\n\n###### Overall Endpoints ######\n\n", paste(unique(hrs_data$Endpoint), collapse = "\n"),
                    "\n\n###### Variables ######\n\n", paste(unique(hrs_data$VAR), collapse = "\n"),
                    "\n\n###### Models ######\n\n", paste(unique(cidx_data$SURV_MODEL), collapse = "\n"))
message
readr::write_file(message, log_file)

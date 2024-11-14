library(dplyr)
source("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/scripts/utils.R")

#### PATHS

dir <- "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/orig/finngen/fg_down_2024-05-29/fg/PheRS_PRS/"
old_dir <- "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/orig/finngen/fg_down_2024-05-29/fg/EDU_CCI/"

res_dir <- "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-05-30/bbs/fg/"

file_names <- "2019-01-01_o8_w2_e10_32_70"
file_append <- paste0(file_names, "_coxph.tsv")
bb <- "FinnGen"

####### DATA
#### Phenotype data
pheno_file <- paste0(dir, file_append)
hrs_data <- get_data(pheno_file, bb)
hrs_data
print(unique(hrs_data$SURV_MODEL))

old_pheno_file <- paste0(old_dir, file_append)
old_data <- get_data(old_pheno_file, bb)
print(unique(old_data$SURV_MODEL))

hrs_data <- dplyr::bind_rows(hrs_data, old_data)
print(unique(hrs_data$SURV_MODEL))

##### Change variables
hrs_data$GROUP <- stringr::str_split(hrs_data$VAR, "_group", simplify = TRUE)[,2]
hrs_data$GROUP[hrs_data$VAR == "EDU0"] <- 0
hrs_data$VAR[hrs_data$VAR == "EDU0"] <- "Edu"
hrs_data$GROUP[hrs_data$VAR == "SEXmale"] <- 0
hrs_data$VAR[hrs_data$VAR == "SEXmale"] <- "Sex"

hrs_data$GROUP <- as.numeric(ifelse(hrs_data$GROUP != "", hrs_data$GROUP, -1))
hrs_data$GROUP
hrs_data$VAR <- stringr::str_split(hrs_data$VAR, "_group", simplify = TRUE)[,1]

hrs_data <- hrs_data %>% dplyr::distinct() %>% dplyr::arrange(Endpoint, SURV_MODEL, Biobank)

hrs_data <- dplyr::rename(hrs_data, BETA_SE=SE)
hrs_data

res_file <- paste0(res_dir, file_append)
res_file
readr::write_delim(hrs_data, res_file, delim = "\t")

#### C-index
file_append <- paste0(file_names, "_cidx.tsv")
####### DATA

####### Phenotype data
pheno_file <- paste0(dir, file_append)
cidx_data <- get_data(pheno_file, bb)
cidx_data
print(unique(cidx_data$SURV_MODEL))

old_pheno_file <- paste0(old_dir, file_append)
old_data <- get_data(old_pheno_file, bb)
print(unique(old_data$SURV_MODEL))

cidx_data <- dplyr::bind_rows(cidx_data, old_data)

cidx_data <- cidx_data %>% dplyr::arrange(Endpoint, SURV_MODEL, Biobank)
cidx_data

res_file <- paste0(res_dir, file_append)
res_file
readr::write_delim(cidx_data, res_file, delim = "\t")

####### Log
log_file <- paste0(res_dir, "README.txt")
log_file
message <- paste0("The files are a combination of the files from ", dir, " and \n", old_dir, " (models without updated PheRS: ", paste0(unique(old_data$SURV_MODEL), collapse=", "), ").\n\n",
                    "\n\n###### Overall Endpoints ######\n\n", paste(unique(hrs_data$Endpoint), collapse = "\n"),
                    "\n\n###### Variables ######\n\n", paste(unique(hrs_data$VAR), collapse = "\n"),
                    "\n\n###### Models ######\n\n", paste(unique(cidx_data$SURV_MODEL), collapse = "\n"))
message
readr::write_file(message, log_file)

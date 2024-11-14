library(dplyr)
source("/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/scripts/utils.R")

#### PATHS

pheno_dir <- "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/orig/ukbb/ukbb_down_2024-07-02/"
prs_dir <- "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/orig/ukbb/ukbb_down_2024-07-02/prs/"

res_dir <- "/home/detrokir/Documents/DSGE/projects/onset_prediction/code/onset_prediction/analysis/data/processed/2024-07-02/bbs/ukb/"

file_names <- "2019-01-01_o8_w2_e10_32_70"
file_append <- paste0(file_names, "_coxph.tsv")
bb <- "UKB"

####### DATA
#### Phenotype data
pheno_file <- paste0(pheno_dir, file_append)
hrs_data <- get_data(pheno_file, bb)
hrs_data
print(unique(hrs_data$SURV_MODEL))
#### PRS data
prs_file <- paste0(prs_dir, file_append)
prs_data <- get_data(prs_file, bb)
#### Combine
hrs_data <- dplyr::bind_rows(hrs_data, prs_data)

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
pheno_file <- paste0(pheno_dir, file_append)
cidx_data <- get_data(pheno_file, bb)
cidx_data
print(unique(cidx_data$SURV_MODEL))
#### PRS data
prs_file <- paste0(prs_dir, file_append)
prs_data <- get_data(prs_file, bb)
#### Combine
cidx_data <- dplyr::bind_rows(cidx_data, prs_data)

# Order
cidx_data <- cidx_data %>% dplyr::arrange(Endpoint, SURV_MODEL, Biobank)
cidx_data

res_file <- paste0(res_dir, file_append)
res_file
readr::write_delim(cidx_data, res_file, delim = "\t")

####### Log
log_file <- paste0(res_dir, "README.txt")
log_file
message <- paste0("The files are a combination of the files from ", pheno_dir, " (all endpoints with phenotypic predictors) \n\n", 
                    prs_dir, " (Models also including PRS for ", prs_data %>% pull(Endpoint) %>% unique() %>% paste0(collapse=", "), ") \n\n and",
                    "\n\n###### Overall Endpoints ######\n\n", paste(unique(hrs_data$Endpoint), collapse = "\n"),
                    "\n\n###### Variables ######\n\n", paste(unique(hrs_data$VAR), collapse = "\n"),
                    "\n\n###### Models ######\n\n", paste(unique(cidx_data$SURV_MODEL), collapse = "\n"))
message
readr::write_file(message, log_file)

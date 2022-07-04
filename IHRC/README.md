# IHRC

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to fit a Cox-proportional hazards model to longitudinal data given different study setups. For creating a study setups see package `Istudy`. Runs the analyis based on risk groups. For `PRS` these are based on the quantiles, where the 40-60% group is the reference group. For `CCI` compares two groups based on a cutoff defined in `bin_cut`.

For the input file format of the phenotypic data see: [INTERVENE Phenotype File Definition](https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit). Additionally, the important column names the date needs to have are inidicated in each function documentation.

### R package Dependencies
 All packages are alread installed in the FinnGen Sandbox.
 
- dplyr - For data manipulation
- tibble - For better data.frames
- ggplot2 - For plotting
- readr - For reading and writing files
- Istudy - For creating the study setup objects
- methods - For working with S4 objects
- survival - For the Cox-PH model

## Example
### Example for CCI analysis
```{r example}
library(IHRC)

icd_file_path <- "/path/to/icd/file_with_name"
icd_data <- readr::read_delim(icd_file_path, delim="\t", col_types=c("cdccc"))
pheno_data <- readr::read_delim(pheno_file_path, delim="\t", col_types=list(SEX="f", DATE_OF_BIRTH="D", ANCESTRY="f", SMOKING="i"))

get_cci_score_age_data <- function(icd_data,
                                   exp_ages=c(20,30,40,50,60),
                                   exp_len=10) {
    score_age_data <- c()
    for(exp_age in exp_ages) {
        score_age_data[[exp_age]] <- ICCI::cacl_cci(icd_data,
                                                    exp_start=exp_age,
                                                    exp_ed=exp_age+10) %>%
                                        dplyr::rename(SCORE=CCI_score)
    }
    return(score_age_data)
}

phenocols <- c("C3_CANCER", "J10_ASTHMA", "G6_EPLEPSY", "F5_DEPRESSIO")
exp_ages <- c(20,30,40,50)
score_age_data <- get_cci_score_age_data(icd_data, exp_ages, 10)

IHRC::run_age_exp_studies(pheno_data,
                          score_age_data,
                          score_type="CCI",
                          endpts=phenocols,
                          exp_ages=exp_ages,
                          exp_len=10,
                          wash_len=2,
                          obs_len=8,
                          downsample_fctr=4,
                          covs=c("SEX", "YEAR_OF_BIRTH"),
                          write_res=TRUE,
                          res_dir="/path/to/results/folder/")
```

### Example for PRS analysis

```{r example}
library(IHRC)

phenocols <- c("C3_CANCER", "J10_ASTHMA", "G6_EPLEPSY", "F5_DEPRESSIO")
prs_cols <- c("AllCancers", "Asthma", "Epilepsy")
col_map <- tibble::tible(pheno=phenocols, prs=prscols)

prs_files_dir <- "/path/to/prs/files"
icd_data <- readr::read_delim(icd_file_path, delim="\t", col_types=c("cdccc"))
pheno_data <- readr::read_delim(pheno_file_path, delim="\t", col_types=list(SEX="f", DATE_OF_BIRTH="D", ANCESTRY="f", SMOKING="i"))
exp_ages <- c(20,30,40,50)

score_age_data <- c()
for(exp_age in exp_ages) {
    score_age_data[[exp_age]] <- ICCI::cacl_cci(icd_data,
                                                exp_start=exp_age,
                                                exp_ed=exp_age+10) %>%
                                    dplyr::rename(SCORE=CCI_score)
}

IHRC::run_age_exp_studies(pheno_data,
                          score_age_data,
                          score_type="CCI",
                          endpts=phenocols,
                          exp_ages=exp_ages,
                          exp_len=10,
                          wash_len=2,
                          obs_len=8,
                          downsample_fctr=4,
                          covs=c("SEX", "YEAR_OF_BIRTH"),
                          write_res=TRUE,
                          res_dir="/path/to/results/folder/")
```
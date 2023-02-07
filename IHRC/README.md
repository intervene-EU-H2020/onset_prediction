# IHRC

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to fit Cox-proportional hazards models to longitudinal data given different study setups and endpoints. For creating a study setups see package `Istudy`. 

For the input file format of the phenotypic data see: [INTERVENE Phenotype File Definition](https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit). Additionally, the important column names the date needs to have are inidicated in each function documentation.

### R package Dependencies
 All packages are alread installed in the FinnGen Sandbox. The minimal versions on which this package has been tester are indicated in brackets. 
 
- dplyr (>= 1.0.10) - For data manipulation
- tibble (>= 3.1.8) - For better data.frames
- ggplot2 (>= 3.4.0) - For plotting
- readr (>= 2.1.3) - For reading and writing files
- methods (>= 4.2.2) - For working with S4 objects
- survival (>= 3.2-7) - For the Cox-PH model
- Hmisc (>= 4.4-2) - For the c-index
- ICCI (>= 1.1.0) - For calculating the CCI
- Istudy (>= 3.3.0) - For selecting the cases and controls
- IUtils (>= 2.0.0) - For file reading and plotting
- lubridate (>= 1.9.0) - For easier dealing with dates
- stringr (>= 1.4.1) - For easier regex


## Example
### Example for CCI analysis
```{r example}
library(IHRC)
library(ICCI)

icd_file_path <- "/path/to/icd/file_with_name"
icd_data <- readr::read_delim(icd_file_path, delim="\t", col_types=c("cdccc"))
pheno_data <- readr::read_delim(pheno_file_path, delim="\t", col_types=list(SEX="f", DATE_OF_BIRTH="D", ANCESTRY="f", SMOKING="i"))

exp_ages <- c(20,30,40,50,60)
exp_len <- 10
score_age_data <- ICCI::calc_cci_for_mult_exp_ages(icd_data, exp_ages, exp_len)

IHRC::run_age_exp_studies(pheno_data,
                          score_age_data,
                          score_type="CCI",
                          endpts=phenocols,
                          exp_ages=exp_ages,
                          exp_len=exp_len,
                          wash_len=2,
                          obs_len=8,
                          downsample_fctr=4,
                          ancs="EUR",
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
exp_len <- 10

score_age_data <- ICCI::calc_cci_for_mult_exp_ages(icd_data,
                                                   exp_ages,
                                                   exp_len=10)
IHRC::run_age_exp_studies(pheno_data,
                          score_age_data,
                          score_type="CCI",
                          endpts=phenocols,
                          exp_ages=exp_ages,
                          exp_len=exp_len,
                          wash_len=2,
                          obs_len=8,
                          downsample_fctr=4,
                          covs=c("SEX", "YEAR_OF_BIRTH"),
                          write_res=TRUE,
                          res_dir="/path/to/results/folder/")
```
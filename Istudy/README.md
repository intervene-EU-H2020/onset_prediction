# Istudy

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to select individuals from the data that are eligible given a certain study setup. A study setup consists of an exposure, a washout, and an observation period. Eligible individuals cannot have the selected endpoint of interest inside the endpoint free period. The endpoint free period ranges from birth until the observation period begins. There are two types of study setups. 

The first one considers individuals of a certain age and a set exposure, washout, and observation periods calcualted onwards from this age. 

![Study Setup](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudy/man/Study_setup_schema.png)

The second one considers all individuals at a set time point. The observation and washout period are calcualted backwards from this time point. The exposure period will be different for each individual depending on their birth date. 
![Study Setup Backwards](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudy/man/Study_Setup_Back_Schema.png)

The important column names for the input data are indicated in each function documentation.

### R package Dependencies
 All packages are alread installed in the FinnGen Sandbox.
 
- assertthat (For testing)
- stats (For creating test data)
- lubridate (For handling dates)
- dplyr (For data manipulation)
- tibble (For better data.frames)
- readr (For reading and writing files)

## Example

```{r example}
library(Istudy)
## basic example code
file_name <- "/finngen/red/Zhiyu/Share/Phenotype/FinnGenR8_Phenotype"
pheno_data <- readr::read_delim(file_name_pheno,
                                delim="\t",
                                col_types=list(SEX="f", DATE_OF_BIRTH="D", ANCESTRY="f", SMOKING="i"))
study <- Istudy::get_study_elig_indv(pheno_data,
                                          endpt="J10_ASTHMA",
                                          exp_age=30,
                                          exp_len=10,
                                          wash_len=2,
                                          obs_len=8
                                          downsample_fctr=4,
                                          write_res=TRUE)
```
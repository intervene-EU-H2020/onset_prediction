# Istudy

<!-- badges: start -->
<!-- badges: end -->

## Istudy

The goal of this package is to select individuals from the data that are eligible given a certain study setup. A study setup consists of an exposure, a washout, and an observation period. Eligible individuals cannot have the selected endpoint of interest inside the endpoint free period. The endpoint free period ranges from birth until the observation period begins. 

There are two types of study setups:

1.`forward` considers individuals of a certain age. It calculates the exposure, washout, and observation period onwards from this age.

![Study Setup](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudy/man/Forward_Study_30_white.svg)

2. `backward` considers all individuals at a set time point. The observation, washout, and exposure period are calcualted backwards from this time point.

![Study Setup Backwards](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudy/man/Back_study_white.svg)

The important column names for the input data are indicated in each function documentation. 

### R package Dependencies
 All packages are alread installed in the FinnGen Sandbox.
 
- assertthat (>= 0.2.1) - For testing
- stats (>= 4.2.2) - For creating test data
- lubridate (>= 1.9.0) - For handling dates
- dplyr (>= 1.0.10) - For data manipulation
- tibble (>= 3.1.8) - For better data.frames
- readr (>= 2.1.3) - For reading and writing files

## Example

```{r example}
library(Istudy)
file_name <- "/finngen/red/Zhiyu/Share/Phenotype/FinnGenR8_Phenotype"
pheno_data <- readr::read_delim(file_name_pheno,
                                delim="\t",
                                col_types=list(SEX="f", DATE_OF_BIRTH="D", ANCESTRY="f", SMOKING="i"))
```
### Forward Study

Example for creating a study forward from a given age, by creating an S4 study object.

```{r example}
set.seed(9231)
test_data <- create_test_df(100)
study <- methods::new("study",
                      study_type="forward",
                      study_data=test_data,
                      endpt="J10_ASTHMA",
                      exp_age=30,
                      exp_len=10,
                      wash_len=2,
                      obs_len=8)  
study@study_data
```

### Backward Study

Example for creating a study backward from a given date, by creating an S4 study object.

```{r example}
set.seed(21923)
test_data <- create_test_df(100)
study <- methods::new("study",
                      study_type="backward",
                      study_data=test_data,
                      endpt="I9_VTE",
                      exp_age=10,
                      wash_len=2,
                      obs_len=8,
                      obs_end_date=as.Date("2019/01/01"))
study@study_data
```
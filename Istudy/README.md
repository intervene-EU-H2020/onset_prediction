# Istudy

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to select individuals from the data that are eligible given a certain study setup. A study setup consists of an exposure, a washout, and an observation period. Eligible individuals cannot have the selected endpoint of interest inside the endpoint free period. The endpoint free period ranges from birth until the observation period begins. 

There are two types of study setups:

1. The first one considers individuals of a certain age and a set exposure, washout, and observation periods calcualted onwards from this age. 

![Study Setup](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudy/man/Study_setup_schema.png)

2. The second one considers all individuals at a set time point. The observation and washout period are calcualted backwards from this time point. The exposure period will be different for each individual depending on their birth date. 
![Study Setup Backwards](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudy/man/Study_Setup_Back_Schema.svg)

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
file_name <- "/finngen/red/Zhiyu/Share/Phenotype/FinnGenR8_Phenotype"
pheno_data <- readr::read_delim(file_name_pheno,
                                delim="\t",
                                col_types=list(SEX="f", DATE_OF_BIRTH="D", ANCESTRY="f", SMOKING="i"))
```
### Forward Study

Example for creating a study forward from a given age, by creating an S4 study object.

```{r example}
study <- methods::new("study",
                      endpt="J10_ASTHMA",
                      exp_age=30,
                      exp_len=10,
                      wash_len=2,
                      obs_len=8,
                      downsample_fctr=4,
                      ancs="AFR")
```
### Backward Study

Example for creating a study backwards from a given time point, using the function `get_backward_study`.

```{r example}
study <- Istudy::get_backward_study(pheno_data,
                                    endpt="J10_ASTHMA",
                                    wash_len=2,
                                    obs_len=8,
                                    obs_end=as.Date("2022/01/01"),
                                    downsample_fctr=4,
                                    ancs="EAS") {
```
### Getting Study Participants

Now we can get the eligible individuals by giving either study setup to the function `get_study_elig_indv`.

```{r example}
study <- Istudy::get_study_elig_indv(pheno_data,
                                     study,
                                     write_res=TRUE,
                                     res_dir="/path/to/res/dir/")
```
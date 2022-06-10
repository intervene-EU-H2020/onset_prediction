# Istudysetup

<!-- badges: start -->
<!-- badges: end -->

The goal of Istudysetup is to select individuals from the data that are eligible given a certain study setup.
A study setup consists of an exposure, a washout, and a prediction period. Eligible individuals cannot have the selected endpoint of interest inside the endpoint free period. The endpoint free period ranges from birth until the prediction period begins.

![Study Setup](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudysetup/man/Stuy_setup_schema.png)

For the input file format see: [INTERVENE Phenotype File Definition](https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit). Additionally, the important column names the date needs to have are inidicated in each function documentation.

## Installation

You can install the development version of Istudysetup from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("intervene-EU-H2020/onset_prediction")
```
### R package Dependencies

- assertthat (For testing)
- stats (For creating test data)
- lubridate (For handling dates)
- dplyr (For)
- tibble
- readr

## Example

```{r example}
library(Istudysetup)
## basic example code
file_name <- "/finngen/red/Zhiyu/Share/Phenotype/FinnGenR8_Phenotype"
pheno_data <- readr::read_delim(file_name_pheno,
                                delim="\t",
                                col_types=list(SEX="f", DATE_OF_BIRTH="D", ANCESTRY="f", SMOKING="i"))
elig_list <- Istudysetup::get_study_elig_indv(pheno_data,
                                              exp_age=30,
                                              exp_len=10,
                                              wash_len=2,
                                              obs_len=8,
                                              endpt="J10_ASTHMA",
                                              downsample_fctr=4)
elig_data <- elig_list$data
```
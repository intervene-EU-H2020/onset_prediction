# Istudy

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to select individuals from the data that are eligible given a certain study setup. A study setup consists of an exposure, a washout, and an observation period. Eligible individuals cannot have the selected endpoint of interest inside the endpoint free period. The endpoint free period ranges from birth until the observation period begins.

![Study Setup](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudy/man/Stuy_setup_schema.png)

For the input file format see: [INTERVENE Phenotype File Definition](https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit). Additionally, the important column names the date needs to have are inidicated in each function documentation.

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
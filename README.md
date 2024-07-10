# Disease Onset Prediction

<!-- badges: start -->
<!-- badges: end -->

This project includes different R packages to help working with INTERVENE longitudinal and phenotype files with the goal of predicting the late-onset of different endpoints using Cox-proporiontal hazards models.

## Installation

Upload to the packages 'ICCI', 'comorbidity' (https://cran.r-project.org/web/packages/comorbidity/), 'IHRC', 'Istudy', and 'IUtils' from this GitHub repository, to the goal environemnt and install as described below.

You can create any directory `/path/to/rpcks/` to install the packages to 

```{r example}
  .libPaths(target_dir, .libPaths())
install.packages("/path/to/rpcks/packag_name.tar.gz",
                 "/path/to/rpcks/",
                 repos = NULL, type="source")
```

You can also find helpful functions for installing all relevant files in the script `installtion_script.R`. Simply change the location for the R library if necessary.

If there are warnings about versions of packages that cannot easily be updated, write me and I might be able to change the minimum necessary version. 

To reinstall packages, delete the folder for the package at `/path/to/rpcks/`, if you work in Rstudio additionally run .rs.restartR() afterwards.

### FinnGen
To install any of the previous named packages in the FinnGen Sandbox, compress it and upload it through green uploads and then use i.e.

See [How to install a R package into Sandbox?](https://finngen.gitbook.io/finngen-analyst-handbook/working-in-the-sandbox/quirks-and-features/how-to-upload-to-your-own-ivm-via-finngen-green/my-r-package-doesnt-exist-in-finngen-sandbox-r-rstudio.-how-can-i-get-a-new-r-package-to-finngen).

# Run

To run the analysis, the simples way is to use a setup file and then run.

```{r example}
library(ICCI)
library(Istudy)
library(IUtils)
library(IHRC)

IHRC::run_ana_setup_file("path/to/setup_file.tsv")
```

## Setup Templates

Both templates run models for all possible combinations of scores. To only get the full model and the covariate model, set `create_score_combos=FALSE`
- `standard_setup_phers.tsv`
  - Runs an analysis with only the PheRS, other phenotypic scores EDU, CCI, and place of birth (ZIP) and covariates Age and Sex
- `standard_setup_prs_phers.tsv`
  - Runs an analysis with both the PheRS and PRS, the other phenotypic scores EUD, CCI, and palce of birth (ZIP) and covariates Age, Sex, and PCs

### Add File Paths to Templates
- `res_dir`: Path to a direcotry where all the results are written to
  - **IMPORTANT, add the final `/` to the path**
- `pheno_file_path`: Path to the [INTERVENE phenotype file](https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit) with **added columns** EDU, and ZIP if used as score. **IMPORTANT** Controls should be marked with 0s not NAs, otherwise they will all be excluded.
- `icd_file_path`: [INTERVENE format longitudinal ICD-code file](https://docs.google.com/document/d/1E2Jc72CmMItEchgQaCvfA4MhZUkQYjALwTu3dCl7qd8/edit) 
- `prs_dir_path`: Path to the directory with all PRS files. The files are expected to be named `[diseas].sscore` see function `IUtils/R/get_prs_endpt_descr.R` for all the disease names. These are the same as used in the flagship paper.
  - **IMPORTANT, add the final `/` to the path**
- `phers_dir_path`: Path to the directory with all PheRS files, created using [Tuomos scripts](https://github.com/intervene-EU-H2020/INTERVENE_PheRS)
  - **IMPORTANT, add the final `/` to the path**

# Dependencies

 All packages are alread installed in the FinnGen Sandbox. The minimal versions on which this package has been tested are indicated in brackets. 

 ## (Likely) need to be manually installed
- ICCI (>= 1.1.0) - For calculating the CCI, see: https://github.com/dsgelab/ICCI
- comorbidity (>= 1.0.0) - For caclulating the CCI, see: https://cran.r-project.org/web/packages/comorbidity/
- IUtils (>= 2.0.0) - For file reading and plotting
- Istudy (>= 3.3.0) - For selecting the cases and controls
- IHRC (>= 4.0.0) - For running the survival analysis

## (Likely) already present

- dplyr (>= 1.0.10) - For data manipulation
- tibble (>= 3.1.8) - For better data.frames
- ggplot2 (>= 3.4.0) - For plotting
- readr (>= 2.1.3) - For reading and writing files
- methods (>= 4.2.2) - For working with S4 objects
- survival (>= 3.2-7) - For the Cox-PH model
- lubridate (>= 1.9.0) - For easier dealing with dates
- stringr (>= 1.4.1) - For easier regex
- assertthat (>= 0.2.1) - For testing
- stats (>= 4.2.2) - For creating test data
- Hmisc (>= 4.4-2) - For the c-index
# Packages

## Istudy

The goal of this package is to select individuals from the data that are eligible given a certain study setup. A study setup consists of an exposure, a washout, and an observation period. Eligible individuals cannot have the selected endpoint of interest inside the endpoint free period. The endpoint free period ranges from birth until the observation period begins. 

The study, considers all individuals at a set time point. The observation, washout, and exposure period are calcualted backwards from this time point.

![Study Setup Backwards](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudy/man/Back_study_white.svg)

The important column names for the input data are indicated in each function documentation. 
## IHRC

This package builds the Cox-propotional hazards models (Cox-PH) for the different endpoints and study setups. Calcualtes the Hazard Ratios(HRs) and the c-index for the different models. 

To make the PheRS and PGS comparable we regressed out the effect of age, sex and the first 10 genetic PCs from all continuous scores using the residuals from a logistic regression with the score as outcome and Subsequently we scaled all predictors to have a mean of zero and standard deviation of 1. 

```{r example}
scale(residuals(glm(scale(PRS_orig)~scale(YEAR_OF_BIRTH)+SEX+scale(PC1)+scale(PC2)+scale(PC3)+scale(PC4)+scale(PC5)+scale(PC6)+scale(PC7)+scale(PC8)+scale(PC9)+scale(PC10), data=score_data)))[,1]
```

We then used these scores in separate Cox proportional-hazards models (Cox-PH), with the survival time defined as the time from 2011 until either diagnosis, censoring (end of follow-up), or the end of the prediction period. 

```{r example}
  surv_obj <- survival::Surv(time=dplyr::pull(study_data, paste0(endpt, "_AGE_FROM_BASE")), event=dplyr::pull(study_data, endpt))
  coxph_formula <- stats::as.formula(paste0(surv_obj, " ~ ",  "[PheRS/PRS/CCI+Age+Sex etc.]"))

  coxph_mdl <- survival::coxph(formula=coxph_formula, 
                               data=study@study_data, 
                               # Larger fit object but no need for
                               # other functions to reconstruct
                               # which fails in this setup
                               model=TRUE)

  # HRs
  betas <- summary(coxph_mdl)$coefficients[,"coef"]
  SE <- summary(coxph_mdl)$coefficients[,"se(coef)"]
  pvals <- summary(coxph_mdl)$coefficients[,"Pr(>|z|)"]
  OR <- exp(betas)
  CI <- get_CI(betas, SE)
  preds <- rownames(summary(coxph_mdl)$coefficients)
```

We used the survival package in R for creating the Cox-PH models and the Hmisc package to calculate the c-indices and 95% CIs. 

```{r example}
preds <- (-1)*predict(coxph_mdl, type="lp")
c_idx <- Hmisc::rcorr.cens(preds, surv_obj)

SE<-c_idx["S.D."]/2
C_IDX<-c_idx["C Index"]
C_IDX_CI_NEG <- C_IDX-1.96*SE
C_IDX_CI_POS <- C_IDX+1.96*SE
```

For the CCI we compared the top 10% of individuals with the highest CCI to the rest. The high risk group included individuals with a CCI>=2 and a few younger ones with a CCI of 1. For the highest education level we compared the risk of individuals with basic education (ISCED-11: 1-4) to those who achieved high education levels (ISCED-11: 5-7). 
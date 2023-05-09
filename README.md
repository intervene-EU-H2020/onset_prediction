# Disease Onset Prediction

<!-- badges: start -->
<!-- badges: end -->

This project includes different R packages to help working with INTERVENE longitudinal and phenotype files with the goal of predicting the late-onset of different endpoints using Cox-proporiontal hazards models.

## Installation

To download the packages, clone this repository and compress the packages `IHRC`, `IUTils`, `Istudy`, separately. Then upload to the goal environemnt and install as described below.


```{r example}
install.packages("/finngen/green/path/to/package/packag_name.tar.xz",
                 "/home/ivm/R/x86_64-pc-linux-gnu-library/4.1",
                 repos = NULL, type="source")
```

You can also find helpful functions for installing all relevant files in the script `installtion_script.R`.
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
- `pheno_file_path`: Path to the [INTERVENE phenotype file](https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit) with **added columns** EDU, and ZIP if used as score
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

## IUtils

Helper functions for plotting results.


# Disease Onset Prediction

<!-- badges: start -->
<!-- badges: end -->

This project includes different R packages to help working with INTERVENE longitudinal and phenotype files with the goal of predicting the late-onset of different endpoints using Cox-proporiontal hazards models.

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

## Installation

To install any of the previous named packages in the FinnGen Sandbox, compress it and upload it through green uploads and then use i.e.

```{r example}
install.packages("/finngen/green/path/to/package/packag_name.tar.xz",
                 "/home/ivm/R/x86_64-pc-linux-gnu-library/4.1",
                 repos = NULL, type="source")
```

See [How to install a R package into Sandbox?](https://finngen.gitbook.io/finngen-analyst-handbook/working-in-the-sandbox/quirks-and-features/how-to-upload-to-your-own-ivm-via-finngen-green/my-r-package-doesnt-exist-in-finngen-sandbox-r-rstudio.-how-can-i-get-a-new-r-package-to-finngen).

# Dependencies

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
- assertthat (>= 0.2.1) - For testing
- stats (>= 4.2.2) - For creating test data
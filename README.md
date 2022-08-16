# Disease Onset Prediction

<!-- badges: start -->
<!-- badges: end -->

This project includes different R packages to help working with INTERVENE longitudinal and phenotype files with the goal of predicting the late-onset of different endpoints.


# Packages
## ICCI

Calculates the Charlson Comorbidity Index (CCI) on longitudinal data in INTERVENE format, using the R package `comorbidity`.
## Istudy

The goal of this package is to select individuals from the data that are eligible given a certain study setup. A study setup consists of an exposure, a washout, and an observation period. Eligible individuals cannot have the selected endpoint of interest inside the endpoint free period. The endpoint free period ranges from birth until the observation period begins. 

There are two types of study setups:

1. The first one considers individuals of a certain age and a set exposure, washout, and observation periods calcualted onwards from this age. 

![Study Setup](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudy/man/Forward_Study_160822.svg)

2. The second one considers all individuals at a set time point. The observation and washout period are calcualted backwards from this time point. The exposure period will be different for each individual depending on their birth date. 

![Study Setup Backwards](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudy/man/Study_Setup_Back_Schema.svg)
The important column names for the input data are indicated in each function documentation.
## IHRC

Builds Cox-propotional hazards model (Cox-PH) for the different endpoints and study setups. Calcualtes the Hazard Ratios(HRs) and the c-index for the different models. 

## IUtils

Helper functions for working with INTERVENE Longitudinal Files and plotting results.

## Installation

To install any of the previous named packages in the FinnGen Sandbox, compress it and upload it through green uploads and then use i.e.

```{r example}
install.packages("/finngen/green/path/to/package/packag_name.tar.xz",
                 "/home/ivm/R/x86_64-pc-linux-gnu-library/4.1",
                 repos = NULL, type="source")
```

See [How to install a R package into Sandbox?](https://finngen.gitbook.io/finngen-analyst-handbook/working-in-the-sandbox/quirks-and-features/how-to-upload-to-your-own-ivm-via-finngen-green/my-r-package-doesnt-exist-in-finngen-sandbox-r-rstudio.-how-can-i-get-a-new-r-package-to-finngen).

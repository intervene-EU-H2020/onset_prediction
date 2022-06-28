# Disease Onset Prediction

<!-- badges: start -->
<!-- badges: end -->

This project includes different R packages to help working with INTERVENE longitudinal and phenotype files with the goal of predicting the late-onset of different endpoints.

# Packages
## ICCI

Calculates the Charlson Comorbidity Index (CCI) on longitudinal data in INTERVENE format, using the R package `comorbidity`.

## IHRC

Calculates the Hazard Ratios (HRs) using a Cox-propotional hazards model (Cox-PH).

## Istudy

Select individuals from the data that are eligible given a certain study setup. A study setup consists of an exposure, a washout, and an observation period. Eligible individuals cannot have the selected endpoint of interest inside the endpoint free period. The endpoint free period ranges from birth until the observation period begins.

![Study Setup](https://github.com/intervene-EU-H2020/onset_prediction/blob/main/Istudy/man/Stuy_setup_schema.png)

## ILongDataUtils

Helper functions for working with INTERVENE Longitudinal Files.
# ICCI

<!-- badges: start -->
<!-- badges: end -->

- This is an R package that calculates the Charlson Comorbidity Index on longitudinal ICD data, using the R package `comorbidity`. 
    - The data should have at least the columns `ID` and `primary_ICD`, and `ICD_version`.
    - If you want to restrict the exposure period to calculate the index on, the data needs an additional column `Event_age`.
- The package can handle different ICD-versions for the same individual.
    - Possible entries for the column `ICD_version` are "10", "10CM", "9", or "9CM".

### R package Dependencies

- assertthat (For testing)
- comorbidity (>= 1.0.0, For calculating the CCI)
- dplyr (For data manipulation)
- tibble (For better data.frames)

## Example

```{r example}
library(dplyr)
library(tibble)
library(comorbidity)
library(ICCI)

file_name <- "/path/to/file/longitudinal_icd_data.txt"
icd_data <- readr::read_delim(file_name)
score_data <- ICCI::calc_cci(icd_data)
score_data_age_20_to_30 <- ICCI::calc_cci(icd_data, 
                                          exp_start=20,
                                          exp_end=30)
```

For variable exposure windows for different individuals there are two options:

1. Give either or both `exp_start`, and `exp_end` vectors of the exact same length as the
original data with the  different exposure periods. This is the ideal option if the start and end of the expsoure period are already part of the same data.frame as the ICD data.

```{r example}
mock_data <- tibble::tibble(ID=c("KT0000001", "KT0000002", "KT0000001", 
                            Event_age=c(12.3, 89, 23.4), 
                            primary_ICD=c("Y728", "M797", "E283"), 
                            secondary_ICD=c(NA, NA, NA), 
                            ICD_version=c("10", "10", "10"),
                            Exp_start=c(10, 40, 10),
                            Exp_end=c(50, 70, 50))
ICCI::calc_cci(icd_data=mock_data,
               exp_start=mock_data$Exp_start,
               exp_end=mock_data$Exp_end)
```

2. Give both `exp_start` and `exp_end`  a data.frame with each a column `ID`, and then `EXP_START` for the `exp_start` argument and `EXP_END`, for the `exp_end` argument. This way ensures that the exposures are mapped to the correct IDs. Thus, this is the better option if the information on exposure period comes from another data source. 

```{r example}
mock_data <- tibble::tibble(ID=c("KT0000001", "KT0000002", "KT0000001", 
                            Event_age=c(12.3, 89, 23.4), 
                            primary_ICD=c("Y728", "M797", "E283"), 
                            secondary_ICD=c(NA, NA, NA), 
                            ICD_version=c("10", "10", "10"))
Exp_start <- tibble::tibble(ID=c("KT0000001", "KT0000002"),
                            EXP_START=c(10, 40))
Exp_end <- tibble::tibble(ID=c("KT0000001", "KT0000002"),
                            EXP_START=c(50, 70))

ICCI::calc_cci(icd_data=mock_data,
               exp_start=Exp_start,
               exp_end=Exp_end)
```
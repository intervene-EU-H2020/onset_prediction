# ICCI

<!-- badges: start -->
<!-- badges: end -->


- This is an R package that calculates the Charlson Comorbidity Index on longitudinal data in INTERVENE format, using the R package comorbidity.
    - The data should have at least the columns `ID` and `primary_ICD`, and `ICD_version`.
    - If you want to restrict the exposure period to calculate the index on, the data needs an additional column Event_age.
- The package can handle different ICD-versions for the same individual.
    - Possible entries for the column `ICD_version` are "10", "10CM", "9", or "9CM".

### R package Dependencies
 All packages are alread installed in the FinnGen Sandbox.
 
- assertthat (For testing)
- comorbidity (For calculating the CCI)
- dplyr (For data manipulation)
- tibble (For better data.frames)

## Example

```{r example}
library(ICCI)
## basic example code
file_name <- "/path/to/file/finngen_R8_detailed_longitudinal_INTERVENE_format.txt"
icd_data <- readr::read_delim(file_name,
                              delim="\t",
                              col_types=c("cdccc"))
score_data <- ICCI::calc_cci(icd_data)
score_data_age_20_to_30 <- ICCI::calc_cci(icd_data, 
                                          exp_start=20,
                                          exp_end=30)
```
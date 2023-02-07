# IHRC

<!-- badges: start -->
<!-- badges: end -->

The goal of this package is to fit Cox-proportional hazards models to longitudinal data given different study setups and endpoints.

For the input file format of the phenotypic data see: [INTERVENE Phenotype File Definition](https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit). Additionally, the important column names the date needs to have are inidicated in each function documentation.

### R package Dependencies
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


## Example

It's easiest to use a setup file and then simply run

```{r example}
library(ICCI)
library(Istudy)
library(IUtils)
library(IHRC)

IHRC::run_ana_setup_file("path/to/setup_file.tsv")
```


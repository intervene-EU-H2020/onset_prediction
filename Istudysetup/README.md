# Istudysetup

<!-- badges: start -->
<!-- badges: end -->

The goal of Istudysetup is to select individuals from the data that are eligible given a certain study setup.
A study setup consists of an exposure, a washout, and a prediction period. Eligible individuals cannot have the selected endpoint of interest inside the endpoint free period. The endpoint free period ranges from birth until the prediction period begins.

For the input file format see: [https://docs.google.com/document/d/1GbZszpPeyf-hyb0V_YDx828YbM7woh8OBJhvzkEwo2g/edit](INTERVENE Phenotype File Definition). Additionally, the important column names the date.frame needs to at least have are inidicated in each function documentation.

![BLa]("man/Study_setup_schema.png")

## Installation

You can install the development version of Istudysetup from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("intervene-EU-H2020/onset_prediction")
```

## Example

This is a basic example which shows you how to solve a common problem:

```{r example}
library(Istudysetup)
## basic example code
```

What is special about using `README.Rmd` instead of just `README.md`? You can include R chunks like so:

```{r cars}
summary(cars)
```

You'll still need to render `README.Rmd` regularly, to keep `README.md` up-to-date. `devtools::build_readme()` is handy for this. You could also use GitHub Actions to re-render `README.Rmd` every time you push. An example workflow can be found here: <https://github.com/r-lib/actions/tree/v1/examples>.

You can also embed plots, for example:

```{r pressure, echo = FALSE}
plot(pressure)
```

In that case, don't forget to commit and push the resulting figure files, so they display on GitHub and CRAN.

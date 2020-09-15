
<!---usethis::use_readme_rmd() -->

<!-- README.md is generated from README.Rmd. Please edit that file -->

# onlineforecast

<!--
usethis::use_cran_badge()
usethis::use_lifecycle_badge("stable")
-->

<!-- badges: start -->

[![CRAN
status](https://www.r-pkg.org/badges/version/onlineforecast)](https://CRAN.R-project.org/package=onlineforecast)
[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://www.tidyverse.org/lifecycle/#stable)
[![CRAN RStudio mirror
downloads](http://cranlogs.r-pkg.org/badges/onlineforecast)](https://cran.r-project.org/package=onlineforecast)
<!-- badges: end -->

The *onlineforecast* R package computes new forecasts whenever new
observations becomes available by using recursive estimation. The
package makes it easy to setup models to be used for online forecasting,
where the user can create their own online forecast-set-up with their
own inputs. The pacakge is created based on the authors long experience
of formulating time series models which use weather forecasts as inputs.
The challenge comes from the overlapping time series as the forecast
needs to be updated whenever the weather forecast is updated.

See more on [onlineforecast](https://onlineforecasting.org/) webpage

## Installation

You can install the released version of onlineforecast from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("onlineforecast")
```

## Example

See examples on how to use the package on the website,
[onlineforecast](https://onlineforecasting.org/examples.html) where
multiple vignettes demonstrate how it works.

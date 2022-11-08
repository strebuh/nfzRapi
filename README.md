
<!-- README.md is generated from README.Rmd. Please edit that file -->

# nfzRapi

<!-- badges: start -->

[![R-CMD-check](https://github.com/strebuh/nfzRapi/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/strebuh/nfzRapi/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

nfzRapi is a set of functions to get data from
`https://api.nfz.gov.pl/app-umw-api/` - an API of Narodowy Fundusz
Zdrowia. As for September 2022 availabe functions let the user to get
general data about agreements and providers.

## Installation

You can install the development version of nfzRapi from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("strebuh/nfzRapi")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(nfzRapi)

# get available years
agr_get_years()
#>  [1] 2008 2009 2010 2011 2012 2013 2014 2015 2016 2017 2018 2019 2020 2021 2022
#> [16] 2023

## get agreements of providers in Łosice, Mazovian voivodship in 2020
df = agr_get_agreements(year=2020, admin_branch='07', town="Łosice", service_type='03')
#> [1] "https://api.nfz.gov.pl/app-umw-api/agreements?year=2020&branch=07&serviceType=03&place=Losice&limit=1&format=json&api-version=1.2"
#> No availabe data for queried scope. Please make sure the arguements are correct.
#> If any of the provided arguments contains Polish characters make sure the local encoding is one of 1250 variations. Run Sys.getlocale('LC_CTYPE') command; if it doesn't show any of 1250's You may call Sys.setlocale('LC_CTYPE', 'Polish_Poland.1250') command.


## get detail about providers in the same year and town
df2 = agr_get_providers(year=2020, admin_branch='07', town="Łosice")
#> [1] "https://api.nfz.gov.pl/app-umw-api/providers?year=2020&branch=07&place=Losice&limit=1&format=json&api-version=1.2"
#> No availabe data for queried scope. Please make sure the arguements are correct.
#> If any of the provided arguments contains Polish characters make sure the local encoding is one of 1250 variations. Run Sys.getlocale('LC_CTYPE') command; if it doesn't show any of 1250's You may call Sys.setlocale('LC_CTYPE', 'Polish_Poland.1250') command.
```


<!-- README.md is generated from README.Rmd. Please edit that file -->

# SESverse

<!-- badges: start -->
<!-- badges: end -->

The goal of SESverse is to examine the flexibility of calculating
Socioeconomic status in behaviour science literature.

## Installation

You can install the development version of SESverse like so:

``` r
install_github("Ziqian-xia/SESverse")
```

## Example

This is a basic example which shows you how to use this package.

Import the package:

``` r
library(SESverse)
```

Datasets are pre-loaded in the package for demo:

-   CFPS 2010 Child
-   PSID elderly

``` r
summary(CFPS_child)

summary(PSID_elderly)
```

There’re many ways to calculate SES, we have coded them to make
comparisons.

-   Betan: Computation method of SES proposed by Betancourt, L, 2016
-   More (Updating)

``` r
betan(faminc=50000,familysize = 5,edu_m = 12,method = 'raw',data = 'CFPS')
#> # A tibble: 1 x 1
#>   SES_betan_cfps
#>            <dbl>
#> 1            3.5
```

    #> # A tibble: 1 x 1
    #>   SES_betan_psid
    #>            <dbl>
    #> 1              2

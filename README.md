
<!-- README.md is generated from README.Rmd. Please edit that file -->

# **jspr (Pronounced *ˈja-spər* or *ˈdʒæspə*)**

<!-- badges: start -->
<!-- badges: end -->

## Introduction

What is the definition of jasper?

This is how ChatGPT defines jasper when asked to make the definition of
jasper long and boring.

*Jasper is a cryptocrystalline variety of quartz characterized by its
opaque appearance and intricate patterns, commonly exhibiting hues
ranging from red and brown to yellow and green. Its formation occurs
through the slow crystallization of silica-rich fluids within
sedimentary rocks or volcanic deposits, a process that typically spans
extended geological timescales.*

*Geologically, jasper is classified as a microcrystalline quartz,
distinguished by its dense, fine-grained structure which contributes to
its durability and polishability. This mineral’s historical significance
stems from its enduring use in decorative arts and jewelry across
civilizations, owing to its aesthetic appeal and cultural symbolism.*

*Metaphysically, jasper holds significance in esoteric practices,
believed to impart qualities of stability, grounding, and endurance.
Different varieties of jasper are purported to exhibit distinct
energetic properties, influencing its application in spiritual and
holistic healing traditions.*

*In contemporary contexts, jasper continues to find application in
lapidary work and industrial uses due to its hardness and attractive
color variations. Its presence in geological formations provides
insights into ancient environments and processes of mineral formation,
contributing to broader scientific understanding and exploration of
Earth’s natural history.*

This package has nothing to do with rocks. This is a personal package
containing useful custom functions used on a daily basis in the life of
a unlikely human working with environmental data.

## **Installation**

You can install the development version of jspr from
[GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("jamespyon/jspr")
```

## **Features**

Here are some features of the *jspr* R package.

### `calculate_epc()`

Taken and reformatted from *atsdrepc* R package, this function
calculates the exposure point concentration used in health consultations
or other projects. The original uses a shiny application that requires
an .xlsx template input that is complicated with R. This function helps
makes the process easier for those who know what to do. It requires an
input of observations (numeric vector) and censoring (logical vector)
and is best run together with *dplyr* verb functions `group_by()` and
`summarise()`. For output, `retval` is epc.

``` r
library(jspr)

set.seed(20240406)

#example of individual inputs
results <- rexp(n = 16, rate = 1)
nondetects <- results<0.8

calculate_epc(obs = results, cen = nondetects)$retval
#> [1] 1.490921

#example of dplyr verbs with dataframe
library(tidyverse)
#> ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
#> ✔ dplyr     1.1.4     ✔ readr     2.1.5
#> ✔ forcats   1.0.0     ✔ stringr   1.5.1
#> ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
#> ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
#> ✔ purrr     1.0.2     
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()
#> ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

data.frame(results, nondetects, group = rep(c("A", "B"), 8)) %>%
  group_by(group) %>%
  summarise(epc = calculate_epc(obs = results, cen = nondetects)$retval)
#> # A tibble: 2 × 2
#>   group   epc
#>   <chr> <dbl>
#> 1 A      1.99
#> 2 B     36.9
```

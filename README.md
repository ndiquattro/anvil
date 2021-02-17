
# anvil

<!-- badges: start -->
[![R build status](https://github.com/alloy-commons/anvil/workflows/R-CMD-check/badge.svg)](https://github.com/alloy-commons/anvil/actions)
<!-- badges: end -->

The goal of anvil is to simplify access to Alloy data via Athena and S3. It can also serve as a repository for common analyses and utility functions we wish to share and improve upon.

## Installation

You can install the released version of anvil with:

``` r
#install.packages("remotes")
remotes::install_github("alloy-commons/anvil")
```

## Docs
In order to get a friendly website for anvil's docs displayed run the following:

``` r
show_docs()
```

## Example

Here are just a few patterns to give a flavor for what is possible. Note that all anvil functions start with `av_`.


First some code for calculating the % of women born per year.

``` r
library(tidyverse)
library(lubridate)
library(anvil)

av_connect("athena")

av_get_table("rds-export-graphite", "graphite_person") %>% 
  mutate(year_born = year(as_date(birth_date))) %>% 
  group_by(year_born) %>% 
  summarise(
    pct_female = mean(as.numeric(gender == "F"), na.rm = TRUE),
    n = n()
  ) %>% 
  arrange(desc(pct_female))
```

What if someone sends you some SQL they were using?

``` r
query <- 
'
SELECT *
FROM "rds-export-graphite".graphite_address
WHERE state = \'CA\'
'

av_get_query(query) %>% 
  filter(city == "SAN FRANCISCO")
```

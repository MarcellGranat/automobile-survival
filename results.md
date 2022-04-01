results - automobile-survival
================

## Causality map

``` mermaid
%%{init: {'theme': 'neutral' } }%%
graph LR
   A[fa:fa-car Characteristics of the car]
   B[fa:fa-tag Offer price]
   C(fa:fa-coins Relative price)
   D(fa:fa-calendar-check Time of sale)
   E[fa:fa-chart-line Economic environment]

   A -->|Q0| C
   A -->|Q2| D
   B -->|Q0| C
   D -->|Q1| C
   C -->|Q1| D
   B -->|Q2| D
   E -..-> A
   E -..-> B
```

## Setup

``` r
library(tidyverse)
```

## Data

``` r
load("data/cars_data.RData")

download_days <- prices_df %>% 
  pull(date) %>% 
  unique() %>% 
  sort()

Sys.setlocale("LC_TIME", "C") # mac os specific language setup
```

    ## [1] "C"

``` r
## [1] "C"
 calendR::calendR(
  start_date = "2021-05-01",
  end_date =  "2022-02-28",
  special.col = "lightblue",
  special.days = download_days - as.Date("2021-04-30"),
  start = "M"
)
```

![](figures/calendaer-1.png)<!-- -->

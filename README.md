# Alcohol-Consumption-by-Income-and-Birth-Sex

## Project Overview
This exploratory analysis seeks to examine the relationship between income, birth sex, and alcohol consumption levels. The goal is to uncover patterns and correlations that may provide valuable insights into how these factors influence drinking behaviors. Specifically, the analysis will investigate whether income levels or birth sex are associated with variations in alcohol consumption, including frequency and quantity of consumption.

By analyzing available data, the study aims to:

1. Identify any significant trends or disparities in alcohol consumption across different income brackets.

2. Explore how alcohol consumption patterns differ between individuals assigned male or female at birth.
   
3. Provide an understanding of how socio-economic factors like income interact with demographic characteristics like sex to affect drinking habits.
   
This exploration could have implications for public health policies, targeted interventions, and further research on the factors influencing alcohol use across various population groups.


It will also look at how max number of drinks on one occasion for an individual effect the weekly alcohol consumption levels.  I use RStudio for the entire exploration and R Markdown for reporting the process and the results.

## Data Sources
The dataset I will be analyzing comes from the [Centers for Disease Control](www.cdc.gov).  “The BRFSS is a system of ongoing health-related telephone surveys designed to collect data on health-related risk behaviors, chronic health conditions, health-care access, and use of preventive services from the
noninstitutionalized adult population (≥ 18 years) residing in the United States and participating areas.” 

## Tools
RStudio [Download_here] (https://posit.co/downloads/)

I have divided the project into 3 phases: 

Phase 1: Cleaning and preparing the data.
Phase 2: Analyzing and visualizing data.
Phase 3: Sharing insights from the analysis


Let’s get started! 

## Phase 1: Cleaning and preparing the data

###Load the libraries:

``` r
library(tidyverse)
library(psych)
library(lm.beta)
```
###Load dataset: 

``` r
brf <- read_csv("brfss2021.csv", show_col_types = FALSE)
```

###Select variables 
``` r
brf_part2 <- brf |>
  select(MAXDRNKS, ALCDAY5, BIRTHSEX, INCOME3) |>
as.data.frame()
```

###Remove unwanted responses

brf_part2 <- brf_part2 %>%
  filter(
    !BIRTHSEX %in% c(7, 9) &   
    !INCOME3 %in% c(77, 99) & 
    !ALCDAY5 %in% c(777,999,888) &
    !MAXDRNKS %in% c(77,99)
    
  ) %>%
 filter(!is.na(MAXDRNKS) & !is.na(BIRTHSEX) & !is.na(INCOME3))











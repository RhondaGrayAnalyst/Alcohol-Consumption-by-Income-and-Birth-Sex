# Alcohol-Consumption-by-Income-and-Birth-Sex

## Table of Contents
   - [Project Overview](#project-overview)
   - [Data Sources](#data-sources)
   - [Tools](#tools)
   - [Phase 1: Cleaning and preparing the data](#phase-1:-cleaning-and-preparing-the-data)

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
Phase 2: Exploratory Data Analysis and Visualization
Phase 3: Sharing insights from the analysis


Let’s get started! 

## Phase 1: Cleaning and preparing the data

   ## 1.1. Loading packages and data
### Load packages for analysis:

``` r
library(tidyverse)
library(psych)
library(lm.beta)
```
### Load dataset and assign name: 

``` r
brf <- read_csv("brfss2021.csv", show_col_types = FALSE)
```
   ### 1.2. Reviewing dataset
```r
glimpse (brf)
```
```r
head (brf)

### Select variables 
``` r
brf_part2 <- brf |>
  select(MAXDRNKS, ALCDAY5, BIRTHSEX, INCOME3) |>
as.data.frame()
```

   ### 1.3 Remove unwanted responses

``` r
brf_part2 <- brf_part2 %>%
  filter(
    !BIRTHSEX %in% c(7, 9) &   
    !INCOME3 %in% c(77, 99) & 
    !ALCDAY5 %in% c(777,999,888) &
    !MAXDRNKS %in% c(77,99)
    
  ) %>%
 filter(!is.na(MAXDRNKS) & !is.na(BIRTHSEX) & !is.na(INCOME3))
```
   ### 1.4 Create variable DKDAY to show number of times they drink per week
```r
brf_part2 <- brf_part2 %>%
  mutate(
    DKDAY = case_when(
      ALCDAY5 > 0 & ALCDAY5 < 200 ~ round(as.numeric(substr(as.character(ALCDAY5), 2, nchar(as.character(ALCDAY5)))), 2),
      ALCDAY5 >= 200 & ALCDAY5 < 300 ~ round(as.numeric(substr(as.character(ALCDAY5), 2, nchar(as.character(ALCDAY5)))) / 7, 2),
      TRUE ~ ALCDAY5 # Handle other cases
    )
  ) |>
  select(DKDAY, everything(), -ALCDAY5)  # Place DKDAY as the first column and remove ALCDAY5
```



### 1.5 Data Attributes

I created a new variable called "DKDAY" using "ALCDAY5" to show the number of times they drank per week.  This is a measurable number to be used to compare variable relationships.  

The 4 variables I will use include "MAXDRNKS","BIRTHSEX" "INCOME3",and  "DKDAY".

"MAXDRNKS" indicates the max number of drinks someone had on any occasion in the last 30 days.
The coding and levels for "MAXDRNKS":

Number
of drinks
77 Don’t
know / Not
sure
99 Refused

The coding and levels for "ALCDAY5" (converted into "DKDAY" *values included below)

1 _ _ Days per
week
2 _ _ Days in
past 30 days
Read if necessary:
A 40-ounce beer
would count as 3
drinks, or a cocktail
drink with 2 shots
would count as 2
drinks.
888 No drinks
in past 30
days
777 Don’t
know / Not sure
999 refused

"DKDAY" shows number of times they drink per week. Value range is 0.14 - 7. This variable is trimmed to only show range 1-7.

"BIRTHSEX" is binary and indicates sex of male or female at the time of birth. 
The coding and levels for "BIRTHSEX":

1 Male
2 Female
7 Don’t
know/Not sure
9 Refused


"INCOME3" notes the annual household income from all sources. 
The coding and levels for "Income3":

01 Less than $10,000?
02 Less than $15,000?
($10,000 to less than
$15,000)
03 Less than $20,000?
($15,000 to less than
$20,000)
04 Less than $25,000
05 Less than $35,000 If
($25,000 to less than
$35,000)
06 Less than $50,000 If
($35,000 to less than
$50,000)
07 Less than $75,000?
($50,000 to less than
$75,000)
08 Less than $100,000?
($75,000 to less than
$100,000)
09 Less than $150,000?
($100,000 to less than
$150,000)?
10 Less than $200,000?
($150,000 to less than
$200,000)
11 $200,000 or more
77 Don’t know / Not sure
99 Refused

### 1.6. Find and Remove Outliers using boxplots and IQR Method

I used boxplots to view and analyze the outliers in the data for all four variables. I filtered out the outliers in the INCOME3 and MAXDRNKS variable using the IQR method based on the upper fence because that is where the outlier(s) were located on the boxplot above the 3rd quartile range.   I filtered out the outliers data in DKDAY that was less than one based on my analysis after reviewing the boxplot. 

```r
boxplot(brf_part2$DKDAY)

#unique(brf_partt$DKDAY)

boxplot(brf_part2$MAXDRNKS)

boxplot(brf_part2$INCOME3)

#remove outliers for under 1
brf_part2 <- brf_part2 %>% 
  filter(DKDAY >= 1)

#create upper fence for Income using IQR quantile
upper_fence <- quantile(brf_part2$INCOME3, 0.75) + (1.5 * IQR(brf_part2$INCOME3))

#remove outliers from income
brf_part2 <- brf_part2 |>
  filter (
    INCOME3 < upper_fence
  )
#create upper fence for MAXDRINKS using IQR quantile
upper_fence2 <- quantile(brf_part2$MAXDRNKS, 0.75) + (1.5 * IQR(brf_part2$MAXDRNKS))

#remove outliers from MAXDRNKS
brf_part2 <- brf_part2 |>
  filter (
    MAXDRNKS < upper_fence
  )
```

## Phase 2: Exploratory Data Analysis and Visualization

I completed exploratory analyses for each of the 4 variables by creating scatterplot visualizations with ggplot2. I also included isualizations that incorporates a combination of variables within one plot. 

#INCOME3
```r
ggplot(brf_part2, aes(x = "", y = INCOME3)) + 
  geom_boxplot() +
  labs(title = "Boxplot of INCOME3", y = "INCOME3") +
  theme_minimal()
```

#MAXdRNKs
```r
ggplot(brf_part2, aes(x = "", y = MAXDRNKS)) + 
  geom_boxplot() +
  labs(title = "Boxplot of MAXDRNKS", y = "MAXDRNKS") +
  theme_minimal()
```

#DKDAY
```r
ggplot(brf_part2, aes(x = "", y = DKDAY)) + 
  geom_boxplot() +
  labs(title = "Boxplot of DKDAY", y = "DKDAY") +
  theme_minimal()
```

#BIRTHSEX

```r
ggplot(brf_part2, aes(x = "", y = BIRTHSEX)) + 
  geom_boxplot() +
  labs(title = "Boxplot of BIRTHSEX", y = "BIRTHSEX") +
  theme_minimal()
```

#Boxplot of MAXDRNKS by Income"
```r
ggplot(brf_part2, aes(x = factor(INCOME3), y = MAXDRNKS)) +
  geom_boxplot() +
  labs(title = "Boxplot of MAXDRNKS by Income",
       x = "INCOME3",
       y = "MAXDRNKS") +
  theme_minimal()
```

#Boxplot of MAXDRNKS by Birth Sex
```r
ggplot(brf_part2, aes(x = factor(BIRTHSEX), y = MAXDRNKS)) +
  geom_boxplot() +
  labs(title = "Boxplot of MAXDRNKS by Birth Sex",
       x = "BIRTHSEX",
       y = "MAXDRNKS") +
  theme_minimal()
```

## Phase 3: Sharing insights from the analysis

Looking at the "Boxplot of MAXDRNKS by Income", I can see that highest values for max drinks is from middle- class people within the range of income $35,000 - $100,00.  When looking at "Boxplot of MAXDRNKS by Birth Sex" the data shows a relatively slight difference in the median, showing 1 (male) higher. The person who drank the most is a a male at birth. I am surprised at these findings so far because of the ostensibly insignificant of the relationship between birth sex and income. Lowest level of income does show the highest median of max drinks.  It will be interesting to further explore and see the highest predictor variable between sex and income.

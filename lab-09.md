Lab 09: Algorithmic Bias
================
Lilly McClendon
04.04.2025

## Load Necessary Packages

First, let’s load the necessary packages:

``` r
library(tidyverse)
library(janitor)
library(fairness)
```

## Load the Data

``` r
compas <- read_csv("data/compas-scores-2-years.csv") %>%
  clean_names() %>%
  rename(decile_score = decile_score_12, 
         priors_count = priors_count_15)
```

    ## New names:
    ## Rows: 7214 Columns: 53
    ## ── Column specification
    ## ──────────────────────────────────────────────────────── Delimiter: "," chr
    ## (19): name, first, last, sex, age_cat, race, c_case_number, c_charge_de... dbl
    ## (19): id, age, juv_fel_count, decile_score...12, juv_misd_count, juv_ot... lgl
    ## (1): violent_recid dttm (2): c_jail_in, c_jail_out date (12):
    ## compas_screening_date, dob, c_offense_date, c_arrest_date, r_offe...
    ## ℹ Use `spec()` to retrieve the full column specification for this data. ℹ
    ## Specify the column types or set `show_col_types = FALSE` to quiet this message.
    ## • `decile_score` -> `decile_score...12`
    ## • `priors_count` -> `priors_count...15`
    ## • `decile_score` -> `decile_score...40`
    ## • `priors_count` -> `priors_count...49`

``` r
glimpse(compas)
```

    ## Rows: 7,214
    ## Columns: 53
    ## $ id                      <dbl> 1, 3, 4, 5, 6, 7, 8, 9, 10, 13, 14, 15, 16, 18…
    ## $ name                    <chr> "miguel hernandez", "kevon dixon", "ed philo",…
    ## $ first                   <chr> "miguel", "kevon", "ed", "marcu", "bouthy", "m…
    ## $ last                    <chr> "hernandez", "dixon", "philo", "brown", "pierr…
    ## $ compas_screening_date   <date> 2013-08-14, 2013-01-27, 2013-04-14, 2013-01-1…
    ## $ sex                     <chr> "Male", "Male", "Male", "Male", "Male", "Male"…
    ## $ dob                     <date> 1947-04-18, 1982-01-22, 1991-05-14, 1993-01-2…
    ## $ age                     <dbl> 69, 34, 24, 23, 43, 44, 41, 43, 39, 21, 27, 23…
    ## $ age_cat                 <chr> "Greater than 45", "25 - 45", "Less than 25", …
    ## $ race                    <chr> "Other", "African-American", "African-American…
    ## $ juv_fel_count           <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ decile_score            <dbl> 1, 3, 4, 8, 1, 1, 6, 4, 1, 3, 4, 6, 1, 4, 1, 3…
    ## $ juv_misd_count          <dbl> 0, 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ juv_other_count         <dbl> 0, 0, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0…
    ## $ priors_count            <dbl> 0, 0, 4, 1, 2, 0, 14, 3, 0, 1, 0, 3, 0, 0, 1, …
    ## $ days_b_screening_arrest <dbl> -1, -1, -1, NA, NA, 0, -1, -1, -1, 428, -1, 0,…
    ## $ c_jail_in               <dttm> 2013-08-13 06:03:42, 2013-01-26 03:45:27, 201…
    ## $ c_jail_out              <dttm> 2013-08-14 05:41:20, 2013-02-05 05:36:53, 201…
    ## $ c_case_number           <chr> "13011352CF10A", "13001275CF10A", "13005330CF1…
    ## $ c_offense_date          <date> 2013-08-13, 2013-01-26, 2013-04-13, 2013-01-1…
    ## $ c_arrest_date           <date> NA, NA, NA, NA, 2013-01-09, NA, NA, 2013-08-2…
    ## $ c_days_from_compas      <dbl> 1, 1, 1, 1, 76, 0, 1, 1, 1, 308, 1, 0, 0, 1, 4…
    ## $ c_charge_degree         <chr> "F", "F", "F", "F", "F", "M", "F", "F", "M", "…
    ## $ c_charge_desc           <chr> "Aggravated Assault w/Firearm", "Felony Batter…
    ## $ is_recid                <dbl> 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1…
    ## $ r_case_number           <chr> NA, "13009779CF10A", "13011511MM10A", NA, NA, …
    ## $ r_charge_degree         <chr> NA, "(F3)", "(M1)", NA, NA, NA, "(F2)", NA, NA…
    ## $ r_days_from_arrest      <dbl> NA, NA, 0, NA, NA, NA, 0, NA, NA, 0, NA, NA, N…
    ## $ r_offense_date          <date> NA, 2013-07-05, 2013-06-16, NA, NA, NA, 2014-…
    ## $ r_charge_desc           <chr> NA, "Felony Battery (Dom Strang)", "Driving Un…
    ## $ r_jail_in               <date> NA, NA, 2013-06-16, NA, NA, NA, 2014-03-31, N…
    ## $ r_jail_out              <date> NA, NA, 2013-06-16, NA, NA, NA, 2014-04-18, N…
    ## $ violent_recid           <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
    ## $ is_violent_recid        <dbl> 0, 1, 0, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 0…
    ## $ vr_case_number          <chr> NA, "13009779CF10A", NA, NA, NA, NA, NA, NA, N…
    ## $ vr_charge_degree        <chr> NA, "(F3)", NA, NA, NA, NA, NA, NA, NA, "(F2)"…
    ## $ vr_offense_date         <date> NA, 2013-07-05, NA, NA, NA, NA, NA, NA, NA, 2…
    ## $ vr_charge_desc          <chr> NA, "Felony Battery (Dom Strang)", NA, NA, NA,…
    ## $ type_of_assessment      <chr> "Risk of Recidivism", "Risk of Recidivism", "R…
    ## $ decile_score_40         <dbl> 1, 3, 4, 8, 1, 1, 6, 4, 1, 3, 4, 6, 1, 4, 1, 3…
    ## $ score_text              <chr> "Low", "Low", "Low", "High", "Low", "Low", "Me…
    ## $ screening_date          <date> 2013-08-14, 2013-01-27, 2013-04-14, 2013-01-1…
    ## $ v_type_of_assessment    <chr> "Risk of Violence", "Risk of Violence", "Risk …
    ## $ v_decile_score          <dbl> 1, 1, 3, 6, 1, 1, 2, 3, 1, 5, 4, 4, 1, 2, 1, 2…
    ## $ v_score_text            <chr> "Low", "Low", "Low", "Medium", "Low", "Low", "…
    ## $ v_screening_date        <date> 2013-08-14, 2013-01-27, 2013-04-14, 2013-01-1…
    ## $ in_custody              <date> 2014-07-07, 2013-01-26, 2013-06-16, NA, NA, 2…
    ## $ out_custody             <date> 2014-07-14, 2013-02-05, 2013-06-16, NA, NA, 2…
    ## $ priors_count_49         <dbl> 0, 0, 4, 1, 2, 0, 14, 3, 0, 1, 0, 3, 0, 0, 1, …
    ## $ start                   <dbl> 0, 9, 0, 0, 0, 1, 5, 0, 2, 0, 0, 4, 1, 0, 0, 0…
    ## $ end                     <dbl> 327, 159, 63, 1174, 1102, 853, 40, 265, 747, 4…
    ## $ event                   <dbl> 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1…
    ## $ two_year_recid          <dbl> 0, 1, 1, 0, 0, 0, 1, 0, 0, 1, 0, 1, 0, 0, 1, 1…

## Exercise 1

``` r
nrow(compas)
```

    ## [1] 7214

``` r
ncol(compas)
```

    ## [1] 53

The dimensions of the data set are 7214 x 53. There are 7214 rows of
data. Each row in the data set represents a criminal defendant. The
variables in this data set are id, first name, last name, the date they
were compas screened, sex, birthday, age, age range, race, juvenile
felony count, decile score, juvenile misdemeanor count, juvenile other
count, priors count, how many days prior to screening the defendant was
arrested, the date they went into jail for the compas screening, the
date they left jail for the compas screening, their case number for the
compas screening, arrest date for the compas screening, days from
compas, the degree of their charge for the compas screening, a
description of their charge for the compas screening, is recidivism,
their case number for the recidivism case, the degree of their charge
for the recidivism case, the days from arrest for the recidivism case,
their offense date of the recidivism case, a description of their charge
for the recidivism case, the date they entered jail for the recidivism
case, the date they left jail for the recidivism case. There are also
variables for violent recidivism, if violent recidivism occurred and
coded, violent recidivism case number, violent recidivism charge degree,
violent recidivism offense date, description of violent recidivism
charge. There are variables for the type of assessment, the decile
score, an interpretation of the decile score, date of screening,
violence type assessment, violence decile score, violence score in text,
violence screening date, date in custody, date left custody, prior
counts, start, end, eent, and two-year recidivism.

## Exercise 2

``` r
library(dplyr)
distinct(compas, name)
```

    ## # A tibble: 7,158 × 1
    ##    name              
    ##    <chr>             
    ##  1 miguel hernandez  
    ##  2 kevon dixon       
    ##  3 ed philo          
    ##  4 marcu brown       
    ##  5 bouthy pierrelouis
    ##  6 marsha miles      
    ##  7 edward riddle     
    ##  8 steven stewart    
    ##  9 elizabeth thieme  
    ## 10 bo bradac         
    ## # ℹ 7,148 more rows

There are 7,158 unique names in the compas data set. It may be that
there are fewer unique defendants in the data set because there are
defendants who have committed another crime and were screened again, or
that some defendants could share a name.

## Exercise 3

``` r
library(ggplot2)
ggplot(compas, aes(x=decile_score)) + 
  geom_histogram(binwidth = .5) + 
  scale_x_continuous(breaks = seq(0,13,1))
```

![](lab-09_files/figure-gfm/compas_distribution-1.png)<!-- -->

The distribution is a unimodal and positively skewed (right-skewed).The
most common decile_score is 1.

## Exercise 4

``` r
library(ggplot2)
ggplot(compas, aes(x=race)) + 
  geom_bar(aes(x=forcats::fct_infreq(race))) + 
  theme(axis.text.x = element_text(angle = 20, vjust = .5))
```

![](lab-09_files/figure-gfm/demographic_distribution_race-1.png)<!-- -->

``` r
library(ggplot2)
ggplot(compas, aes(x=sex)) + 
  geom_bar(aes(x=forcats::fct_infreq(sex)))
```

![](lab-09_files/figure-gfm/demographic_distribution_sex-1.png)<!-- -->

``` r
library(ggplot2)
ggplot(compas, aes(x=age_cat)) +
  geom_bar()
```

![](lab-09_files/figure-gfm/demographic_distribution_age_cat-1.png)<!-- -->

``` r
library(ggplot2)
library(tidyverse)

agecat_compas <- subset(compas, select=c("age_cat"))

agecat_compas$age_cat <- factor(agecat_compas$age_cat, levels = c ("Less than 25", "25 - 45", "Greater than 45"))

ggplot(agecat_compas, aes(x=age_cat)) + 
  geom_bar() + 
  labs(x="Age Range", y = "Count")
```

![](lab-09_files/figure-gfm/demographic_distribution_age_cat_reorder-1.png)<!-- -->

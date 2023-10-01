P8105 HW2
================

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.3     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.3     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

# Read in some data

``` r
pols_df = read_csv("HW2data/pols-month.csv")
```

    ## Rows: 822 Columns: 9
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl  (8): prez_gop, gov_gop, sen_gop, rep_gop, prez_dem, gov_dem, sen_dem, r...
    ## date (1): mon
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pols_df = janitor::clean_names(pols_df)
```

``` r
unemployment_df = read_csv("HW2data/unemployment.csv")
```

    ## Rows: 68 Columns: 13
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## dbl (13): Year, Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sep, Oct, Nov, Dec
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
unemployment_df = janitor::clean_names(unemployment_df)
```

``` r
snp_df = read_csv("HW2data/snp.csv")
```

    ## Rows: 787 Columns: 2
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): date
    ## dbl (1): close
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
snp_df = janitor::clean_names(snp_df)
```

\##Date cleaning

``` r
pols_cleaned <- pols_df %>%
  separate(mon, into = c('year', 'month', 'day'), sep = '-')
```

``` r
pols_cleaned = pols_cleaned %>%
  mutate(month = month.name[as.integer(month)])
```

``` r
pols_cleaned$president <-   
   ifelse(pols_cleaned$prez_gop == 1, "gop",
      ifelse(pols_cleaned$prez_dem == 1, "dem", NA))
```

``` r
pols_cleaned = pols_cleaned %>%
  select(-day,-prez_dem,-prez_gop)
```

\##checking if data is cleaned

``` r
head(pols_cleaned)
```

    ## # A tibble: 6 × 9
    ##   year  month    gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president
    ##   <chr> <chr>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>    
    ## 1 1947  January       23      51     253      23      45     198 dem      
    ## 2 1947  February      23      51     253      23      45     198 dem      
    ## 3 1947  March         23      51     253      23      45     198 dem      
    ## 4 1947  April         23      51     253      23      45     198 dem      
    ## 5 1947  May           23      51     253      23      45     198 dem      
    ## 6 1947  June          23      51     253      23      45     198 dem

## Cleaning snp.csv

``` r
snp_cleaned <- snp_df %>%
  separate(date, into = c("month", "day", "year"), sep = "/") %>%
  mutate(month = month.abb[as.numeric(month)]) %>%
   mutate(year = as.numeric(year),
         year = ifelse(year >= 50, 1900 + year, 2000 + year)) %>%
  select (year, month, everything()) %>%
  select(-day)
```

\##check snp data

``` r
head(snp_cleaned)
```

    ## # A tibble: 6 × 3
    ##    year month close
    ##   <dbl> <chr> <dbl>
    ## 1  2015 Jul   2080.
    ## 2  2015 Jun   2063.
    ## 3  2015 May   2107.
    ## 4  2015 Apr   2086.
    ## 5  2015 Mar   2068.
    ## 6  2015 Feb   2104.

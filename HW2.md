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
pols_cleaned = pols_cleaned %>%
  select(-day,-prez_dem,-prez_gop)
```

\##checking if data is cleaned

``` r
head(pols_cleaned)
```

    ## # A tibble: 6 × 8
    ##   year  month    gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem
    ##   <chr> <chr>      <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>
    ## 1 1947  January       23      51     253      23      45     198
    ## 2 1947  February      23      51     253      23      45     198
    ## 3 1947  March         23      51     253      23      45     198
    ## 4 1947  April         23      51     253      23      45     198
    ## 5 1947  May           23      51     253      23      45     198
    ## 6 1947  June          23      51     253      23      45     198

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
pols_df <- pols_df %>%
  separate(mon, into = c('year', 'month', 'day'), sep = '-')
```

``` r
pols_df = pols_df %>%
  mutate(month = month.name[as.integer(month)])
```

``` r
pols_df$president <-   
   ifelse(pols_df$prez_gop == 1, "gop",
      ifelse(pols_df$prez_dem == 1, "dem", NA))
```

``` r
pols_df = pols_df %>%
  select(-day,-prez_dem,-prez_gop)
```

\##checking if data is cleaned

``` r
head(pols_df)
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
snp_df <- snp_df %>%
  separate(date, into = c("month", "day", "year"), sep = "/") %>%
  mutate(month = month.abb[as.numeric(month)]) %>%
   mutate(year = as.numeric(year),
         year = ifelse(year >= 50, 1900 + year, 2000 + year)) %>%
  select (year, month, everything()) %>%
  select(-day)
```

\##check snp data

``` r
head(snp_df)
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

## tidy unemployment data

``` r
unemployment_df <- unemployment_df %>%
  pivot_longer(cols = jan:dec, names_to = "month", values_to = "unemployment_rate")
```

``` r
head(unemployment_df)
```

    ## # A tibble: 6 × 3
    ##    year month unemployment_rate
    ##   <dbl> <chr>             <dbl>
    ## 1  1948 jan                 3.4
    ## 2  1948 feb                 3.8
    ## 3  1948 mar                 4  
    ## 4  1948 apr                 3.9
    ## 5  1948 may                 3.5
    ## 6  1948 jun                 3.6

\##Merging dataset

``` r
pols_df$month <- month.abb[match(pols_df$month, month.name)]

pols_df$year <- as.numeric(pols_df$year)
```

``` r
snp_df$month <- tools::toTitleCase(tolower(snp_df$month))
```

``` r
unemployment_df$month <- tools::toTitleCase(unemployment_df$month)
```

``` r
merged_data <- left_join(pols_df, snp_df, by = c("year", "month"))
```

``` r
final_merged_data <- left_join(merged_data, unemployment_df, by = c("year", "month"))
```

``` r
print(final_merged_data)
```

    ## # A tibble: 822 × 11
    ##     year month gov_gop sen_gop rep_gop gov_dem sen_dem rep_dem president close
    ##    <dbl> <chr>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl>   <dbl> <chr>     <dbl>
    ##  1  1947 Jan        23      51     253      23      45     198 dem          NA
    ##  2  1947 Feb        23      51     253      23      45     198 dem          NA
    ##  3  1947 Mar        23      51     253      23      45     198 dem          NA
    ##  4  1947 Apr        23      51     253      23      45     198 dem          NA
    ##  5  1947 May        23      51     253      23      45     198 dem          NA
    ##  6  1947 Jun        23      51     253      23      45     198 dem          NA
    ##  7  1947 Jul        23      51     253      23      45     198 dem          NA
    ##  8  1947 Aug        23      51     253      23      45     198 dem          NA
    ##  9  1947 Sep        23      51     253      23      45     198 dem          NA
    ## 10  1947 Oct        23      51     253      23      45     198 dem          NA
    ## # ℹ 812 more rows
    ## # ℹ 1 more variable: unemployment_rate <dbl>

\##The final_merge_data include 822 rows and 11 column, comprises three
primary sources of data. Pols_df is political data about the monthly
representation of Republican and Democratic officials at the levels of
governor, senator and representative. the snp_df data is the financial
data about the monthly closing value for S&P stock index, using as a
benchmark for market performance. the unemployment_df presents the
economic monthly umenemployment rates. the final_merged_data spans from
1947 to 2015, which is a wide range of information. The variables are
year, month, gov_gop, sen_gop, rep_gop, gov_dem, sen_dem, rep_dem,
president, close and unemployment.

\##Problem 2

``` r
library(readxl)
library(dplyr)
```

``` r
mrtrash_df = 
  read_excel("HW2data/202309 Trash Wheel Collection Data.xlsx", sheet="Mr. Trash Wheel", range =cell_cols("A:N")) |> 
  janitor::clean_names() 
```

``` r
mrtrash_df <- mrtrash_df %>%
separate(date, into = c("year_2", "month_num","day"), convert = TRUE) |>
  select(-year_2) |>
  mutate(homes_powered = weight_tons * 500/30,
         year = as.numeric(year)) |>
  mutate (wheel ="mtw") |>
  filter(dumpster !="NA")
```

## Professor Trash Wheel

``` r
prof_trash_df <- read_excel("HW2data/202309 Trash Wheel Collection Data.xlsx", sheet="Professor Trash Wheel", range = cell_cols("A:M")) %>% 
  janitor::clean_names() %>%
  separate(date, into = c("year_2", "month_num","day"), convert = TRUE) %>% 
  select(-year_2) %>% 
  mutate(
    homes_powered = weight_tons * 500/30,
    year = as.numeric(year),
    wheel = "ptw"
  ) %>%
  filter(dumpster != "NA")
```

## Gwynnda data

``` r
gwynnda_df <- read_excel("HW2data/202309 Trash Wheel Collection Data.xlsx", sheet="Gwynnda Trash Wheel", range = cell_cols("A:L")) %>% 
  janitor::clean_names() %>%
  separate(date, into = c("year_2", "month_num","day"), convert = TRUE) %>% 
  select(-year_2) %>% 
  mutate(
    homes_powered = weight_tons * 500/30,
    year = as.numeric(year),
    wheel = "gwynnda"
  ) %>%
  filter(dumpster != "NA")
```

\##Now, combine the data

``` r
combined_df <- bind_rows(mrtrash_df, prof_trash_df, gwynnda_df)
```

``` r
sum(pull(prof_trash_df, weight_tons))
```

    ## [1] 216.26

The total weight of trash collected by Professor Trash Wheel was 216.26
tons.

``` r
total_cigarette_butts_july_2021 <- gwynnda_df %>%
  filter(wheel == "gwynnda", year == 2021, month == "July") %>%
  summarise(total_cigarette_butts = sum(cigarette_butts, na.rm = TRUE)) %>%
  pull(total_cigarette_butts)
```

The total number of cigarette butts collected by Gwynnda in July of 2021
is 1.63^{4}.

\##Summary description of the data The Trash Wheel dataset is a
comprehensive record of trash collection spanning multiple entries, with
a total of 845 observations. Each observation in the dataset provides
details about the specific `dumpster used`, the `month`, `year`, and
specific `day` of the collection, as well as the `month_num` for easier
chronological sorting. In terms of the collected trash’s
characteristics, the dataset offers insights into the `weight_tons` of
the trash and its `volume_cubic_yards`. Additionally, to give a more
granular view of the types of trash collected, the dataset provides
specific counts for items like `plastic_bottles`, `polystyrene`,
`cigarette_butts`, `glass_bottles`, `plastic_bags`, and `wrappers`.
Interestingly, there’s even a count for `sports_balls`. To provide an
environmental perspective, a derived metric, `homes_powered`, estimates
the number of homes that could be powered from the energy saved by the
trash collection. Lastly, the `wheel` variable indicates which specific
Trash Wheel device was responsible for the collection. This dataset
offers a thorough overview of the efforts and impacts of the Trash Wheel
initiative over time.

## Problem 3

``` r
baseline_df = read.csv("data_mci/MCI_baseline.csv",  skip=1) |>
  janitor::clean_names()
```

``` r
baseline_df <- baseline_df |>
 mutate(sex = case_match(sex,
      1 ~ "male",
      0 ~ "female"),
    apoe4 = case_match( apoe4,
      1 ~ "carrier",
      0 ~ "non-carrier")
  ) |> 
  filter(age_at_onset == "." | age_at_onset > current_age)
```

Summary: There are 479 participants recruited.

93 participants developed MCI during this study.

The average baseline age is 65.

3.  The proportion of women in the study are APOE4 carriers are 30%.

\##amyloid data

``` r
amyloid_df = 
  read_csv("data_mci/mci_amyloid.csv",skip = 1) |> 
  janitor::clean_names() |>
rename(id = study_id)
```

    ## Rows: 487 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (5): Baseline, Time 2, Time 4, Time 6, Time 8
    ## dbl (1): Study ID
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
amyloid_new = amyloid_df |> pivot_longer(
  baseline:time_8,
  names_to = "visit",
  values_to = "amyloid_ratio"
)
```

``` r
amyloid_merge = 
  full_join(baseline_df, amyloid_df, by = c("id"))
```

``` r
problem_3_combined_df <- baseline_df %>%
  inner_join(amyloid_new, by = "id")
```

``` r
write.csv(problem_3_combined_df, "data_mci//combined_data_problem3.csv")
```

In order to proceed merge file, id and study_id have to be matching, so
study_id is renamed to id.

There are 487 participants in the dataset.

MCI is recorded at baseline time of 2,4,6 and 8.

After merging the data, there are 2355 participants.

Out of these dataset, There are 16 participants in the amyloid set,
however,does not exist in the baseline_df set. - There are 8
participants in the baseline set, however, not in the amyloid set.

In the final merge set, there are 2355 rows and 8 columns.

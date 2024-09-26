class6 afterclass note
================
Yingyu Cui
2024-09-26

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.4     ✔ readr     2.1.5
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.1
    ## ✔ ggplot2   3.5.1     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.3     ✔ tidyr     1.3.1
    ## ✔ purrr     1.0.2     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
library(haven)
```

load the sas file and clean names

``` r
pulse_df = read_sas("./data_import_examples/public_pulse_data.sas7bdat") |> 
  janitor::clean_names()
```

``` r
pulse_df1 = read_sas("./data_import_examples/public_pulse_data.sas7bdat") |> 
  janitor::clean_names()
```

thin the data frame

``` r
pulse_df = pivot_longer(pulse_df1, 
                        bdi_score_bl:bdi_score_12m,
                        names_to = "visit",
                        names_prefix = "bdi_score_",
                        values_to = "bdi")
pulse_df
```

    ## # A tibble: 4,348 × 5
    ##       id   age sex   visit   bdi
    ##    <dbl> <dbl> <chr> <chr> <dbl>
    ##  1 10003  48.0 male  bl        7
    ##  2 10003  48.0 male  01m       1
    ##  3 10003  48.0 male  06m       2
    ##  4 10003  48.0 male  12m       0
    ##  5 10015  72.5 male  bl        6
    ##  6 10015  72.5 male  01m      NA
    ##  7 10015  72.5 male  06m      NA
    ##  8 10015  72.5 male  12m      NA
    ##  9 10022  58.5 male  bl       14
    ## 10 10022  58.5 male  01m       3
    ## # ℹ 4,338 more rows

replace items in the visit column

``` r
pulse_df1 = read_sas("./data_import_examples/public_pulse_data.sas7bdat") |> 
  janitor::clean_names() |>
  pivot_longer(
    bdi_score_bl:bdi_score_12m,
    names_to = "visit",
    names_prefix = "bdi_score_",
    values_to = "bdi") |>
  mutate(visit = replace(visit, visit == "bl", "00m"),
         visit = factor(visit))
pulse_df1
```

    ## # A tibble: 4,348 × 5
    ##       id   age sex   visit   bdi
    ##    <dbl> <dbl> <chr> <fct> <dbl>
    ##  1 10003  48.0 male  00m       7
    ##  2 10003  48.0 male  01m       1
    ##  3 10003  48.0 male  06m       2
    ##  4 10003  48.0 male  12m       0
    ##  5 10015  72.5 male  00m       6
    ##  6 10015  72.5 male  01m      NA
    ##  7 10015  72.5 male  06m      NA
    ##  8 10015  72.5 male  12m      NA
    ##  9 10022  58.5 male  00m      14
    ## 10 10022  58.5 male  01m       3
    ## # ℹ 4,338 more rows

assessment 1:

``` r
litter_df1 = read_csv("./data_import_examples/FAS_litters.csv") |> 
  janitor::clean_names() |> 
  select(litter_number, ends_with("weight")) |> 
  pivot_longer(gd0_weight:gd18_weight,
               names_to = "gd",
               values_to = "weight"
               ) |> 
  mutate(gd = 
           case_match(gd,
                      "gd0_weight"  ~ 0,
                      "gd18_weight" ~ 18
                      )
         )
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (4): Group, Litter Number, GD0 weight, GD18 weight
    ## dbl (4): GD of Birth, Pups born alive, Pups dead @ birth, Pups survive
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litter_df1
```

    ## # A tibble: 98 × 3
    ##    litter_number    gd weight
    ##    <chr>         <dbl> <chr> 
    ##  1 #85               0 19.7  
    ##  2 #85              18 34.7  
    ##  3 #1/2/95/2         0 27    
    ##  4 #1/2/95/2        18 42    
    ##  5 #5/5/3/83/3-3     0 26    
    ##  6 #5/5/3/83/3-3    18 41.4  
    ##  7 #5/4/2/95/2       0 28.5  
    ##  8 #5/4/2/95/2      18 44.1  
    ##  9 #4/2/95/3-3       0 <NA>  
    ## 10 #4/2/95/3-3      18 <NA>  
    ## # ℹ 88 more rows

# Pivot_wider

``` r
analysis_result = 
  tibble(
    group = c("treatment", "treatment", "placebo", "placebo"),
    time = c("pre", "post", "pre", "post"),
    mean = c(4, 8, 3.5, 4)
  )
analysis_result
```

    ## # A tibble: 4 × 3
    ##   group     time   mean
    ##   <chr>     <chr> <dbl>
    ## 1 treatment pre     4  
    ## 2 treatment post    8  
    ## 3 placebo   pre     3.5
    ## 4 placebo   post    4

now we transform this data frame

``` r
analysis_result |> 
  pivot_wider(names_from = "time",
              values_from = "mean")
```

    ## # A tibble: 2 × 3
    ##   group       pre  post
    ##   <chr>     <dbl> <dbl>
    ## 1 treatment   4       8
    ## 2 placebo     3.5     4

a nicer table for reading

``` r
knitr::kable(analysis_result)
```

| group     | time | mean |
|:----------|:-----|-----:|
| treatment | pre  |  4.0 |
| treatment | post |  8.0 |
| placebo   | pre  |  3.5 |
| placebo   | post |  4.0 |

# Binding rows

import three charts

``` r
lotr_df1 = readxl::read_excel("./data_import_examples/LotR_Words.xlsx", range = "B3:D6") |> 
  mutate(movie = "fellowship_ring")
lotr_df2 = readxl::read_excel("./data_import_examples/LotR_Words.xlsx", range = "F3:H6") |>
  mutate(movie = "two_towers")
lotr_df3 = readxl::read_excel("./data_import_examples/LotR_Words.xlsx", range = "J3:L6") |>
  mutate(movie = "return_king")
```

bind the rows

``` r
lotr_tidy = 
  bind_rows(lotr_df1, lotr_df2, lotr_df3) |> 
  janitor::clean_names() |> 
  pivot_longer(
    female:male,
    names_to = "gender", 
    values_to = "words") |> 
  mutate(race = str_to_lower(race)) |> 
  select(movie, everything())
lotr_tidy
```

    ## # A tibble: 18 × 4
    ##    movie           race   gender words
    ##    <chr>           <chr>  <chr>  <dbl>
    ##  1 fellowship_ring elf    female  1229
    ##  2 fellowship_ring elf    male     971
    ##  3 fellowship_ring hobbit female    14
    ##  4 fellowship_ring hobbit male    3644
    ##  5 fellowship_ring man    female     0
    ##  6 fellowship_ring man    male    1995
    ##  7 two_towers      elf    female   331
    ##  8 two_towers      elf    male     513
    ##  9 two_towers      hobbit female     0
    ## 10 two_towers      hobbit male    2463
    ## 11 two_towers      man    female   401
    ## 12 two_towers      man    male    3589
    ## 13 return_king     elf    female   183
    ## 14 return_king     elf    male     510
    ## 15 return_king     hobbit female     2
    ## 16 return_king     hobbit male    2673
    ## 17 return_king     man    female   268
    ## 18 return_king     man    male    2459

# Join datasets

``` r
pup_df = 
  read_csv("./data_import_examples/FAS_pups.csv",
    na = c("NA", "", ".")) |>
  janitor::clean_names() |>
  mutate(
    sex = case_match(sex, 
                     1 ~ "male", 
                     2 ~ "female"),
    sex = as.factor(sex)) 
```

    ## Rows: 313 Columns: 6
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (1): Litter Number
    ## dbl (5): Sex, PD ears, PD eyes, PD pivot, PD walk
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
pup_df
```

    ## # A tibble: 313 × 6
    ##    litter_number sex   pd_ears pd_eyes pd_pivot pd_walk
    ##    <chr>         <fct>   <dbl>   <dbl>    <dbl>   <dbl>
    ##  1 #85           male        4      13        7      11
    ##  2 #85           male        4      13        7      12
    ##  3 #1/2/95/2     male        5      13        7       9
    ##  4 #1/2/95/2     male        5      13        8      10
    ##  5 #5/5/3/83/3-3 male        5      13        8      10
    ##  6 #5/5/3/83/3-3 male        5      14        6       9
    ##  7 #5/4/2/95/2   male       NA      14        5       9
    ##  8 #4/2/95/3-3   male        4      13        6       8
    ##  9 #4/2/95/3-3   male        4      13        7       9
    ## 10 #2/2/95/3-2   male        4      NA        8      10
    ## # ℹ 303 more rows

``` r
litter_df = 
  read_csv("./data_import_examples/FAS_litters.csv", na = c("NA", ".", "")) |> 
  janitor::clean_names() |>
  separate(group, into = c("dose", "day_of_tx"), sep = 3) |>
  relocate(litter_number) |>
   mutate(
    wt_gain = gd18_weight - gd0_weight,
    dose = str_to_lower(dose))
```

    ## Rows: 49 Columns: 8
    ## ── Column specification ────────────────────────────────────────────────────────
    ## Delimiter: ","
    ## chr (2): Group, Litter Number
    ## dbl (6): GD0 weight, GD18 weight, GD of Birth, Pups born alive, Pups dead @ ...
    ## 
    ## ℹ Use `spec()` to retrieve the full column specification for this data.
    ## ℹ Specify the column types or set `show_col_types = FALSE` to quiet this message.

``` r
litter_df
```

    ## # A tibble: 49 × 10
    ##    litter_number   dose  day_of_tx gd0_weight gd18_weight gd_of_birth
    ##    <chr>           <chr> <chr>          <dbl>       <dbl>       <dbl>
    ##  1 #85             con   7               19.7        34.7          20
    ##  2 #1/2/95/2       con   7               27          42            19
    ##  3 #5/5/3/83/3-3   con   7               26          41.4          19
    ##  4 #5/4/2/95/2     con   7               28.5        44.1          19
    ##  5 #4/2/95/3-3     con   7               NA          NA            20
    ##  6 #2/2/95/3-2     con   7               NA          NA            20
    ##  7 #1/5/3/83/3-3/2 con   7               NA          NA            20
    ##  8 #3/83/3-3       con   8               NA          NA            20
    ##  9 #2/95/3         con   8               NA          NA            20
    ## 10 #3/5/2/2/95     con   8               28.5        NA            20
    ## # ℹ 39 more rows
    ## # ℹ 4 more variables: pups_born_alive <dbl>, pups_dead_birth <dbl>,
    ## #   pups_survive <dbl>, wt_gain <dbl>

join the two datasets

``` r
fas_df = 
  left_join(pup_df, litter_df, by = "litter_number")
```

Assessment 2:

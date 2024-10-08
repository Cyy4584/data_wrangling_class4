data_manipulation_class5
================
Yingyu Cui
2024-09-23

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

# Import the litter data

``` r
litter_df = read_csv("./data_import_examples/FAS_litters.csv")
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
litter_df = janitor::clean_names(litter_df)
```

# ‘Select’ some columns but not others

different usages of ‘select’ function: - select columns by name

``` r
select(litter_df, group, litter_number)
```

    ## # A tibble: 49 × 2
    ##    group litter_number  
    ##    <chr> <chr>          
    ##  1 Con7  #85            
    ##  2 Con7  #1/2/95/2      
    ##  3 Con7  #5/5/3/83/3-3  
    ##  4 Con7  #5/4/2/95/2    
    ##  5 Con7  #4/2/95/3-3    
    ##  6 Con7  #2/2/95/3-2    
    ##  7 Con7  #1/5/3/83/3-3/2
    ##  8 Con8  #3/83/3-3      
    ##  9 Con8  #2/95/3        
    ## 10 Con8  #3/5/2/2/95    
    ## # ℹ 39 more rows

``` r
select(litter_df, -group)
```

    ## # A tibble: 49 × 7
    ##    litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr>           <chr>      <chr>             <dbl>           <dbl>
    ##  1 #85             19.7       34.7                 20               3
    ##  2 #1/2/95/2       27         42                   19               8
    ##  3 #5/5/3/83/3-3   26         41.4                 19               6
    ##  4 #5/4/2/95/2     28.5       44.1                 19               5
    ##  5 #4/2/95/3-3     <NA>       <NA>                 20               6
    ##  6 #2/2/95/3-2     <NA>       <NA>                 20               6
    ##  7 #1/5/3/83/3-3/2 <NA>       <NA>                 20               9
    ##  8 #3/83/3-3       <NA>       <NA>                 20               9
    ##  9 #2/95/3         <NA>       <NA>                 20               8
    ## 10 #3/5/2/2/95     28.5       <NA>                 20               8
    ## # ℹ 39 more rows
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

``` r
select(litter_df, group:gd18_weight)
```

    ## # A tibble: 49 × 4
    ##    group litter_number   gd0_weight gd18_weight
    ##    <chr> <chr>           <chr>      <chr>      
    ##  1 Con7  #85             19.7       34.7       
    ##  2 Con7  #1/2/95/2       27         42         
    ##  3 Con7  #5/5/3/83/3-3   26         41.4       
    ##  4 Con7  #5/4/2/95/2     28.5       44.1       
    ##  5 Con7  #4/2/95/3-3     <NA>       <NA>       
    ##  6 Con7  #2/2/95/3-2     <NA>       <NA>       
    ##  7 Con7  #1/5/3/83/3-3/2 <NA>       <NA>       
    ##  8 Con8  #3/83/3-3       <NA>       <NA>       
    ##  9 Con8  #2/95/3         <NA>       <NA>       
    ## 10 Con8  #3/5/2/2/95     28.5       <NA>       
    ## # ℹ 39 more rows

- Rename columns: one change and select, the other one change but keep
  others.

``` r
select(litter_df, GROUP = group, LITTER_NUMBER = litter_number)
```

    ## # A tibble: 49 × 2
    ##    GROUP LITTER_NUMBER  
    ##    <chr> <chr>          
    ##  1 Con7  #85            
    ##  2 Con7  #1/2/95/2      
    ##  3 Con7  #5/5/3/83/3-3  
    ##  4 Con7  #5/4/2/95/2    
    ##  5 Con7  #4/2/95/3-3    
    ##  6 Con7  #2/2/95/3-2    
    ##  7 Con7  #1/5/3/83/3-3/2
    ##  8 Con8  #3/83/3-3      
    ##  9 Con8  #2/95/3        
    ## 10 Con8  #3/5/2/2/95    
    ## # ℹ 39 more rows

``` r
rename(litter_df, GROUP = group, LITTER_NUMBER = litter_number)
```

    ## # A tibble: 49 × 8
    ##    GROUP LITTER_NUMBER   gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>           <chr>      <chr>             <dbl>           <dbl>
    ##  1 Con7  #85             19.7       34.7                 20               3
    ##  2 Con7  #1/2/95/2       27         42                   19               8
    ##  3 Con7  #5/5/3/83/3-3   26         41.4                 19               6
    ##  4 Con7  #5/4/2/95/2     28.5       44.1                 19               5
    ##  5 Con7  #4/2/95/3-3     <NA>       <NA>                 20               6
    ##  6 Con7  #2/2/95/3-2     <NA>       <NA>                 20               6
    ##  7 Con7  #1/5/3/83/3-3/2 <NA>       <NA>                 20               9
    ##  8 Con8  #3/83/3-3       <NA>       <NA>                 20               9
    ##  9 Con8  #2/95/3         <NA>       <NA>                 20               8
    ## 10 Con8  #3/5/2/2/95     28.5       <NA>                 20               8
    ## # ℹ 39 more rows
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

- select columns by condition / select helpers the last two examples
  represent the same function but the different expressions.

``` r
select(litter_df, starts_with("gd"))
```

    ## # A tibble: 49 × 3
    ##    gd0_weight gd18_weight gd_of_birth
    ##    <chr>      <chr>             <dbl>
    ##  1 19.7       34.7                 20
    ##  2 27         42                   19
    ##  3 26         41.4                 19
    ##  4 28.5       44.1                 19
    ##  5 <NA>       <NA>                 20
    ##  6 <NA>       <NA>                 20
    ##  7 <NA>       <NA>                 20
    ##  8 <NA>       <NA>                 20
    ##  9 <NA>       <NA>                 20
    ## 10 28.5       <NA>                 20
    ## # ℹ 39 more rows

``` r
select(litter_df, litter_number, everything())
```

    ## # A tibble: 49 × 8
    ##    litter_number   group gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr>           <chr> <chr>      <chr>             <dbl>           <dbl>
    ##  1 #85             Con7  19.7       34.7                 20               3
    ##  2 #1/2/95/2       Con7  27         42                   19               8
    ##  3 #5/5/3/83/3-3   Con7  26         41.4                 19               6
    ##  4 #5/4/2/95/2     Con7  28.5       44.1                 19               5
    ##  5 #4/2/95/3-3     Con7  <NA>       <NA>                 20               6
    ##  6 #2/2/95/3-2     Con7  <NA>       <NA>                 20               6
    ##  7 #1/5/3/83/3-3/2 Con7  <NA>       <NA>                 20               9
    ##  8 #3/83/3-3       Con8  <NA>       <NA>                 20               9
    ##  9 #2/95/3         Con8  <NA>       <NA>                 20               8
    ## 10 #3/5/2/2/95     Con8  28.5       <NA>                 20               8
    ## # ℹ 39 more rows
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

``` r
relocate(litter_df, litter_number)
```

    ## # A tibble: 49 × 8
    ##    litter_number   group gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr>           <chr> <chr>      <chr>             <dbl>           <dbl>
    ##  1 #85             Con7  19.7       34.7                 20               3
    ##  2 #1/2/95/2       Con7  27         42                   19               8
    ##  3 #5/5/3/83/3-3   Con7  26         41.4                 19               6
    ##  4 #5/4/2/95/2     Con7  28.5       44.1                 19               5
    ##  5 #4/2/95/3-3     Con7  <NA>       <NA>                 20               6
    ##  6 #2/2/95/3-2     Con7  <NA>       <NA>                 20               6
    ##  7 #1/5/3/83/3-3/2 Con7  <NA>       <NA>                 20               9
    ##  8 #3/83/3-3       Con8  <NA>       <NA>                 20               9
    ##  9 #2/95/3         Con8  <NA>       <NA>                 20               8
    ## 10 #3/5/2/2/95     Con8  28.5       <NA>                 20               8
    ## # ℹ 39 more rows
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

# ‘Filter’ function

select is on columns, filter is on rows. when we want to see whether a
condition is valid, we use “==”, instead of “=”, which is used to assign
values. the last one repreesents choosing the subjects that meet either
of the two conditions but not both.

``` r
filter(litter_df, gd_of_birth == 20)
```

    ## # A tibble: 32 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>           <chr>      <chr>             <dbl>           <dbl>
    ##  1 Con7  #85             19.7       34.7                 20               3
    ##  2 Con7  #4/2/95/3-3     <NA>       <NA>                 20               6
    ##  3 Con7  #2/2/95/3-2     <NA>       <NA>                 20               6
    ##  4 Con7  #1/5/3/83/3-3/2 <NA>       <NA>                 20               9
    ##  5 Con8  #3/83/3-3       <NA>       <NA>                 20               9
    ##  6 Con8  #2/95/3         <NA>       <NA>                 20               8
    ##  7 Con8  #3/5/2/2/95     28.5       <NA>                 20               8
    ##  8 Con8  #1/6/2/2/95-2   <NA>       <NA>                 20               7
    ##  9 Con8  #3/5/3/83/3-3-2 <NA>       <NA>                 20               8
    ## 10 Con8  #3/6/2/2/95-3   <NA>       <NA>                 20               7
    ## # ℹ 22 more rows
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

``` r
filter(litter_df, gd_of_birth != 20)
```

    ## # A tibble: 17 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>         <chr>      <chr>             <dbl>           <dbl>
    ##  1 Con7  #1/2/95/2     27         42                   19               8
    ##  2 Con7  #5/5/3/83/3-3 26         41.4                 19               6
    ##  3 Con7  #5/4/2/95/2   28.5       44.1                 19               5
    ##  4 Con8  #5/4/3/83/3   28         <NA>                 19               9
    ##  5 Con8  #2/2/95/2     <NA>       <NA>                 19               5
    ##  6 Mod7  #59           17         33.4                 19               8
    ##  7 Mod7  #103          21.4       42.1                 19               9
    ##  8 Mod7  #1/82/3-2     <NA>       <NA>                 19               6
    ##  9 Mod7  #3/83/3-2     <NA>       <NA>                 19               8
    ## 10 Mod7  #4/2/95/2     23.5       <NA>                 19               9
    ## 11 Mod7  #5/3/83/5-2   22.6       37                   19               5
    ## 12 Mod7  #94/2         24.4       42.9                 19               7
    ## 13 Mod7  #62           19.5       35.9                 19               7
    ## 14 Low7  #112          23.9       40.5                 19               6
    ## 15 Mod8  #5/93/2       .          .                    19               8
    ## 16 Mod8  #7/110/3-2    27.5       46                   19               8
    ## 17 Low8  #79           25.4       43.8                 19               8
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

``` r
filter(litter_df, !(gd_of_birth == 20))
```

    ## # A tibble: 17 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>         <chr>      <chr>             <dbl>           <dbl>
    ##  1 Con7  #1/2/95/2     27         42                   19               8
    ##  2 Con7  #5/5/3/83/3-3 26         41.4                 19               6
    ##  3 Con7  #5/4/2/95/2   28.5       44.1                 19               5
    ##  4 Con8  #5/4/3/83/3   28         <NA>                 19               9
    ##  5 Con8  #2/2/95/2     <NA>       <NA>                 19               5
    ##  6 Mod7  #59           17         33.4                 19               8
    ##  7 Mod7  #103          21.4       42.1                 19               9
    ##  8 Mod7  #1/82/3-2     <NA>       <NA>                 19               6
    ##  9 Mod7  #3/83/3-2     <NA>       <NA>                 19               8
    ## 10 Mod7  #4/2/95/2     23.5       <NA>                 19               9
    ## 11 Mod7  #5/3/83/5-2   22.6       37                   19               5
    ## 12 Mod7  #94/2         24.4       42.9                 19               7
    ## 13 Mod7  #62           19.5       35.9                 19               7
    ## 14 Low7  #112          23.9       40.5                 19               6
    ## 15 Mod8  #5/93/2       .          .                    19               8
    ## 16 Mod8  #7/110/3-2    27.5       46                   19               8
    ## 17 Low8  #79           25.4       43.8                 19               8
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

``` r
filter(litter_df, gd_of_birth <= 20)
```

    ## # A tibble: 49 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>           <chr>      <chr>             <dbl>           <dbl>
    ##  1 Con7  #85             19.7       34.7                 20               3
    ##  2 Con7  #1/2/95/2       27         42                   19               8
    ##  3 Con7  #5/5/3/83/3-3   26         41.4                 19               6
    ##  4 Con7  #5/4/2/95/2     28.5       44.1                 19               5
    ##  5 Con7  #4/2/95/3-3     <NA>       <NA>                 20               6
    ##  6 Con7  #2/2/95/3-2     <NA>       <NA>                 20               6
    ##  7 Con7  #1/5/3/83/3-3/2 <NA>       <NA>                 20               9
    ##  8 Con8  #3/83/3-3       <NA>       <NA>                 20               9
    ##  9 Con8  #2/95/3         <NA>       <NA>                 20               8
    ## 10 Con8  #3/5/2/2/95     28.5       <NA>                 20               8
    ## # ℹ 39 more rows
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

``` r
filter(litter_df, gd_of_birth == 20, gd0_weight > 22)
```

    ## # A tibble: 16 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>         <chr>      <chr>             <dbl>           <dbl>
    ##  1 Con8  #3/5/2/2/95   28.5       <NA>                 20               8
    ##  2 Mod7  #3/82/3-2     28         45.9                 20               5
    ##  3 Low7  #84/2         24.3       40.8                 20               8
    ##  4 Low7  #107          22.6       42.4                 20               9
    ##  5 Low7  #85/2         22.2       38.5                 20               8
    ##  6 Low7  #98           23.8       43.8                 20               9
    ##  7 Low7  #102          22.6       43.3                 20              11
    ##  8 Low7  #101          23.8       42.7                 20               9
    ##  9 Low7  #111          25.5       44.6                 20               3
    ## 10 Mod8  #97           24.5       42.8                 20               8
    ## 11 Mod8  #7/82-3-2     26.9       43.2                 20               7
    ## 12 Mod8  #2/95/2       28.5       44.5                 20               9
    ## 13 Mod8  #82/4         33.4       52.7                 20               8
    ## 14 Low8  #108          25.6       47.5                 20               8
    ## 15 Low8  #99           23.5       39                   20               6
    ## 16 Low8  #110          25.5       42.7                 20               7
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

``` r
filter(litter_df, group == "Mod8")
```

    ## # A tibble: 7 × 8
    ##   group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##   <chr> <chr>         <chr>      <chr>             <dbl>           <dbl>
    ## 1 Mod8  #97           24.5       42.8                 20               8
    ## 2 Mod8  #5/93         <NA>       41.1                 20              11
    ## 3 Mod8  #5/93/2       .          .                    19               8
    ## 4 Mod8  #7/82-3-2     26.9       43.2                 20               7
    ## 5 Mod8  #7/110/3-2    27.5       46                   19               8
    ## 6 Mod8  #2/95/2       28.5       44.5                 20               9
    ## 7 Mod8  #82/4         33.4       52.7                 20               8
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

``` r
filter(litter_df, group %in% c("Mod8", "Con7"))
```

    ## # A tibble: 14 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>           <chr>      <chr>             <dbl>           <dbl>
    ##  1 Con7  #85             19.7       34.7                 20               3
    ##  2 Con7  #1/2/95/2       27         42                   19               8
    ##  3 Con7  #5/5/3/83/3-3   26         41.4                 19               6
    ##  4 Con7  #5/4/2/95/2     28.5       44.1                 19               5
    ##  5 Con7  #4/2/95/3-3     <NA>       <NA>                 20               6
    ##  6 Con7  #2/2/95/3-2     <NA>       <NA>                 20               6
    ##  7 Con7  #1/5/3/83/3-3/2 <NA>       <NA>                 20               9
    ##  8 Mod8  #97             24.5       42.8                 20               8
    ##  9 Mod8  #5/93           <NA>       41.1                 20              11
    ## 10 Mod8  #5/93/2         .          .                    19               8
    ## 11 Mod8  #7/82-3-2       26.9       43.2                 20               7
    ## 12 Mod8  #7/110/3-2      27.5       46                   19               8
    ## 13 Mod8  #2/95/2         28.5       44.5                 20               9
    ## 14 Mod8  #82/4           33.4       52.7                 20               8
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

## ‘Mutate’

mutate is used to create new columns or change the context in different
columns. Question: why the diff(diff_weight = 18_weight-0_weight) cannot
calculate but Prof.Jeff could?

``` r
mutate(litter_df,  
       group = str_to_lower(group))
```

    ## # A tibble: 49 × 8
    ##    group litter_number   gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>           <chr>      <chr>             <dbl>           <dbl>
    ##  1 con7  #85             19.7       34.7                 20               3
    ##  2 con7  #1/2/95/2       27         42                   19               8
    ##  3 con7  #5/5/3/83/3-3   26         41.4                 19               6
    ##  4 con7  #5/4/2/95/2     28.5       44.1                 19               5
    ##  5 con7  #4/2/95/3-3     <NA>       <NA>                 20               6
    ##  6 con7  #2/2/95/3-2     <NA>       <NA>                 20               6
    ##  7 con7  #1/5/3/83/3-3/2 <NA>       <NA>                 20               9
    ##  8 con8  #3/83/3-3       <NA>       <NA>                 20               9
    ##  9 con8  #2/95/3         <NA>       <NA>                 20               8
    ## 10 con8  #3/5/2/2/95     28.5       <NA>                 20               8
    ## # ℹ 39 more rows
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

## ‘Arrange’

which is put the data in order.(Used not that often) first A and then B

``` r
arrange(litter_df, pups_born_alive)
```

    ## # A tibble: 49 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>         <chr>      <chr>             <dbl>           <dbl>
    ##  1 Con7  #85           19.7       34.7                 20               3
    ##  2 Low7  #111          25.5       44.6                 20               3
    ##  3 Low8  #4/84         21.8       35.2                 20               4
    ##  4 Con7  #5/4/2/95/2   28.5       44.1                 19               5
    ##  5 Con8  #2/2/95/2     <NA>       <NA>                 19               5
    ##  6 Mod7  #3/82/3-2     28         45.9                 20               5
    ##  7 Mod7  #5/3/83/5-2   22.6       37                   19               5
    ##  8 Mod7  #106          21.7       37.8                 20               5
    ##  9 Con7  #5/5/3/83/3-3 26         41.4                 19               6
    ## 10 Con7  #4/2/95/3-3   <NA>       <NA>                 20               6
    ## # ℹ 39 more rows
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

``` r
arrange(litter_df, pups_born_alive, gd0_weight)
```

    ## # A tibble: 49 × 8
    ##    group litter_number gd0_weight gd18_weight gd_of_birth pups_born_alive
    ##    <chr> <chr>         <chr>      <chr>             <dbl>           <dbl>
    ##  1 Con7  #85           19.7       34.7                 20               3
    ##  2 Low7  #111          25.5       44.6                 20               3
    ##  3 Low8  #4/84         21.8       35.2                 20               4
    ##  4 Mod7  #106          21.7       37.8                 20               5
    ##  5 Mod7  #5/3/83/5-2   22.6       37                   19               5
    ##  6 Mod7  #3/82/3-2     28         45.9                 20               5
    ##  7 Con7  #5/4/2/95/2   28.5       44.1                 19               5
    ##  8 Con8  #2/2/95/2     <NA>       <NA>                 19               5
    ##  9 Low8  #99           23.5       39                   20               6
    ## 10 Low7  #112          23.9       40.5                 19               6
    ## # ℹ 39 more rows
    ## # ℹ 2 more variables: pups_dead_birth <dbl>, pups_survive <dbl>

## pipe %\>%

pipe is used to connect the functions together. one more fuction:

``` r
litter_df_without_missing = drop_na(litter_df, gd0_weight)
```

A %\>% B %\>% select(only code the things after ‘,’), mutate works the
same way.

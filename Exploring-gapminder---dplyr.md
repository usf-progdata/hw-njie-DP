Exploring gapminder & dplyr
================
Ni Jie
2/8/2021

# Loading Packages

``` r
library(gapminder)
library(palmerpenguins)
library(ggplot2)
library(tidyverse)
```

    ## ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.0 ──

    ## ✓ tibble  3.0.5     ✓ dplyr   1.0.3
    ## ✓ tidyr   1.1.2     ✓ stringr 1.4.0
    ## ✓ readr   1.4.0     ✓ forcats 0.5.1
    ## ✓ purrr   0.3.4

    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## x dplyr::filter() masks stats::filter()
    ## x dplyr::lag()    masks stats::lag()

``` r
library(dplyr)
View(gapminder)
```

    ## Warning in View(gapminder): unable to open display

    ## Error in .External2(C_dataviewer, x, title): unable to start data viewer

``` r
View(palmerpenguins::penguins)
```

    ## Warning in View(palmerpenguins::penguins): unable to open display

    ## Error in .External2(C_dataviewer, x, title): unable to start data viewer

# Exercise 1: Basic dplyr

1.1 Use filter() to subset the gapminder data to three countries of your
choice in the 1970’s.

``` r
subset1970 <- filter(gapminder, country %in% c("Austria", "Benin", "Brazil") & year >=1970 & year < 1980)
subset1970
```

    ## # A tibble: 6 x 6
    ##   country continent  year lifeExp       pop gdpPercap
    ##   <fct>   <fct>     <int>   <dbl>     <int>     <dbl>
    ## 1 Austria Europe     1972    70.6   7544201    16662.
    ## 2 Austria Europe     1977    72.2   7568430    19749.
    ## 3 Benin   Africa     1972    47.0   2761407     1086.
    ## 4 Benin   Africa     1977    49.2   3168267     1029.
    ## 5 Brazil  Americas   1972    59.5 100840058     4986.
    ## 6 Brazil  Americas   1977    61.5 114313951     6660.

1.2 Use the pipe operator %&gt;% to select “country” and “gdpPercap”
from your filtered dataset in 1.1.

``` r
subset1970 %>%
  select(country, gdpPercap)
```

    ## # A tibble: 6 x 2
    ##   country gdpPercap
    ##   <fct>       <dbl>
    ## 1 Austria    16662.
    ## 2 Austria    19749.
    ## 3 Benin       1086.
    ## 4 Benin       1029.
    ## 5 Brazil      4986.
    ## 6 Brazil      6660.

1.3 Make a new variable in gapminder for the change in life expectancy
from the previous measurement. Filter this table to show all of the
entries that have experienced a drop in life expectancy.

``` r
gapminder %>%
  mutate(changeLifeExp = lifeExp - lag(lifeExp)) %>%
  filter(changeLifeExp<0)
```

    ## # A tibble: 221 x 7
    ##    country    continent  year lifeExp      pop gdpPercap changeLifeExp
    ##    <fct>      <fct>     <int>   <dbl>    <int>     <dbl>         <dbl>
    ##  1 Albania    Europe     1992    71.6  3326498     2497.        -0.419
    ##  2 Algeria    Africa     1952    43.1  9279525     2449.       -33.3  
    ##  3 Angola     Africa     1952    30.0  4232095     3521.       -42.3  
    ##  4 Angola     Africa     1987    39.9  7874230     2430.        -0.036
    ##  5 Australia  Oceania    1952    69.1  8691212    10040.        -6.20 
    ##  6 Austria    Europe     1952    66.8  6927772     6137.       -14.4  
    ##  7 Bahrain    Asia       1952    50.9   120447     9867.       -28.9  
    ##  8 Bangladesh Asia       1952    37.5 46886859      684.       -38.2  
    ##  9 Benin      Africa     1952    38.2  1738315     1063.       -41.2  
    ## 10 Benin      Africa     2002    54.4  7026113     1373.        -0.371
    ## # … with 211 more rows

1.4 Filter gapminder so that it shows the max GDP per capita experienced
by each country.

``` r
gapminder %>%
  group_by(country) %>%
  summarise(gdpPercap = max(gdpPercap))
```

    ## # A tibble: 142 x 2
    ##    country     gdpPercap
    ##  * <fct>           <dbl>
    ##  1 Afghanistan      978.
    ##  2 Albania         5937.
    ##  3 Algeria         6223.
    ##  4 Angola          5523.
    ##  5 Argentina      12779.
    ##  6 Australia      34435.
    ##  7 Austria        36126.
    ##  8 Bahrain        29796.
    ##  9 Bangladesh      1391.
    ## 10 Belgium        33693.
    ## # … with 132 more rows

1.5 Produce a scatterplot of Canada’s life expectancy vs. GDP per capita
using ggplot2, without defining a new variable. That is, after filtering
the gapminder data set, pipe it directly into the ggplot() function. In
your plot, put GDP per capita on a log scale.

``` r
gapminder %>%
  filter(country == "Canada") %>%
  ggplot(aes(x=lifeExp, y=log(gdpPercap))) + 
  geom_point()
```

![](Exploring-gapminder---dplyr_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

# Exercise 2: Explore two variables with dplyr and ggplot2

2.1 I use “palmerpenguins::penguins” dataset and two quantitative
variables that are “bill length” and “flipper length”.

``` r
#descriptive table
palmerpenguins::penguins %>%
  select(bill_length_mm, flipper_length_mm) %>%
  summarize(mean_bill = mean(bill_length_mm, na.rm = TRUE),
            mean_flipper = mean(flipper_length_mm, na.rm = TRUE),
            sd_bill = sd(bill_length_mm, na.rm = TRUE),
            sd_flipper = sd(flipper_length_mm, na.rm = TRUE))
```

    ## # A tibble: 1 x 4
    ##   mean_bill mean_flipper sd_bill sd_flipper
    ##       <dbl>        <dbl>   <dbl>      <dbl>
    ## 1      43.9         201.    5.46       14.1

``` r
# scatterplot
palmerpenguins::penguins %>%
  select(bill_length_mm, flipper_length_mm) %>%
  ggplot(aes(x=bill_length_mm, y=flipper_length_mm)) + 
  geom_point(alpha = .5)
```

    ## Warning: Removed 2 rows containing missing values (geom_point).

![](Exploring-gapminder---dplyr_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->
2.2 I pick “body mass” and “species”. Histogram and Boxplot are used to
visualize these variables.

``` r
#descriptive table
palmerpenguins::penguins %>%
  select(species, body_mass_g) %>%
  group_by(species) %>%
  summarize(sample_n = n(), 
            mean_bodymass = mean(body_mass_g, na.rm = TRUE), 
            sd_bodymass = sd(body_mass_g, na.rm = TRUE))
```

    ## # A tibble: 3 x 4
    ##   species   sample_n mean_bodymass sd_bodymass
    ## * <fct>        <int>         <dbl>       <dbl>
    ## 1 Adelie         152         3701.        459.
    ## 2 Chinstrap       68         3733.        384.
    ## 3 Gentoo         124         5076.        504.

``` r
#histogram
palmerpenguins::penguins %>%
  select(species, body_mass_g) %>%
  ggplot(aes(x=body_mass_g, color=species)) + 
  geom_histogram()
```

    ## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

    ## Warning: Removed 2 rows containing non-finite values (stat_bin).

![](Exploring-gapminder---dplyr_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
#boxplot
palmerpenguins::penguins %>%
  select(species, body_mass_g) %>%
  ggplot(aes(x=species, y=body_mass_g)) + 
  geom_boxplot()
```

    ## Warning: Removed 2 rows containing non-finite values (stat_boxplot).

![](Exploring-gapminder---dplyr_files/figure-gfm/unnamed-chunk-7-2.png)<!-- -->

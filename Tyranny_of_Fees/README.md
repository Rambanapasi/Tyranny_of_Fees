# Approach

-   management fees are constructed as ad valorem fee and overtime will
    have a compounding effect just like returns when they compound
    overtime.

## notes

-   since fees compound i tried apply the fees towards the investment
    amount at the end of the year. the fee should also compound because
    its growing at a constant on an increasing base year over year.

-   the effect of the feee should be deducted every year from the year
    investment amount.

-   so whatever value we have for the portfolio we deduct the relevant
    fee for that period.

-   to get the compounding effect yoy I have to get the index referring
    to the year in which the total return applies. So day we are in 2002
    and that the start of the index, i can depict that as investmnet
    horizon year zero. the rest will be straight forward.

-   this works because annualized return is the yearly depiction of
    compounding return

-   be careful to take measures that will not break the chain in
    returns, Nico uses coalesce to replace any NAs with Zeros.

-   so get annualized return for the actual investment and deduct the
    net fees from that. and a compoundnig period column

``` r
rm(list = ls()) # Clean your environment:
gc() # garbage collection - It can be useful to call gc after a large object has been removed, as this may prompt R to return memory to the operating system.
```

    ##          used (Mb) gc trigger (Mb) limit (Mb) max used (Mb)
    ## Ncells 468159 25.1    1002891 53.6         NA   669411 35.8
    ## Vcells 875486  6.7    8388608 64.0      16384  1851710 14.2

``` r
library(tidyverse)
```

    ## ── Attaching core tidyverse packages ──────────────────────── tidyverse 2.0.0 ──
    ## ✔ dplyr     1.1.2     ✔ readr     2.1.4
    ## ✔ forcats   1.0.0     ✔ stringr   1.5.0
    ## ✔ ggplot2   3.4.2     ✔ tibble    3.2.1
    ## ✔ lubridate 1.9.2     ✔ tidyr     1.3.0
    ## ✔ purrr     1.0.1     
    ## ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
    ## ✖ dplyr::filter() masks stats::filter()
    ## ✖ dplyr::lag()    masks stats::lag()
    ## ℹ Use the conflicted package (<http://conflicted.r-lib.org/>) to force all conflicts to become errors

``` r
list.files('code/', full.names = T, recursive = T) %>% .[grepl('.R', .)] %>% as.list() %>% walk(~source(.))
library(lubridate)

data <- fmxdat::Jalshtr
```

``` r
#  calculate annulaized returns 

com.rets <- data %>% 
  arrange(date) %>% 
  mutate(Year = format(date,"%Y")) %>% 
  mutate (ret = TRI / lag (TRI) - 1) %>% 
  filter(date>first(date)) %>%
  mutate(com.ret = cumprod(1+ret) ) %>% 
  group_by(Year) %>% 
  filter(date == last(date)) %>% 
  ungroup()
  
#  now to get annulized returns 

ann.ret <- com.rets %>%
  mutate(Year = format(date, "%Y"),
         Year = as.Date(paste0(Year, "-12-31"))) %>%
  group_by(Year) %>%
  summarise(Annual.ret = com.ret^(1/12) - 1)

#  add another column for componding period

ann.ret <- ann.ret %>%
  mutate(horizon = as.numeric(difftime(Year, as.Date("2002-12-31"), units = "days")) / 365,
         horizon = round(horizon))


# include fees 

fee_impact <- function(investment, fee, annual.return, horizon) {
  fee <- fee / 10000  # Convert basis points (bp) to decimal form
  final_portfolio_with_fees <- investment * (1 + annual.return - fee) ^ horizon
  final_portfolio_with_fees
}

fee.difference <- function(df, bp){
 new.df <-  df %>% 
    mutate(bp_1 = fee_impact(1000, bp, ann.ret$Annual.ret, ann.ret$horizon))
    new.df
}

fee.difference(ann.ret, 0)
```

    ## # A tibble: 20 × 4
    ##    Year       Annual.ret horizon   bp_1
    ##    <date>          <dbl>   <dbl>  <dbl>
    ##  1 2002-12-31   -0.0154        0  1000 
    ##  2 2003-12-31   -0.00314       1   997.
    ##  3 2004-12-31    0.0159        2  1032.
    ##  4 2005-12-31    0.0492        3  1155.
    ##  5 2006-12-31    0.0798        4  1359.
    ##  6 2007-12-31    0.0957        5  1579.
    ##  7 2008-12-31    0.0718        6  1516.
    ##  8 2009-12-31    0.0970        7  1912.
    ##  9 2010-12-31    0.113         8  2355.
    ## 10 2011-12-31    0.115         9  2671.
    ## 11 2012-12-31    0.138        10  3629.
    ## 12 2013-12-31    0.156        11  4932.
    ## 13 2014-12-31    0.166        12  6322.
    ## 14 2015-12-31    0.171        13  7783.
    ## 15 2016-12-31    0.174        14  9394.
    ## 16 2017-12-31    0.192        15 13983.
    ## 17 2018-12-31    0.183        16 14803.
    ## 18 2019-12-31    0.195        17 20582.
    ## 19 2020-12-31    0.201        18 27215.
    ## 20 2021-12-31    0.215        19 40368.

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
    ## Ncells 468155 25.1    1002882 53.6         NA   669405 35.8
    ## Vcells 875440  6.7    8388608 64.0      16384  1851632 14.2

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


fee_impact(1000, 100, ann.ret$Annual.ret, ann.ret$horizon )
```

    ##  [1]  1000.0000   986.8636  1011.7787  1122.1586  1309.7461  1508.5228
    ##  [7]  1433.2098  1793.1147  2190.8979  2463.3777  3321.9608  4482.5681
    ## [13]  5701.5100  6961.7528  8333.0408 12323.4772 12923.8692 17841.1175
    ## [19] 23413.8492 34501.0346

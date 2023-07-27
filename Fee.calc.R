library(PortfolioAnalytics)
fee.data <- function(df, fee_rate_1) {
  fee.structure <- df %>% 
    mutate(net_cumulative_returns_1 = cum.ret * (1 - fee_rate_1)) 
  
  return(fee.structure)
}

fee.data_2 <- function(df, fee_rates) {
  fee_structure <- map_dfc(fee_rates, ~mutate(df, !!paste0("net_cumulative_returns_", .x) := cum.ret * (1 - .x)))
  
  return(fee_structure)
}

cum.ret %>% fee.data_2(., 0,02 )

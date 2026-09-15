# 06_asset_prices_pull.R
#
# Network-dependent. Pulls monthly equity/commodity/corporate-bond price
# series (~1,000 tickers across 7 regional baskets) from Yahoo Finance
# and Stooq. Feeds Table 4 and Figures 5-7/10 via
# 07_asset_prices_cleaning.R -> 11_dfm_global_factor.R.
#
# NOT run by run_all.R by default and NOT guaranteed to reproduce the exact
# panel used in the thesis: Yahoo/Stooq histories can be revised or tickers
# delisted after the fact. The historical vintage is in data/build/asset_prices/;
# 07_asset_prices_cleaning.R reads that vintage directly.


source(here::here("R", "00_config.R"))

suppressMessages({
  library(openxlsx)
  library(dplyr)
  library(tidyr)
  library(purrr)
  library(quantmod)
  library(lubridate)
  library(tidyquant)
})

out <- path$build$asset_prices
ticker <- read.xlsx(file.path(path$raw$asset_prices, "asset prices_ticker.xlsx"), sheet = 1)

prices.M <- function(ticker, src) {
  from <- as.Date("1990-01-01")
  to <- as.Date("2024-12-31")

  map_dfr(na.omit(ticker), function(sym) {
    getSymbols(sym, src = src, from = from, to = to, auto.assign = FALSE) %>%
      to.monthly(indexAt = "lastof", drop.time = TRUE) %>%
      Ad() %>%
      `colnames<-`("price") %>%
      as.data.frame() %>%
      tibble::rownames_to_column("date") %>%
      mutate(date = as.Date(date), symbol = sym, .before = 1)
  })
}

write_region <- function(df, file) {
  wide <- df %>%
    pivot_wider(names_from = symbol, values_from = price) %>%
    arrange(date)
  write.csv(wide, file.path(out, file), row.names = FALSE)
  wide
}

write_region(prices.M(ticker$NorthAm, src = "yahoo"), "north_america_prices.csv")
write_region(prices.M(ticker$LatAm, src = "yahoo"), "latin_america_prices.csv")

EU.df <- prices.M(ticker$EU, src = "yahoo")
EU.nodupl <- EU.df |>
  group_by(date, symbol) |>
  summarise(price = last(price[!is.na(price)]), .groups = "drop")
EU.wide <- EU.nodupl |>
  pivot_wider(names_from = symbol, values_from = price, values_fill = NA) |>
  arrange(date)
write.csv(EU.wide, file.path(out, "europe_prices.csv"), row.names = FALSE)

write_region(prices.M(ticker$Asia, src = "yahoo"), "asia_prices.csv")
write_region(prices.M(ticker$AUS, src = "yahoo"), "australia_prices.csv")
write_region(prices.M(ticker$Corporate, src = "yahoo"), "corporate_bond_prices.csv")

prices.M.stooq <- function(ticker, from = as.Date("1990-01-01"), to = as.Date("2024-12-31")) {
  map_dfr(na.omit(ticker), function(sym) {
    tq_get(sym, get = "stock.prices", from = from, to = to, source = "stooq") %>%
      group_by(date = floor_date(date, "month")) %>%
      slice_tail(n = 1) %>%
      ungroup() %>%
      transmute(symbol = sym, date, price = close)
  })
}

write_region(prices.M.stooq(ticker$Cmdty), "commodities_prices.csv")

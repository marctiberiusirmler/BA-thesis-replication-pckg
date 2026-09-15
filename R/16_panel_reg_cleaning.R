# 16_panel_reg_cleaning.R
#
# Network-dependent for two of its inputs (GDP growth and NEER via the IMF
# IFS API) everything else is local. Builds the country-quarter panel
# used in the Section-5 panel regressions: BOP flow variables, VIX level &
# change, lagged real GDP growth and NEER, Bloomberg stock-market returns,
# and banking-sector leverage growth.

source(here::here("R", "00_config.R"))

suppressMessages({
  library(tidyverse)
  library(readxl)
  library(countrycode)
  library(stringr)
  library(zoo)
  library(imf.data)
  library(purrr)
  library(lubridate)
})

flows_raw <- path$raw$capital_flows
credit_dir <- path$build$credit
vix_dir <- path$build$vix
misc_dir <- path$raw$misc
panel_raw <- path$raw$panel

codes <- read_excel(file.path(flows_raw, "imf_country_codes.xlsx"), sheet = 1)
codes.incl <- na.omit(codes$`ISO Code (incl.)`)
countries.incl <- countrycode(codes.incl, "imf", "iso2c")

IFS <- load_datasets("IFS", use_cache = TRUE)

# Capital Flows ----

flows <- read.csv(file.path(flows_raw, "BOP_04-25-2025 12-12-16-14_timeSeries.csv"), header = TRUE) %>%
  dplyr::select(-matches("^X(19[0-8][0-9])Q[1-4]$")) %>%
  mutate(Country.Code = as.numeric(Country.Code)) %>%
  filter(Country.Code %in% codes.incl) %>%
  filter(Attribute %in% "Value") %>%
  mutate(
    flow_type = case_when(
      str_detect(Indicator.Name, regex("Liabilities", ignore_case = TRUE)) ~ "Liability",
      str_detect(Indicator.Name, "Assets") ~ "Asset",
      TRUE ~ NA_character_
    ),
    asset_class = case_when(
      str_detect(Indicator.Code, "^BFD") ~ "FDI",
      str_detect(Indicator.Code, "E_") ~ "Equity",
      str_detect(Indicator.Code, "D_") ~ "Debt",
      str_detect(Indicator.Code, "^BFO") ~ "Credit",
      TRUE ~ NA_character_
    ),
    country = countrycode(Country.Code, "imf", "iso2c")
  ) %>%
  relocate(country, flow_type, asset_class) %>%
  dplyr::select(-c(Country.Name, Country.Code, Indicator.Name, Indicator.Code, Attribute, X)) %>%
  filter(asset_class != "FDI")

flows <- replace(flows, flows == 0, NA)

flows <- flows %>%
  mutate(across(starts_with("X"), ~ parse_number(as.character(.x)))) %>%
  pivot_longer(cols = starts_with("X"), names_to = "quarter_raw", values_to = "value") %>%
  mutate(quarter = str_remove(quarter_raw, "^X"), quarter = as.yearqtr(quarter, format = "%YQ%q")) %>%
  dplyr::select(-quarter_raw)

flows <- flows %>%
  mutate(var = paste0(tolower(asset_class), "_", substr(tolower(flow_type), 1, 1))) %>%
  dplyr::select(-flow_type, -asset_class) %>%
  pivot_wider(names_from = var, values_from = value)

flows <- flows %>%
  arrange(country, quarter) %>%
  group_by(country) %>%
  mutate(across(c(credit_l, credit_a, debt_l, debt_a, equity_l, equity_a), ~ .x / 1e12)) %>%
  mutate(across(c(credit_l, credit_a, debt_l, debt_a, equity_l, equity_a), ~ dplyr::lag(.x), .names = "{.col}_lag")) %>%
  ungroup()

# Credit nonbank Inflows ----

credit_nb <- read_csv(file.path(misc_dir, "BIS_LBS_credit_nonbank.csv"), skip = 15, show_col_types = FALSE) %>%
  slice(-c(1:4)) %>%
  dplyr::select(-`C9:Czechoslovakia`) %>%
  rename(quarter = "L_CP_COUNTRY:Counterparty country") %>%
  pivot_longer(cols = -quarter, names_to = "country", values_to = "stock_nb") %>%
  mutate(quarter = as.yearqtr(as.Date(quarter), format = "%YQ%q"), country = countrycode(country, "country.name.en", "iso2c")) %>%
  group_by(country) %>%
  arrange(quarter) %>%
  mutate(
    stock_nb = as.numeric(stock_nb) / 1e6,
    credit_nb_l = stock_nb - dplyr::lag(stock_nb),
    credit_nb_l_lag = dplyr::lag(credit_nb_l)
  ) %>%
  ungroup() %>%
  dplyr::select(quarter, country, credit_nb_l, credit_nb_l_lag)

# VIX ----

VIX <- read_csv(file.path(vix_dir, "vix_quarterly.csv"), show_col_types = FALSE) %>%
  mutate(delta_VIX = c(NA, diff(CLOSE)), VIX_lag = dplyr::lag(CLOSE), DATE = as.yearqtr(DATE, format = "%YQ%q")) %>%
  rename(VIX_t = CLOSE, quarter = DATE)

# lagged GDP growth ----

GDP_code <- "NGDP_R_SA_XDC" # Gross Domestic Product, Real, Seasonally Adjusted, Domestic Currency

GDP <- IFS$get_series(indicator = GDP_code, ref_area = countries.incl, start = 1990, end = 2024, freq = "Q") %>%
  rename(quarter = TIME_PERIOD) %>%
  mutate(quarter = as.yearqtr(quarter, format = "%Y-Q%q")) %>%
  pivot_longer(cols = -quarter, names_to = c("country", "series"), names_pattern = "^Q\\.([A-Z]{2})\\.(.+)$", values_to = "GDP") %>%
  dplyr::select(-series) %>%
  arrange(country, quarter) %>%
  group_by(country) %>%
  mutate(GDP = as.numeric(GDP), GDPg = GDP / dplyr::lag(GDP) - 1, GDPg_lag = dplyr::lag(GDPg)) %>%
  dplyr::select(-GDP, -GDPg)

# Nominal effective exchange rate ----

NEER_code <- "ENEER_IX"

NEER <- IFS$get_series(indicator = NEER_code, ref_area = countries.incl, start = 1990, end = 2024, freq = "Q") %>%
  rename(quarter = TIME_PERIOD) %>%
  mutate(quarter = as.yearqtr(quarter, format = "%Y-Q%q")) %>%
  pivot_longer(cols = -quarter, names_to = c("country", "series"), names_pattern = "^Q\\.([A-Z]{2})\\.(.+)$", values_to = "NEER") %>%
  dplyr::select(-series) %>%
  group_by(country) %>%
  arrange(quarter) %>%
  mutate(NEER_lag = lag(NEER)) %>%
  ungroup() %>%
  dplyr::select(-NEER)

# Stock Market Returns (Bloomberg) ----

raw_prices <- read_xlsx(file.path(panel_raw, "World Indices_EoQ Prices.xlsx"), skip = 2)
names(raw_prices)[1] <- "quarter"

raw_prices <- raw_prices %>%
  slice(-1) %>%
  mutate(quarter = as.yearqtr(quarter, "%d.%m.%Y")) %>%
  filter(quarter >= 1990)

stock_market <- raw_prices %>%
  pivot_longer(cols = -quarter, names_to = "country", values_to = "price") %>%
  mutate(country = countrycode(country, "country.name.en", "iso2c"), price = as.numeric(price)) %>%
  arrange(country, quarter) %>%
  group_by(country) %>%
  mutate(return = c(NA, diff(log(price)) * 100))

# Banking Sector Leverage ----

lev_g <- read_csv(file.path(credit_dir, "leverage_growth_by_country.csv"), show_col_types = FALSE) %>%
  rename(quarter = date) %>%
  mutate(quarter = as.yearqtr(quarter)) %>%
  dplyr::select(-leverage)

# build and export panel ----

Panel <- flows %>%
  left_join(VIX, by = "quarter") %>%
  left_join(stock_market, by = c("country", "quarter")) %>%
  left_join(lev_g, by = c("country", "quarter")) %>%
  left_join(credit_nb, by = c("country", "quarter")) %>%
  left_join(GDP, by = c("country", "quarter")) %>%
  left_join(NEER, by = c("country", "quarter")) %>%
  dplyr::select(
    quarter, country,
    return, lev_growth,
    VIX_t, delta_VIX, VIX_lag,
    credit_l, credit_nb_l, credit_a, debt_l, debt_a, equity_l, equity_a,
    credit_l_lag, credit_nb_l_lag, credit_a_lag, debt_l_lag, debt_a_lag, equity_l_lag, equity_a_lag,
    GDPg_lag, NEER_lag
  )

write.csv(Panel, file.path(path$build$panel, "panel_regression_dataset.csv"), row.names = FALSE)

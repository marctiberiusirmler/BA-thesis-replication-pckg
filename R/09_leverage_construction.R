# 09_leverage_construction.R
#
# Requires network access (IMF IFS API, FRED) and the state saved
# by 08_credit_construction.R. Computes national banking-sector leverage
# (private credit / deposits) via ODC and DMB surveys with US/CA/GB special
# cases and EU-gap interpolation, then aggregates it two different ways:
#
#   (a) block-median leverage, with block-level leverage growth defined as
#       the growth rate of that block median (Table 3) and feeds the SVAR's 
#       EA-12 leverage series;
#   (b) the same block medians, but with block-level leverage growth
#       instead defined as the median of *country-level* leverage
#       differences, only a robustness variant.
#       Its used byproduct is the country-level leverage-growth panel 
#       that 16_panel_reg_cleaning.R needs for the
#       Section 5 regressions (Table 5, Table C.1).
#
# NOT run by run_all.R by default. Not guaranteed to reproduce the exact
# thesis vintage (live IMF/FRED pulls).


source(here::here("R", "00_config.R"))

suppressMessages({
  library(countrycode)
  library(openxlsx)
  library(readxl)
  library(tidyverse)
  library(zoo)
  library(imf.data)
})

raw <- path$raw$credit_leverage
out <- path$build$credit

state_file <- file.path(out, "credit_construction_state.rds")
if (!file.exists(state_file)) {
  stop(
    "Missing ", state_file, ". Run R/08_credit_construction.R first (",
    "requires network access) to produce it, or use the frozen leverage ",
    "files already shipped in data/build/credit/ and skip this script."
  )
}
state <- readRDS(state_file)
credit <- state$credit
dmb_credit <- state$dmb_credit
ex <- state$ex
block_map <- state$block_map
US_df <- state$US_df
CA_df <- state$CA_df
EU_countries <- state$EU_countries

countries <- c(
  "CA", "US", "AR", "BO", "BR", "CL", "CO", "CR", "EC", "MX",
  "BY", "BG", "HR", "CZ", "HU", "LV", "LT", "PL", "RO", "RU", "SK", "SI", "TR",
  "AT", "BE", "CY", "DK", "FI", "FR", "DE", "IS", "IE", "IT", "LU", "MT", "NL", "NO", "PT", "ES", "SE", "CH", "GB",
  "CN", "ID", "MY", "SG", "TH", "AU", "JP", "KR", "NZ", "IL", "ZA"
)

fix_iso2 <- function(name) countrycode(name, origin = "country.name", destination = "iso2c", nomatch = NA_character_)

# ODC-survey deposits (non-EU via IMF API, EU via saved legacy export) ----

IFS <- load_datasets("IFS", use_cache = TRUE)

dep_codes_nonEU <- c(demand = "FOST_XDC", other = "FOSD_XDC")
dep_raw_nonEU <- IFS$get_series(indicator = dep_codes_nonEU, ref_area = countries, start = 2001, end = 2024, freq = "Q")
dep_raw_EU <- read_csv(file.path(raw, "IFS_deposits_ODC_EA.csv"), show_col_types = FALSE)

dep_nonEU_tidy <- dep_raw_nonEU %>%
  pivot_longer(cols = -TIME_PERIOD, names_to = c("country", "code"), names_pattern = "^Q\\.([^\\.]+)\\.(.+)$", values_to = "value") %>%
  transmute(country, date = as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"), var = recode(code, FOST_XDC = "demand", FOSD_XDC = "other"), value = as.numeric(value))

dep_EU_tidy <- dep_raw_EU %>%
  rename(country = "Country Name", var = "Indicator Code") %>%
  mutate(country = countrycode(country, "country.name.en", "iso2c")) %>%
  dplyr::select(-c(2, 3, 5), -"Base Year", -"...100") %>%
  pivot_longer(cols = -c(country, var), names_to = "date", values_to = "value") %>%
  mutate(
    date = as.yearqtr(date, format = "%Y Q%q"),
    var = recode(var, FOSDEA_EUR = "demand", FOSODEA_EUR = "other"),
    value = as.numeric(value)
  )

dep_odc <- bind_rows(dep_nonEU_tidy, dep_EU_tidy) %>%
  group_by(country, date, var) %>%
  summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = var, values_from = value) %>%
  mutate(deposits_m = demand + other)

priv_odc <- credit %>% transmute(country, date, claims_priv_m = as.numeric(claims_private))

lev_odc <- priv_odc %>%
  left_join(dep_odc, by = c("country", "date")) %>%
  mutate(leverage = claims_priv_m / deposits_m) %>%
  dplyr::select(country, date, claims_priv_m, deposits_m, leverage)

# DMB-survey deposits via Excel ----

DMB_list <- list(
  demand = read_excel(file.path(raw, "IFS_DMB_demand dep.xlsx"), skip = 1),
  other  = read_excel(file.path(raw, "IFS_DMB_time, savings, fx dep.xlsx"), skip = 1) # time, savings, and fx deposits
)

dmb_dep <- imap_dfr(DMB_list, ~ {
  .x %>%
    rename(country_name = ...1, indicator = ...2) %>%
    mutate(country = fix_iso2(country_name)) %>%
    filter(country %in% countries) %>%
    pivot_longer(cols = -c(country_name, indicator, country), names_to = "TIME_PERIOD", values_to = "value") %>%
    transmute(country, date = as.yearqtr(TIME_PERIOD, format = "Q%q %Y"), var = .y, value = as.numeric(value))
}) %>%
  group_by(country, date, var) %>%
  summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(names_from = var, values_from = value) %>%
  replace_na(list(demand = 0, other = 0)) %>%
  mutate(deposits_m = demand + other, deposits_m = na_if(deposits_m, 0))

priv_dmb <- dmb_credit %>%
  transmute(country, date, claims_priv_m = as.numeric(claims_private)) %>%
  mutate(claims_priv_m = na_if(claims_priv_m, 0))

lev_dmb <- priv_dmb %>%
  left_join(dmb_dep, by = c("country", "date")) %>%
  mutate(leverage = claims_priv_m / deposits_m) %>%
  dplyr::select(country, date, claims_priv_m, deposits_m, leverage)

# Stitch DMB (1990-2000Q4) + ODC (2002Q1-2024) + US/CA/GB national sources ----

# US: private-sector claims from BIS (US_df, via 08), deposits from FRED
url <- "https://fred.stlouisfed.org/graph/fredgraph.csv?id=DPSACBW027SBOG"
deps_df <- read.csv(url, stringsAsFactors = FALSE)
deps_xts <- xts::xts(deps_df[, "DPSACBW027SBOG"], order.by = as.Date(deps_df[, "observation_date"]))
deps_q <- xts::to.quarterly(deps_xts)[, 4]
colnames(deps_q) <- "deps"

us_df <- data.frame(date = as.yearqtr(zoo::index(deps_q)), deps = zoo::coredata(deps_q))

us_lev <- us_df %>%
  filter(date >= 1990 & date < 2025) %>%
  left_join(US_df, by = "date") %>%
  mutate(lev_us = ndc_usd_bil / deps) %>%
  relocate(country, .after = date) %>%
  dplyr::select(date, country, lev_us)

# CA: private-sector claims from BIS, deposits from the Bank of Canada
ca_df <- read.csv(file.path(raw, "BoC_deposits.csv"), skip = 80) %>%
  transmute(date = as.yearqtr(date, format = "%YQ%q"), deps_cad = K1245 / 1e3) %>%
  filter(date >= 1990 & date < 2025)

ca_lev <- ca_df %>%
  left_join(CA_df, by = "date") %>%
  left_join(ex, by = c("country", "date")) %>%
  mutate(deps = deps_cad * ex_rate, lev_ca = ndc_ca_bil / deps) %>%
  relocate(country, .after = date) %>%
  dplyr::select(date, country, lev_ca)

# GB: private-sector claims from IFS (lev_dmb), deposits from BoE (LPMVYAX)
gb_df <- read.csv(file.path(raw, "BoE_deposits.csv")) %>%
  mutate(date = as.Date(Date, format = "%d %b %y")) %>%
  filter(month(date) %in% c(3, 6, 9, 12)) %>%
  transmute(
    date = as.yearqtr(Date, format = "%d %b %y"),
    country = "GB",
    deps = as.numeric(Monthly.amounts.outstanding.of.monetary.financial.institutions..sterling.deposits.from.private.sector..in.sterling.millions..not.seasonally.adjusted...............a...b...c...d...e...f...g...h...i...j...k...l...m...n...o...p...q...r...r...q...s...t..............LPMVYAX)
  ) %>%
  filter(date >= 1990 & date < 2025)

gb_lev <- gb_df %>%
  left_join(lev_dmb, by = c("country", "date")) %>%
  mutate(lev_gb = claims_priv_m / deps) %>%
  dplyr::select(date, country, lev_gb)

lev_dmb_j <- lev_dmb %>% dplyr::select(country, date, lev_dmb = leverage)
lev_odc_j <- lev_odc %>% dplyr::select(country, date, lev_odc = leverage)

lev_nat <- full_join(lev_dmb_j, lev_odc_j, by = c("country", "date")) %>%
  full_join(us_lev, by = c("country", "date")) %>%
  full_join(gb_lev, by = c("country", "date")) %>%
  full_join(ca_lev, by = c("country", "date")) %>%
  mutate(leverage = case_when(
    country == "US" ~ lev_us,
    country == "GB" ~ lev_gb,
    country == "CA" ~ lev_ca,
    country %in% c("CH", "CN", "SG") ~ lev_dmb,
    date >= as.yearqtr("2002 Q1", "%Y Q%q") & !is.na(lev_odc) ~ lev_odc,
    TRUE ~ lev_dmb
  )) %>%
  dplyr::select(country, date, leverage)

# Interpolate EU reporting gap ----

q0 <- as.yearqtr("1998-Q4", "%Y-Q%q")
q1 <- as.yearqtr("2001-Q4", "%Y-Q%q")

lev_nat <- lev_nat %>%
  mutate(date = as.yearqtr(date, format = "%Y-Q%q")) %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(
    lev_gap    = case_when(country %in% EU_countries & date >= q0 & date <= q1 ~ NA_real_, TRUE ~ leverage),
    lev_interp = na.approx(lev_gap, x = as.numeric(date), na.rm = FALSE),
    lev_final  = if_else(is.na(lev_gap), lev_interp, lev_gap)
  ) %>%
  ungroup() %>%
  dplyr::select(country, date, leverage = lev_final)

# EA-12 median leverage (for the SVAR) ----

EA12 <- c("AT", "BE", "FI", "FR", "DE", "GR", "IE", "IT", "LU", "NL", "PT", "ES")

lev_eu12 <- lev_nat %>%
  filter(country %in% EA12) %>%
  group_by(date) %>%
  summarize(eu_med_leverage = median(leverage, na.rm = TRUE), .groups = "drop")

write_csv(lev_eu12, file.path(out, "leverage_ea12_median.csv"))

# (a) Block median leverage, growth of the block median -- matches Table 3 ----

lev_block <- lev_nat %>%
  left_join(block_map, by = "country") %>%
  filter(!(country == "ZA" & date >= as.yearqtr("1991 Q2", "%Y Q%q") & date <= as.yearqtr("1991 Q4", "%Y Q%q"))) %>%
  group_by(block, date) %>%
  summarize(med_leverage = median(leverage, na.rm = TRUE), .groups = "drop") %>%
  group_by(block) %>%
  arrange(date) %>%
  mutate(lev_qoq = med_leverage / lag(med_leverage) - 1) %>%
  ungroup() %>%
  dplyr::select(block, date, med_leverage, lev_qoq)

write_csv(lev_block, file.path(out, "leverage_by_block.csv"))

# (b) Country-level leverage growth, and its block median -- robustness variant ----

lev_nat_growth <- lev_nat %>%
  dplyr::select(country, date, leverage) %>%
  group_by(country) %>%
  arrange(date) %>%
  mutate(lev_growth = leverage - lag(leverage)) %>% 
  ungroup()

write_csv(
  lev_nat_growth %>%
    filter(!country %in% c("GR", "EE")) %>%
    filter(!(country == "ZA" & date >= as.yearqtr("1991 Q2", "%Y Q%q") & date <= as.yearqtr("1991 Q4", "%Y Q%q"))),
  file.path(out, "leverage_growth_by_country.csv")
)

lev_block_growth_median <- lev_nat_growth %>%
  filter(!country %in% c("GR", "EE")) %>%
  left_join(block_map, by = "country") %>%
  filter(!(country == "ZA" & date >= as.yearqtr("1991 Q2", "%Y Q%q") & date <= as.yearqtr("1991 Q4", "%Y Q%q"))) %>%
  group_by(block, date) %>%
  summarise(med_leverage = median(leverage, na.rm = TRUE), lev_qoq = median(lev_growth, na.rm = TRUE), .groups = "drop") %>%
  dplyr::select(block, date, med_leverage, lev_qoq)

write_csv(lev_block_growth_median, file.path(out, "leverage_by_block_country_growth_median.csv"))

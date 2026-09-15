# 08_credit_construction.R
#
# Network-dependent: requires network access (IMF IFS API via the
# `imf.data` package). Builds global/national domestic credit (NDC): IMF
# IFS "Other Depository Corporations" survey for 2002+, blended with the
# legacy "Deposit Money Banks" survey for 1990-2001, across ~50 countries,
# with US/Canada/Russia national-source splices and EU-gap interpolation.
# Feeds Figure 4 (left panel), Table 3 ("Domestic Credit Growth" rows), and
# the SVAR's CREDIT variable.
#
# NOT run by run_all.R by default and NOT guaranteed to reproduce the exact
# thesis vintage. The historical vintage is in data/build/credit/ 
# (global_domestic_credit.csv, domestic_credit_by_block.csv, 
# and credit_construction_state.rds); scripts
# 09, 12, and 18 read that shipped vintage.

source(here::here("R", "00_config.R"))

suppressMessages({
  library(countrycode)
  library(openxlsx)
  library(readxl)
  library(tidyverse)
  library(zoo)
  library(lubridate)
  library(imf.data)
})

raw <- path$raw$credit_leverage
out <- path$build$credit

# 0. Setup & helpers ----

d <- list_datasets()
IFS <- load_datasets("IFS", use_cache = FALSE)
IMF.indicators <- IFS$dimensions$indicator
IMF.codes <- IFS$dimensions$ref_area
unitmult <- IFS$dimensions$unit_mult

countries <- c(
  "CA", "US",
  "AR", "BO", "BR", "CL", "CO", "CR", "EC", "MX",
  "BY", "BG", "HR", "CZ", "HU", "LV", "LT", "PL", "RO", "RU", "SK", "SI", "TR",
  "AT", "BE", "CY", "DK", "FI", "FR", "DE", "IS", "IE", "IT", "LU", "MT", "NL", "NO", "PT", "ES", "SE", "CH", "GB",
  "CN", "ID", "MY", "SG", "TH",
  "AU", "JP", "KR", "NZ",
  "IL", "ZA"
)

fix_iso2 <- function(name) countrycode(name, origin = "country.name", destination = "iso2c", nomatch = NA_real_)

# 1. Spot exchange rates ----

X_code <- "EDNE_USD_XDC_RATE"

ex_raw_XDC <- IFS$get_series(indicator = X_code, ref_area = countries, start = 1990, end = 2024, freq = "Q")
ex_raw_EUR <- IFS$get_series(indicator = X_code, ref_area = "U2", start = 1990, end = 2024, freq = "Q")

ex_ind <- ex_raw_XDC %>%
  pivot_longer(cols = -TIME_PERIOD, names_to = c("country", "code"), names_pattern = "^Q\\.([^\\.]+)\\.(.+)$", values_to = "value") %>%
  transmute(country, date = as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"), ex_rate_ind = as.numeric(value))

EU_countries <- c("AT", "BE", "CY", "EE", "ES", "FI", "FR", "DE", "IE", "IT", "LT", "LU", "LV", "MT", "NL", "PT", "SI", "SK")

ex_eur <- ex_raw_EUR %>%
  pivot_longer(cols = -TIME_PERIOD, names_to = c("ea", "code"), names_pattern = "^Q\\.([^\\.]+)\\.(.+)$", values_to = "value") %>%
  filter(ea == "U2") %>%
  transmute(date = as.yearqtr(TIME_PERIOD, format = "%Y-Q%q"), ex_rate_eur = as.numeric(value)) %>%
  crossing(country = EU_countries)

ex <- full_join(ex_ind, ex_eur, by = c("country", "date")) %>%
  mutate(ex_rate = case_when(
    country %in% EU_countries & date >= as.yearqtr("1999-Q1", "%Y-Q%q") ~ ex_rate_eur,
    TRUE ~ ex_rate_ind
  )) %>%
  dplyr::select(country, date, ex_rate)

# 2. Pull and tidy ODC series ----

odc_codes_nonEU <- c(
  claims_private = "FOSAOP_XDC", claims_public_nonfin = "FOSAON_XDC",
  claims_other_fin = "FOSAOF_XDC", claims_state_local = "FOSAOG_XDC", net_claims_cg = "FOSG_XDC"
)

# The EU-specific ODC claims codes below were retired from the API during the
# IMF's migration to the new data portal; EU claims come from a saved legacy
# export instead (data/raw/credit_leverage/IFS_claims_ODC_EA.csv).
odc_raw_nonEU <- IFS$get_series(indicator = odc_codes_nonEU, ref_area = countries, start = 2002, end = 2024, freq = "Q")
odc_raw_EU <- read_csv(file.path(raw, "IFS_claims_ODC_EA.csv"), show_col_types = FALSE)

odc_nonEU_tidy <- odc_raw_nonEU %>%
  pivot_longer(cols = -TIME_PERIOD, names_to = c("country", "code"), names_pattern = "^Q\\.([^\\.]+)\\.(.+)$", values_to = "value") %>%
  mutate(var = recode(code,
    FOSAOP_XDC = "claims_private", FOSAON_XDC = "claims_public_nonfin",
    FOSAOF_XDC = "claims_other_fin", FOSAOG_XDC = "claims_state_local", FOSG_XDC = "net_claims_cg"
  )) %>%
  dplyr::select(-code) %>%
  pivot_wider(names_from = var, values_from = value) %>%
  mutate(across(starts_with("claims_"), as.numeric), net_claims_cg = as.numeric(net_claims_cg)) %>%
  rename(date = TIME_PERIOD) %>%
  mutate(date = as.yearqtr(date, format = "%Y-Q%q"))

odc_EU_tidy <- odc_raw_EU %>%
  rename(country = "Country Name", var = "Indicator Code") %>%
  mutate(country = countrycode(country, "country.name.en", "iso2c")) %>%
  dplyr::select(-c(2, 3, 5), -"Base Year", -"...100") %>%
  pivot_longer(cols = -c(country, var), names_to = "date", values_to = "value") %>%
  mutate(
    date = as.yearqtr(date, format = "%Y Q%q"),
    var = recode(var,
      FOSAOPEA_EUR = "claims_private", FOSAONEA_EUR = "claims_public_nonfin",
      FOSAOFEA_EUR = "claims_other_fin", FOSAOGEA_EUR = "claims_state_local", FOSGEA_EUR = "net_claims_cg"
    ),
    value = as.numeric(value)
  ) %>%
  pivot_wider(names_from = var, values_from = value)

odc_all <- bind_rows(odc_nonEU_tidy, odc_EU_tidy) %>%
  dplyr::select(country, date, starts_with("claims_"), net_claims_cg) %>%
  arrange(country, date)

# 3. Compute NDC (local & USD) ----

credit <- odc_all %>%
  mutate(
    claims_all = claims_private + claims_public_nonfin + claims_other_fin + claims_state_local,
    ndc_local  = claims_all - net_claims_cg
  ) %>%
  left_join(ex, by = c("country", "date")) %>%
  mutate(
    ndc_local   = as.numeric(ndc_local),
    ex_rate     = as.numeric(ex_rate),
    ndc_usd_bil = ndc_local * ex_rate / 1e3
  )

# 4. IFS DMB data import & tidy ----

DMB_raw_PS <- read_excel(file.path(raw, "IFS_DMB_claims on private sector.xlsx"), skip = 1)
DMB_raw_PNF <- read_excel(file.path(raw, "IFS_DMB_claims on public non-fin corps.xlsx"), skip = 1)
DMB_raw_OF <- read_excel(file.path(raw, "IFS_DMB_claims on other fin corps.xlsx"), skip = 1)
DMB_raw_SLG <- read_excel(file.path(raw, "IFS_DMB_claims_state_local_gov.xlsx"), skip = 1)
DMB_raw_CG_claims <- read_excel(file.path(raw, "IFS_DMB_claims_cg.xlsx"), skip = 1)
DMB_raw_CG_deposits <- read_excel(file.path(raw, "IFS_DMB_deposits_cg.xlsx"), skip = 1)

dmb_nonEA_list <- list(
  claims_private = DMB_raw_PS, claims_public_nonfin = DMB_raw_PNF, claims_other_fin = DMB_raw_OF,
  claims_state_local = DMB_raw_SLG, cg_claims = DMB_raw_CG_claims, cg_deposits = DMB_raw_CG_deposits
)

dmb_nonEA_tidy <- imap_dfr(dmb_nonEA_list, ~ {
  .x %>%
    rename(country_name = ...1, indicator = ...2) %>%
    mutate(country = fix_iso2(country_name)) %>%
    filter(country %in% countries) %>%
    pivot_longer(cols = -c(country_name, indicator, country), names_to = "TIME_PERIOD", values_to = "value") %>%
    transmute(country, TIME_PERIOD, var = .y, value)
})

dmb_EA_tidy <- read_excel(file.path(raw, "IFS_DMB_EA.xlsx"), skip = 1) %>%
  rename(country_name = ...1, indicator = ...2) %>%
  mutate(country = fix_iso2(country_name)) %>%
  filter(country %in% countries) %>%
  pivot_longer(cols = -c(country_name, indicator, country), names_to = "TIME_PERIOD", values_to = "value") %>%
  transmute(
    country, TIME_PERIOD,
    var = case_when(
      str_detect(indicator, regex("Private  Sector", ignore_case = TRUE)) ~ "claims_private",
      str_detect(indicator, regex("Public Nonfinancial", ignore_case = TRUE)) ~ "claims_public_nonfin",
      str_detect(indicator, regex("Other Financial", ignore_case = TRUE)) ~ "claims_other_fin",
      str_detect(indicator, regex("State and Local", ignore_case = TRUE)) ~ "claims_state_local",
      str_detect(indicator, regex("Net Claims on Central", ignore_case = TRUE)) ~ "net_claims_cg",
      TRUE ~ NA_character_
    ),
    value
  )

dmb_wide <- bind_rows(dmb_nonEA_tidy, dmb_EA_tidy) %>%
  group_by(country, TIME_PERIOD, var) %>%
  summarize(value = mean(value, na.rm = TRUE), .groups = "drop") %>%
  pivot_wider(id_cols = c(country, TIME_PERIOD), names_from = var, values_from = value) %>%
  mutate(date = as.yearqtr(TIME_PERIOD, format = "Q%q %Y")) %>%
  filter(date >= as.yearqtr("Q1 1990", "Q%q %Y")) %>%
  dplyr::select(country, date, claims_private, claims_public_nonfin, claims_other_fin, claims_state_local, cg_claims, cg_deposits, net_claims_cg)

dmb_credit <- dmb_wide %>%
  mutate(across(
    c(claims_private, claims_public_nonfin, claims_other_fin, claims_state_local, cg_claims, cg_deposits, net_claims_cg),
    ~ replace_na(as.numeric(.), 0)
  )) %>%
  mutate(
    net_claims_cg = coalesce(net_claims_cg, cg_claims - cg_deposits),
    claims_all    = claims_private + claims_public_nonfin + claims_other_fin + claims_state_local,
    ndc_local     = claims_all - net_claims_cg
  ) %>%
  left_join(ex, by = c("country", "date")) %>%
  mutate(ndc_usd_bil = ndc_local * ex_rate / 1e3)

# 5. Merge ODC & DMB, add BIS series (US, RU, CA) ----

US_df <- read.csv(file.path(raw, "ndc_US.csv"), skip = 2) %>%
  transmute(country = "US", date = as.yearqtr(TIME_PERIOD, format = "%Y-%m-%d"), ndc_usd_bil = OBS_VALUE) %>%
  filter(date >= 1990)

RU_df <- read.csv(file.path(raw, "ndc_RU.csv"), skip = 2) %>%
  transmute(country = "RU", date = as.yearqtr(TIME_PERIOD, format = "%Y-%m-%d"), ndc_ru_bil = OBS_VALUE) %>%
  filter(date >= 1990)

CA_df <- read.csv(file.path(raw, "BIS_CA_priv sector claims.csv"), skip = 2) %>%
  transmute(country = "CA", date = as.yearqtr(TIME_PERIOD, format = "%Y-%m-%d"), ndc_ca_bil = OBS_VALUE) %>%
  filter(date >= 1990)

d_dmb <- dmb_credit %>% dplyr::select(country, date, ndc_dmb = ndc_usd_bil)
d_odc <- credit %>% dplyr::select(country, date, ndc_odc = ndc_usd_bil)

nat_credit <- full_join(d_dmb, d_odc, by = c("country", "date")) %>%
  full_join(US_df, by = c("country", "date")) %>%
  mutate(ndc_raw = case_when(
    country == "US" ~ ndc_usd_bil,
    country %in% c("CA", "CH", "CN", "GB", "SG") ~ ndc_dmb,
    date >= as.yearqtr("1998 Q2", "%Y Q%q") & !is.na(ndc_odc) ~ ndc_odc,
    TRUE ~ ndc_dmb
  )) %>%
  left_join(RU_df, by = c("country", "date")) %>%
  left_join(CA_df, by = c("country", "date")) %>%
  mutate(ndc_usd_bil = case_when(
    country == "RU" & date >= as.yearqtr("2022 Q1", "%Y Q%q") & !is.na(ndc_ru_bil) ~ ndc_ru_bil,
    country == "CA" & date >= as.yearqtr("2009 Q1", "%Y Q%q") & !is.na(ndc_ca_bil) ~ ndc_ca_bil,
    TRUE ~ ndc_raw
  )) %>%
  dplyr::select(country, date, ndc_usd_bil)

# 6. Interpolate EU gap ----

q0 <- as.yearqtr("1998-Q2", "%Y-Q%q")
q1 <- as.yearqtr("2002-Q1", "%Y-Q%q")

nat_credit <- nat_credit %>%
  arrange(country, date) %>%
  group_by(country) %>%
  mutate(
    ndc_gap    = if_else(country %in% EU_countries & date >= q0 & date <= q1, NA_real_, ndc_usd_bil),
    ndc_interp = na.approx(ndc_gap, x = as.numeric(date), na.rm = FALSE),
    ndc_final  = if_else(is.na(ndc_gap), ndc_interp, ndc_gap)
  ) %>%
  ungroup() %>%
  dplyr::select(country, date, ndc_usd_bil = ndc_final)

# 7. Compute global and block-level aggregates ----

global_full <- nat_credit %>%
  filter(
    !country %in% c("BY", "GR") | !(country == "SK" & date >= as.yearqtr("2002 Q3", "%Y Q%q") & date <= as.yearqtr("2005 Q4", "%Y Q%q")),
    !(country == "SI" & date >= as.yearqtr("1998 Q2", "%Y Q%q") & date <= as.yearqtr("2003 Q4", "%Y Q%q"))
  ) %>%
  group_by(date) %>%
  summarize(global_ndc_usd_bil = sum(ndc_usd_bil, na.rm = TRUE), .groups = "drop")

block_map <- tribble(
  ~country, ~block,
  "CA", "North America", "US", "North America",
  "AR", "Latin America", "BO", "Latin America", "BR", "Latin America",
  "CL", "Latin America", "CO", "Latin America", "CR", "Latin America",
  "EC", "Latin America", "MX", "Latin America",
  "BY", "Central and E. Europe", "BG", "Central and E. Europe",
  "HR", "Central and E. Europe", "CZ", "Central and E. Europe",
  "HU", "Central and E. Europe", "LV", "Central and E. Europe",
  "LT", "Central and E. Europe", "PL", "Central and E. Europe",
  "RO", "Central and E. Europe", "RU", "Central and E. Europe",
  "SK", "Central and E. Europe", "SI", "Central and E. Europe",
  "TR", "Central and E. Europe", "GR", "Central and E. Europe",
  "AT", "W. Europe", "BE", "W. Europe", "CY", "W. Europe",
  "DK", "W. Europe", "FI", "W. Europe", "FR", "W. Europe",
  "DE", "W. Europe", "IS", "W. Europe", "IE", "W. Europe",
  "IT", "W. Europe", "LU", "W. Europe", "MT", "W. Europe",
  "NL", "W. Europe", "NO", "W. Europe", "PT", "W. Europe",
  "ES", "W. Europe", "SE", "W. Europe", "CH", "W. Europe",
  "GB", "W. Europe",
  "CN", "Emerging Asia", "ID", "Emerging Asia", "MY", "Emerging Asia",
  "SG", "Emerging Asia", "TH", "Emerging Asia",
  "AU", "Asia Pacific", "JP", "Asia Pacific",
  "KR", "Asia Pacific", "NZ", "Asia Pacific",
  "IL", "ME/Africa", "ZA", "ME/Africa"
)

block_credit <- nat_credit %>%
  left_join(block_map, by = "country") %>%
  filter(
    !country %in% c("BY", "GR"),
    !(country == "SK" & date >= as.yearqtr("2002 Q3", "%Y Q%q") & date <= as.yearqtr("2005 Q4", "%Y Q%q")),
    !(country == "SI" & date >= as.yearqtr("1998 Q2", "%Y Q%q") & date <= as.yearqtr("2003 Q4", "%Y Q%q"))
  ) %>%
  group_by(block, date) %>%
  summarize(block_ndc_usd_bil = sum(ndc_usd_bil, na.rm = TRUE), .groups = "drop") %>%
  group_by(block) %>%
  arrange(date) %>%
  mutate(ndc_qoq = block_ndc_usd_bil / lag(block_ndc_usd_bil) - 1) %>%
  ungroup() %>%
  dplyr::select(block, date, block_ndc_usd_bil, ndc_qoq)

# 8. Write out CSVs + explicit intermediate state for the leverage scripts ----

write_csv(global_full, file.path(out, "global_domestic_credit.csv"))
write_csv(block_credit, file.path(out, "domestic_credit_by_block.csv"))

saveRDS(
  list(credit = credit, dmb_credit = dmb_credit, ex = ex, block_map = block_map, US_df = US_df, CA_df = CA_df, EU_countries = EU_countries),
  file.path(out, "credit_construction_state.rds")
)

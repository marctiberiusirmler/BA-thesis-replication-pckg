# 02_world_short_rate.R
#
# Builds a GDP-PPP-weighted world nominal policy rate and subtracts OECD
# inflation forecasts to get a real world short rate. Feeds the "world real
# short rate" conditioning variable used in Tables 1-3 (Correlation Tables).

source(here::here("R", "00_config.R"))

suppressMessages({
  library(tidyverse)
  library(lubridate)
  library(janitor)
  library(countrycode)
  library(stringr)
  library(zoo)
})

raw <- path$raw$macro

# GDP data cleaning and weight calculation ----

w.GDP <- read.csv(file.path(raw, "WorldBank_GDP PPP_OECD_EuroArea.A.csv"))

names(w.GDP)[5:39] <- names(w.GDP)[5:39] %>%
  str_replace("^X(\\d{4})\\.\\.YR\\d{4}\\.$", "\\1")

w.GDP[["2024"]] <- w.GDP[["2023"]] # copy missing 2024 obs from 2023

year.cols <- names(w.GDP) %>% str_subset("^\\d{4}$")

crosswalk <- tribble(
  ~r.nom.lbl,     ~gdp.lbl,    ~iso3c,
  "XM.Euro.area", "Euro Area", "EMU"
)

w.GDP.long <- w.GDP %>%
  dplyr::select(
    gdp.lbl = Country.Name,
    iso3c   = Country.Code,
    all_of(year.cols)
  ) %>%
  mutate(across(all_of(year.cols), ~ parse_number(as.character(.x), na = c("", "..")))) %>%
  pivot_longer(
    cols      = all_of(year.cols),
    names_to  = "year",
    values_to = "gdp"
  ) %>%
  mutate(
    year = as.integer(year),
    iso3c = coalesce(
      iso3c,
      countrycode(gdp.lbl, "country.name", "iso3c"),
      crosswalk$iso3c[match(gdp.lbl, crosswalk$gdp.lbl)]
    )
  ) %>%
  filter(!is.na(iso3c)) %>%
  group_by(year) %>%
  mutate(weight = gdp / sum(gdp, na.rm = TRUE)) %>%
  ungroup() %>%
  filter(!is.na(gdp))

# Nominal rates cleaning and weighted average calculation ----

r.nom <- read.csv(file.path(raw, "CBPOL_OECD_EuroArea.M.csv"), skip = 6)

r.nom.long <- r.nom %>%
  rename(date = `REF_AREA.Reference.area`) %>%
  mutate(date = ymd(substr(date, 1, 10))) %>%
  pivot_longer(-date, names_to = "raw.lbl", values_to = "policy") %>%
  mutate(
    policy = parse_number(policy),
    country.name = str_replace(raw.lbl, "^.+?\\.", ""),
    iso3c = case_when(
      raw.lbl == "XM.Euro.area" ~ "EMU",
      TRUE ~ countrycode(country.name, origin = "country.name", destination = "iso3c")
    )
  ) %>%
  mutate(mon = month(date)) %>%
  filter(mon %in% c(1, 4, 7, 10)) %>%
  dplyr::select(date, iso3c, policy)

r.nom.wghtd <- r.nom.long %>%
  mutate(year = year(date)) %>%
  left_join(w.GDP.long %>% dplyr::select(iso3c, year, weight), by = c("iso3c", "year")) %>%
  filter(!is.na(weight), !is.na(policy)) %>%
  mutate(policy.wghtd = weight * policy)

r.nom.wrld.Q <- r.nom.wghtd %>%
  group_by(date) %>%
  summarise(r.nom.wrld = sum(policy.wghtd, na.rm = TRUE), .groups = "drop") %>%
  arrange(date)

# real world short rate ----

pi.exp <- read.csv(file.path(raw, "OECD_Inflation forecast.csv"),
  sep = ";", skip = 2, strip.white = TRUE
) %>%
  rename(
    OECD_CPI_FORECAST = OECD,
    stamp             = Category
  ) %>%
  mutate(
    OECD_CPI_FORECAST = parse_number(OECD_CPI_FORECAST, locale = locale(decimal_mark = ",")),
    quarter = as.yearqtr(as.Date(substr(stamp, 1, 10), format = "%Y-%m-%d"))
  ) %>%
  dplyr::select(quarter, OECD_CPI_FORECAST)

r.nom.wrld.Q <- r.nom.wrld.Q %>%
  rename(WRLD_NOM_RATE = r.nom.wrld, quarter = date) %>%
  mutate(quarter = as.yearqtr(quarter, format = "%Y-%m-%d"))

real.world.rate.Q <- r.nom.wrld.Q %>%
  left_join(pi.exp, by = "quarter") %>%
  mutate(real.rate = WRLD_NOM_RATE - OECD_CPI_FORECAST)

cat("mean real world rate:", mean(real.world.rate.Q$real.rate), "\n")
cat("sd real world rate:  ", sd(real.world.rate.Q$real.rate), "\n")

write.csv(real.world.rate.Q, file.path(path$build$macro, "world_real_short_rate.csv"), row.names = FALSE)

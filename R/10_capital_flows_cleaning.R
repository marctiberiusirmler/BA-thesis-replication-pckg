# 10_capital_flows_cleaning.R
#
# Builds regional gross capital inflow/outflow panels by asset class from
# IMF BOP data, and a "global inflows as % of world GDP" series. Feeds
# Figures 1-3, B.1-B.3, Tables 1-2, and (via Heatmaps.R / Plot GFAC.R) the
# heatmap and overlay figures.

source(here::here("R", "00_config.R"))

suppressMessages({
  library(dplyr)
  library(stringr)
  library(openxlsx)
  library(tidyr)
  library(purrr)
  library(zoo)
  library(readr)
})

raw <- path$raw$capital_flows

BOP <- read.csv(file.path(raw, "BOP_04-25-2025 12-12-16-14_timeSeries.csv"), header = TRUE)

BOP.1990 <- BOP %>%
  dplyr::select(-matches("^X(19[0-8][0-9])Q[1-4]$")) # drop 1950-1989 quarters

codes <- read.xlsx(file.path(raw, "imf_country_codes.xlsx"), sheet = 1)
codes.incl <- na.omit(codes$`ISO.Code.(incl.)`) # codes of the 54 included countries

BOP.1990 <- BOP.1990 %>%
  mutate(Country.Code = as.numeric(Country.Code)) %>%
  filter(Country.Code %in% codes.incl) %>%
  filter(Attribute %in% "Value")

NorthAm.codes <- na.omit(codes$NorthAmerica)
LatAm.codes <- na.omit(codes$LatinAmerica)
CEEU.codes <- na.omit(codes$CE.Europe)
WEU.codes <- na.omit(codes$W.Europe)
EmAsia.codes <- na.omit(codes$Emerging.Asia)
AsiaPac.codes <- na.omit(codes$Asia.Pacific)
AfricaME.codes <- na.omit(codes$Africa.ME)

all.regions <- list(
  "North America" = BOP.1990 %>% filter(Country.Code %in% NorthAm.codes),
  "CE Europe"     = BOP.1990 %>% filter(Country.Code %in% CEEU.codes),
  "W Europe"      = BOP.1990 %>% filter(Country.Code %in% WEU.codes),
  "Em. Asia"      = BOP.1990 %>% filter(Country.Code %in% EmAsia.codes),
  "Asia-Pacific"  = BOP.1990 %>% filter(Country.Code %in% AsiaPac.codes),
  "Latin America" = BOP.1990 %>% filter(Country.Code %in% LatAm.codes),
  "Africa-ME"     = BOP.1990 %>% filter(Country.Code %in% AfricaME.codes)
)

flows_long <- imap_dfr(all.regions, ~
  .x %>%
    mutate(region = .y) %>%
    mutate(across(starts_with("X"), ~ parse_number(as.character(.x)))) %>%
    pivot_longer(cols = starts_with("X"), names_to = "quarter_raw", values_to = "value") %>%
    mutate(
      quarter = str_remove(quarter_raw, "^X"),
      quarter = as.yearqtr(quarter, format = "%YQ%q")
    ) %>%
    dplyr::select(-quarter_raw))

flows_long <- flows_long %>%
  mutate(
    flow_type = case_when(
      str_detect(Indicator.Name, regex("Liabilities", ignore_case = TRUE)) ~ "Liability", # inflows
      str_detect(Indicator.Name, "Assets") ~ "Asset", # outflows
      TRUE ~ NA_character_
    ),
    asset_class = case_when(
      str_detect(Indicator.Code, "^BFD") ~ "FDI",
      str_detect(Indicator.Code, "E_") ~ "Port. Equity",
      str_detect(Indicator.Code, "D_") ~ "Port. Debt",
      str_detect(Indicator.Code, "^BFO") ~ "Other Inv",
      TRUE ~ NA_character_
    )
  )

flows_regional <- flows_long %>%
  group_by(region, flow_type, asset_class, quarter) %>%
  summarise(value = sum(value, na.rm = TRUE), .groups = "drop")

label_fun <- function(region, flow_type, asset_class) sprintf("%s %s %s", region, flow_type, asset_class)

make_wide <- function(df) {
  df %>%
    mutate(series = label_fun(region, flow_type, asset_class)) %>%
    dplyr::select(quarter, series, value) %>%
    pivot_wider(names_from = series, values_from = value) %>%
    arrange(quarter)
}

inflows <- flows_regional %>%
  filter(flow_type == "Liability") %>%
  make_wide()
outflows <- flows_regional %>%
  filter(flow_type == "Asset") %>%
  make_wide()
outflows <- replace(outflows, outflows == 0, NA)

write.csv(inflows, file.path(path$build$capital_flows, "gross_capital_inflows.csv"), row.names = FALSE)
write.csv(outflows, file.path(path$build$capital_flows, "gross_capital_outflows.csv"), row.names = FALSE)

# Global inflows as a percentage of world GDP ----

GDP.wrld.Y <- read.csv(file.path(raw, "IMF.WRLD.GDP.Y.csv"), header = TRUE)
GDP.wrld.Y <- subset(GDP.wrld.Y, select = c(TIME_PERIOD, OBS_VALUE))
colnames(GDP.wrld.Y) <- c("year", "gdp")

GDP.wrld.Q <- GDP.wrld.Y %>%
  uncount(4, .id = "q") %>%
  mutate(
    quarter = as.yearqtr(year) + (q - 1) / 4,
    gdp.q = gdp / 4 # divide by four to obtain correct reference for quarterly flow data
  ) %>%
  dplyr::select(quarter, gdp.q)

inflows.glbl.Q <- inflows %>%
  pivot_longer(-quarter, names_to = "series", values_to = "value") %>%
  mutate(asset.class = case_when(
    str_detect(series, "FDI") ~ "FDI",
    str_detect(series, "Port\\. Equity") ~ "Port. Equity",
    str_detect(series, "Port\\. Debt") ~ "Port. Debt",
    str_detect(series, "Other Inv") ~ "Other Inv"
  )) %>%
  group_by(quarter, asset.class) %>%
  summarise(flow = sum(value, na.rm = TRUE), .groups = "drop")

inflows.glbl.pctGDP.Q <- inflows.glbl.Q %>%
  left_join(GDP.wrld.Q, by = "quarter") %>%
  mutate(pctGDP = 100 * flow / gdp.q)

write.csv(inflows.glbl.pctGDP.Q, file.path(path$build$capital_flows, "global_inflows_pct_gdp.csv"), row.names = FALSE)

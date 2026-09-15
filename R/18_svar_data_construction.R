# 18_svar_data_construction.R
#
# Builds the SVAR datasets: Rey (2013)-style 7-variable set
# (VAR_DATA_rey: GDP, GDPDEF, CREDIT, INFLOWS, EULEV, FFR, VIX; no
# transforms, truncated to <=2012Q4) and the "adjusted" 7-variable set that
# the rest of the SVAR pipeline uses (VAR_DATA_upd: GDP, GDPDEF,
# CREDIT, INFLOWS, BDLEV, WXSR, VIX), plus a
# fully-differenced/stationary variant (VAR_DATA_stat). Feeds the SVAR
# (Figures 8, 9, D.1-D.4) via matlab/main_svar.m and 19_chow_fevd.R.
#
# Reads outputs of 08 (global_domestic_credit.csv),
# 09 (leverage_ea12_median.csv, for the Rey spec) plus inputs
# from data/raw/svar/ (US GDP/GDPDEF, FRB Z1 broker-dealer leverage,
# FEDFUNDS, Wu-Xia shadow rate).

source(here::here("R", "00_config.R"))

suppressMessages({
  library(zoo)
  library(tidyverse)
  library(lubridate)
  library(tseries)
  library(readr)
  library(readxl)
  library(ggplot2)
})

svar_raw <- path$raw$svar
out <- path$build$svar

quarters <- seq(from = as.yearqtr("1990 Q1"), to = as.yearqtr("2024 Q4"), by = 0.25)

# GDP ----

GDP <- read.csv(file.path(svar_raw, "US GDP.Q.csv"), skip = 3) %>%
  dplyr::select(-c(Line, X2025)) %>%
  slice(2) %>%
  dplyr::select(-X) %>%
  pivot_longer(cols = everything(), names_to = "quarter", values_to = "gdp") %>%
  mutate(
    quarter = seq(as.yearqtr("1990 Q1"), as.yearqtr("2024 Q4"), 0.25),
    gdp_logdiff = c(NA, diff(log(as.numeric(gdp)))),
    gdp_diff = c(NA, diff(as.numeric(gdp)))
  )

cat("ADF, GDP level (expect: cannot reject non-stationarity):\n")
print(adf.test(GDP$gdp))
gdp_adf <- GDP %>%
  slice(-1) %>%
  dplyr::select(gdp_diff)
cat("ADF, GDP diff (expect: reject non-stationarity):\n")
print(adf.test(gdp_adf$gdp_diff))

# GDPDEF ----

GDPDEF <- read.csv(file.path(svar_raw, "US GDPDEF.Q.csv"), skip = 3) %>%
  dplyr::select(-c(Line, X2025)) %>%
  slice(2) %>%
  dplyr::select(-X) %>%
  pivot_longer(cols = everything(), names_to = "quarter", values_to = "gdpdef") %>%
  mutate(
    quarter = seq(as.yearqtr("1990 Q1"), as.yearqtr("2024 Q4"), 0.25),
    gdpdef_logdiff = c(NA, diff(log(as.numeric(gdpdef))))
  )

cat("ADF, GDPDEF level:\n")
print(adf.test(GDPDEF$gdpdef))
gdpdef_adf <- GDPDEF %>%
  slice(-1) %>%
  dplyr::select(gdpdef_logdiff)
cat("ADF, GDPDEF logdiff:\n")
print(adf.test(gdpdef_adf$gdpdef_logdiff))

# GDC (CREDIT) ----

CREDIT <- read.csv(file.path(path$build$credit, "global_domestic_credit.csv")) %>%
  mutate(quarter = as.yearqtr(date), gdc_log = log(global_ndc_usd_bil), gdc_logdiff = c(NA, diff(gdc_log))) %>%
  dplyr::select(quarter, gdc_log, gdc_logdiff)

cat("ADF, CREDIT log level:\n")
print(adf.test(CREDIT$gdc_log))
gdc_adf <- CREDIT %>%
  slice(-1) %>%
  dplyr::select(gdc_logdiff)
cat("ADF, CREDIT logdiff:\n")
print(adf.test(gdc_adf$gdc_logdiff))

# INFLOWS ----

INFLOWS <- read.csv(file.path(path$raw$misc, "BIS_LBS_credit_nonbank.csv"), skip = 15)
colnames(INFLOWS)[1] <- "quarter"

INFLOWS <- INFLOWS %>%
  slice(-c(1:4)) %>%
  mutate(across(-1, ~ na_if(.x, ""))) %>%
  mutate(across(-1, as.numeric)) %>%
  mutate(
    quarter = as.yearqtr(quarter, format = "%Y-%m-%d"),
    cb_credit = rowSums(across(-1), na.rm = TRUE) / 1000,
    inflows = c(NA, diff(cb_credit)),
    inflows_logdiff = c(NA, diff(log(cb_credit)))
  ) %>%
  dplyr::select(quarter, inflows, inflows_logdiff)

inflows_adf <- INFLOWS %>% slice(-1)
cat("ADF, INFLOWS diff:\n")
print(adf.test(inflows_adf$inflows))

# EULEV (Rey spec only) ----

EULEV <- read.csv(file.path(path$build$credit, "leverage_ea12_median.csv"))

# US BD Leverage ----
# constructed from total financial liabilities (FL664190005.Q) and total
# financial assets (FL664090005.Q), Fed Flow of Funds

BDLEV <- read.csv(file.path(svar_raw, "FRB_Z1_brokers_dealers.csv"), skip = 5) %>%
  mutate(
    quarter = as.yearqtr(Time.Period),
    bdlev = FL664090005.Q / (FL664090005.Q - FL664190005.Q),
    bdlev_log = log(bdlev),
    bdlev_logdiff = c(NA, diff(bdlev_log))
  ) %>%
  dplyr::select(quarter, bdlev, bdlev_log, bdlev_logdiff)

cat("ADF, BDLEV level:\n")
print(adf.test(BDLEV$bdlev))
cat("ADF, BDLEV log level:\n")
print(adf.test(BDLEV$bdlev_log))
bdlev_adf <- BDLEV %>% slice(-1)
cat("ADF, BDLEV logdiff:\n")
print(adf.test(bdlev_adf$bdlev_logdiff))

# FFR ----

FFR <- read.csv(file.path(svar_raw, "FEDFUNDS.csv")) # monthly data
EoQ.mths <- as.Date(quarters) %m+% months(2)
FFR <- FFR %>% filter(as.Date(observation_date) %in% EoQ.mths)

# Wu-Xia Shadow rate ----

WXSR <- read_excel(file.path(svar_raw, "WuXiaShadowRate.xlsx"), sheet = 2) %>%
  rename(date = "...1", WXSR = "Wu-Xia shadow federal funds rate (last business day of month)") %>%
  filter(date >= as.Date("1990-01-01")) %>%
  dplyr::select(date, WXSR) %>%
  mutate(date = as.Date(date), quarter = as.yearqtr(date)) %>%
  group_by(quarter) %>%
  slice_max(order_by = date, n = 1, with_ties = FALSE) %>%
  ungroup() %>%
  filter(quarter <= as.yearqtr("2021 Q4")) %>%
  dplyr::select(quarter, rate = WXSR)

# append FFR post-2022Q1: FFR was raised and the WX shadow rate was discontinued
FFR_sub <- FFR %>%
  mutate(quarter = as.yearqtr(observation_date, format = "%Y-%m-%d")) %>%
  filter(quarter >= as.yearqtr("2022 Q1"), quarter <= as.yearqtr("2024 Q4")) %>%
  transmute(quarter, rate = FEDFUNDS)

WXSR_full <- bind_rows(WXSR, FFR_sub) %>% arrange(quarter)

theme_times_new_roman <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Times New Roman") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5, linetype = 1)
    )
}

plot_wxsr_ffr <- ggplot() +
  geom_line(data = FFR, aes(x = as.Date(observation_date), y = FEDFUNDS, colour = "Effective FFR", linetype = "Effective FFR"), linewidth = 0.5) +
  geom_line(data = WXSR_full, aes(x = as.Date(quarter), y = rate, colour = "Wu-Xia Shadow Rate", linetype = "Wu-Xia Shadow Rate"), linewidth = 0.5) +
  geom_hline(yintercept = 0, colour = "gray40", linetype = "dotted", linewidth = 0.4) +
  scale_colour_manual(name = NULL, values = c("Effective FFR" = "black", "Wu-Xia Shadow Rate" = "red"), breaks = c("Effective FFR", "Wu-Xia Shadow Rate")) +
  scale_linetype_manual(name = NULL, values = c("Effective FFR" = "solid", "Wu-Xia Shadow Rate" = "dashed"), breaks = c("Effective FFR", "Wu-Xia Shadow Rate")) +
  labs(x = NULL, y = "Percent", title = NULL) +
  scale_x_date(date_breaks = "5 years", date_labels = "%Y", expand = c(0, 0)) +
  theme_times_new_roman(base_size = 12) +
  theme(legend.position = "bottom", axis.title.x = element_text(margin = margin(t = 6)))

ggsave(file.path(path$output$figures, "supp_wxsr_vs_effective_ffr.png"), plot = plot_wxsr_ffr, width = 6, height = 3.5, dpi = 300)

# VIX ----

VIX <- read.csv(file.path(path$build$vix, "vix_quarterly.csv")) %>%
  mutate(quarter = as.yearqtr(DATE, format = "%Y-%m-%d"), vix_log = log(CLOSE), vix_logdiff = c(NA, diff(vix_log)))

cat("ADF, VIX log level:\n")
print(adf.test(VIX$vix_log))
vix_adf <- VIX %>% slice(-1)
cat("ADF, VIX logdiff:\n")
print(adf.test(vix_adf$vix_logdiff))

# combine in VAR_DATA ----

VAR_DATA_rey_dated <- tibble(
  quarter = as.Date.yearqtr(quarters),
  GDP = as.numeric(GDP$gdp), GDPDEF = as.numeric(GDPDEF$gdpdef),
  CREDIT = as.numeric(CREDIT$gdc_log), INFLOWS = as.numeric(INFLOWS$inflows),
  EULEV = as.numeric(EULEV$eu_med_leverage), FFR = as.numeric(FFR$FEDFUNDS), VIX = as.numeric(VIX$vix_log)
) %>%
  filter(quarter <= as.Date("2012-12-31")) %>%
  slice(-1)

VAR_DATA_rey <- VAR_DATA_rey_dated %>% dplyr::select(-quarter)

VAR_DATA_upd <- tibble(
  GDP = GDP$gdp, GDPDEF = GDPDEF$gdpdef, CREDIT = CREDIT$gdc_log,
  INFLOWS = INFLOWS$inflows_logdiff, BDLEV = BDLEV$bdlev, WXSR = WXSR_full$rate, VIX = VIX$vix_log
) %>%
  mutate(across(everything(), as.numeric)) %>%
  slice(-1)

VAR_DATA_stat <- tibble(
  GDP = GDP$gdp_logdiff, GDPDEF = GDPDEF$gdpdef_logdiff, CREDIT = CREDIT$gdc_logdiff,
  INFLOWS = INFLOWS$inflows_logdiff, BDLEV = BDLEV$bdlev_logdiff, WXSR = WXSR_full$rate, VIX = VIX$vix_logdiff
) %>%
  mutate(across(everything(), as.numeric)) %>%
  slice(-1)

# Diagnostic series plots ----

quarter_axis_rey <- VAR_DATA_rey_dated$quarter
quarter_axis_upd <- as.Date.yearqtr(quarters)[-1]

pdf(file.path(path$output$figures, "supp_svar_series_rey_specification.pdf"), width = 8.27, height = 11.69)
par(mfrow = c(4, 2), mar = c(2, 2, 2, 1))
for (v in names(VAR_DATA_rey)) plot(quarter_axis_rey, VAR_DATA_rey[[v]], type = "l", main = v, xlab = "", ylab = "")
dev.off()

pdf(file.path(path$output$figures, "supp_svar_series_adjusted_specification.pdf"), width = 8.27, height = 11.69)
par(mfrow = c(4, 2), mar = c(2, 2, 2, 1))
for (v in names(VAR_DATA_upd)) plot(quarter_axis_upd, VAR_DATA_upd[[v]], type = "l", main = v, xlab = "", ylab = "")
dev.off()

# export ----

write.csv(VAR_DATA_rey, file.path(out, "svar_data_rey_spec.csv"), row.names = FALSE)
write.csv(VAR_DATA_upd, file.path(out, "svar_data_adjusted_spec.csv"), row.names = FALSE)
write.csv(VAR_DATA_stat, file.path(out, "svar_data_stationary.csv"), row.names = FALSE) # fix: was writing VAR_DATA_upd again -- see header note

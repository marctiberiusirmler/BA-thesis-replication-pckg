# 19_chow_fevd.R
#
# An independent R cross-check of the MATLAB SVAR (matlab/main_svar.m):
# fits full/pre/post VARs on VAR_DATA_upd.csv via `vars::VAR` (Cholesky
# order GDP, GDPDEF, CREDIT, INFLOWS, BDLEV, WXSR, VIX -- the same ordering
# restored in main_svar.m's `cholSet`, see that file's header), runs a Chow
# break test at 2009Q2, and reports FEVD shares. The thesis (p. 26) quotes
# the Chow test p-value ("< 2x10^-16") and FEVD shares ("almost 6%" full
# sample, "roughly 1%" pre-GFC, "below 1%" post-GFC) from this script's
# console output
#
# Reads the output of 18_svar_data_construction.R.

source(here::here("R", "00_config.R"))

suppressMessages({
  library(dplyr)
  library(readr)
  library(vars)
  library(svars)
})

VAR_DATA_upd <- read.csv(file.path(path$build$svar, "svar_data_adjusted_spec.csv"))

sink(file.path(path$output$tables, "supp_svar_chow_test_and_fevd.txt"))

v_full <- vars::VAR(VAR_DATA_upd, p = 2, type = "const")
x_full <- id.chol(v_full, order_k = c("GDP", "GDPDEF", "CREDIT", "INFLOWS", "BDLEV", "WXSR", "VIX"))
cat("=== Full-sample Cholesky-identified SVAR ===\n")
print(summary(x_full))

cat("\n=== Chow break test (SB = 77, i.e. 2009Q2) ===\n")
z_full <- chow.test(v_full, SB = 77)
print(summary(z_full))

SB <- 77L
VAR_DATA_upd <- VAR_DATA_upd %>% mutate(.t = row_number())
VAR_DATA_pre <- VAR_DATA_upd %>%
  filter(.t <= SB) %>%
  dplyr::select(-.t)
VAR_DATA_post <- VAR_DATA_upd %>%
  filter(.t > SB) %>%
  dplyr::select(-.t)

v_pre <- vars::VAR(VAR_DATA_pre, p = 1, type = "const")
v_post <- vars::VAR(VAR_DATA_post, p = 1, type = "const")

x_pre <- id.chol(v_pre, order_k = c("GDP", "GDPDEF", "CREDIT", "INFLOWS", "BDLEV", "WXSR", "VIX"))
x_post <- id.chol(v_post, order_k = c("GDP", "GDPDEF", "CREDIT", "INFLOWS", "BDLEV", "WXSR", "VIX"))

H <- 20
fe_full <- fevd(x_full, n.ahead = H)
fe_pre <- fevd(x_pre, n.ahead = H)
fe_post <- fevd(x_post, n.ahead = H)

cat("\n=== FEVD: max share of VIX forecast-error variance attributable to WXSR ===\n")
cat("Full sample (thesis: almost 6%, at horizon 3):   ", max(fe_full$VIX["WXSR"]), "\n")
cat("Pre-GFC    (thesis: roughly 1%, at horizon 12):  ", max(fe_pre$VIX["WXSR"]), "\n")
cat("Post-GFC   (thesis: remains below 1%):           ", max(fe_post$VIX["WXSR"]), "\n")

cat("\n=== Full FEVD tables (VIX row, all horizons) ===\n")
cat("-- Full sample --\n")
print(fe_full$VIX)
cat("-- Pre-GFC --\n")
print(fe_pre$VIX)
cat("-- Post-GFC --\n")
print(fe_post$VIX)

sink()

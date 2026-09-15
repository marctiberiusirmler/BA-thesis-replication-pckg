# 12_correlation_tables.R
#
# Computes unconditional and rate/GDP-growth-conditional correlations of VIX
# with regional capital inflows, and of VIX with credit growth/leverage by
# block. Produces Tables 1, 2, and 3.
#
# Not network-dependent: reads outputs of 03 (VIX), 11 (Inflows), 02 (real
# world rate), 01 (GDP growth), 08 (Block DC), 09 (Leverage by block -- the
# Table-3-matching script), and 10 (Leverage by block_updated, kept only for
# the `lev_qoq_upd` comparison column, per the original).

source(here::here("R", "00_config.R"))

suppressMessages({
  library(tidyverse)
  library(zoo)
  library(broom)
})

vix_dir <- path$build$vix
flows_dir <- path$build$capital_flows
macro_dir <- path$build$macro
credit_dir <- path$build$credit
out_dir <- path$output$tables

to_yrqtr <- function(x, fmt = "%Y Q%q") as.yearqtr(x, format = fmt)

get_corr <- function(x, y) {
  t <- cor.test(x, y, use = "complete.obs", method = "pearson", alternative = "two.sided", conf.level = 0.95)
  tibble(estimate = unname(t$estimate), p.value = t$p.value)
}

get_cond_corr <- function(df, y, cond_vars) {
  f_vix <- as.formula(paste("VIX ~", paste(cond_vars, collapse = " + ")))
  f_y <- as.formula(paste(y, "~", paste(cond_vars, collapse = " + ")))
  vx <- resid(lm(f_vix, data = df))
  vy <- resid(lm(f_y, data = df))
  t <- cor.test(vy, vx, use = "complete.obs", method = "pearson", alternative = "two.sided", conf.level = 0.95)
  tibble(estimate = unname(t$estimate), p.value = t$p.value)
}

get_cond_corr_lag <- function(df, y, cond_vars) {
  f_vix <- as.formula(paste("VIX_lag ~", paste(cond_vars, collapse = " + ")))
  f_y <- as.formula(paste(y, "~", paste(cond_vars, collapse = " + ")))
  vx <- resid(lm(f_vix, data = df))
  vy <- resid(lm(f_y, data = df))
  t <- cor.test(vy, vx, use = "complete.obs", method = "pearson", alternative = "two.sided", conf.level = 0.95)
  tibble(estimate = unname(t$estimate), p.value = t$p.value)
}

vix_full <- read_csv(file.path(vix_dir, "vix_quarterly.csv"), show_col_types = FALSE) %>%
  mutate(date = as.yearqtr(DATE, format = "%Y-%m-%d"), VIX = CLOSE, VIX_lag = dplyr::lag(VIX)) %>%
  dplyr::select(date, VIX, VIX_lag)

inflows_full <- read_csv(file.path(flows_dir, "gross_capital_inflows.csv"), show_col_types = FALSE) %>%
  mutate(date = to_yrqtr(quarter)) %>%
  slice(-141) %>% # drop NA row
  dplyr::select(-quarter) %>%
  relocate(date)

rate_full <- read_csv(file.path(macro_dir, "world_real_short_rate.csv"), show_col_types = FALSE) %>%
  mutate(date = to_yrqtr(quarter)) %>%
  dplyr::select(-quarter) %>%
  relocate(date)

gdp_full <- read_csv(file.path(macro_dir, "world_gdp_growth.csv"), show_col_types = FALSE) %>%
  mutate(date = to_yrqtr(quarter)) %>%
  dplyr::select(-quarter) %>%
  relocate(date)

dc <- read_csv(file.path(credit_dir, "domestic_credit_by_block.csv"), show_col_types = FALSE) %>%
  mutate(date = to_yrqtr(date)) %>%
  relocate(date)

lev <- read_csv(file.path(credit_dir, "leverage_by_block.csv"), show_col_types = FALSE) %>%
  mutate(date = to_yrqtr(date)) %>%
  relocate(date)

lev_upd <- read_csv(file.path(credit_dir, "leverage_by_block_country_growth_median.csv"), show_col_types = FALSE) %>%
  relocate(date) %>%
  mutate(date = as.yearqtr(date)) %>%
  rename(lev_qoq_upd = lev_qoq) %>%
  dplyr::select(-med_leverage)

splits <- tribble(
  ~sample,   ~start,                ~end,
  "full",    as.yearqtr("1990 Q1"), as.yearqtr("2024 Q4"),
  "preGFC",  as.yearqtr("1990 Q1"), as.yearqtr("2009 Q2"),
  "postGFC", as.yearqtr("2009 Q3"), as.yearqtr("2024 Q4")
)

panel_IF <- inflows_full %>%
  left_join(vix_full, by = "date") %>%
  left_join(rate_full, by = "date") %>%
  left_join(gdp_full, by = "date") %>%
  crossing(splits) %>%
  filter(date >= start, date <= end)

panel_DC <- dc %>%
  left_join(lev, by = c("block", "date")) %>%
  left_join(lev_upd, by = c("block", "date")) %>%
  left_join(vix_full, by = "date") %>%
  left_join(rate_full, by = "date") %>%
  left_join(gdp_full, by = "date") %>%
  crossing(splits) %>%
  filter(date >= start, date <= end)

# Table 1 -- unconditional correlations (Inflows x VIX)
targets_IF <- names(inflows_full)[-1]

uncond_IF <- panel_IF %>%
  pivot_longer(all_of(targets_IF), names_to = "region_asset", values_to = "value") %>%
  group_by(sample, region_asset) %>%
  summarize(get_corr(value, VIX), .groups = "drop")

uncond_IF_corr_tbl <- uncond_IF %>%
  dplyr::select(-p.value) %>%
  mutate(estimate = round(estimate, 2)) %>%
  pivot_wider(names_from = sample, values_from = estimate)
uncond_IF_p_tbl <- uncond_IF %>%
  dplyr::select(-estimate) %>%
  mutate(p.value = round(p.value, 3)) %>%
  pivot_wider(names_from = sample, values_from = p.value)

# Table 2 -- conditional correlations on real rate & world GDP growth
cond_vars <- c("real.rate", "QoQ.pct")

cond_IF <- panel_IF %>%
  pivot_longer(all_of(targets_IF), names_to = "region_asset", values_to = "value") %>%
  group_by(sample, region_asset) %>%
  group_modify(~ get_cond_corr(.x, "value", cond_vars)) %>%
  ungroup()

cond_IF_corr_tbl <- cond_IF %>%
  dplyr::select(-p.value) %>%
  mutate(estimate = round(estimate, 2)) %>%
  pivot_wider(names_from = sample, values_from = estimate)
cond_IF_p_tbl <- cond_IF %>%
  dplyr::select(-estimate) %>%
  mutate(p.value = sprintf("%.3f", p.value)) %>%
  pivot_wider(names_from = sample, values_from = p.value)

# Table 3 -- domestic credit growth & leverage by block, conditional on lagged VIX
targets_DC <- c("ndc_qoq", "med_leverage", "lev_qoq", "lev_qoq_upd")

cond_DC <- panel_DC %>%
  pivot_longer(all_of(targets_DC), names_to = "variable", values_to = "value") %>%
  group_by(sample, block, variable) %>%
  group_modify(~ get_cond_corr_lag(.x, "value", cond_vars)) %>%
  ungroup()

cond_DC_corr_tbl <- cond_DC %>%
  dplyr::select(-p.value) %>%
  mutate(estimate = round(estimate, 2)) %>%
  pivot_wider(names_from = sample, values_from = estimate)
cond_DC_p_tbl <- cond_DC %>%
  dplyr::select(-estimate) %>%
  mutate(p.value = sprintf("%.3f", p.value)) %>%
  pivot_wider(names_from = sample, values_from = p.value)

write.csv(uncond_IF_corr_tbl, file.path(out_dir, "table01_unconditional_correlations_inflows_vix.csv"))
write.csv(uncond_IF_p_tbl, file.path(out_dir, "table01_unconditional_correlations_inflows_vix_pvalues.csv"))
write.csv(cond_IF_corr_tbl, file.path(out_dir, "table02_partial_correlations_inflows_vix.csv"))
write.csv(cond_IF_p_tbl, file.path(out_dir, "table02_partial_correlations_inflows_vix_pvalues.csv"))
write.csv(cond_DC_corr_tbl, file.path(out_dir, "table03_partial_correlations_credit_leverage.csv"))
write.csv(cond_DC_p_tbl, file.path(out_dir, "table03_partial_correlations_credit_leverage_pvalues.csv"))

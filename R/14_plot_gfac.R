# 14_plot_gfac.R
#
# Sequence of plots and correlation
# checks (global inflows vs. -VIX, GFAC vs. MAR/MANR global factors, DFM
# factor loadings by region, GFAC/VIX/MAR overlay, domestic credit & leverage
# by block, GDC vs. cross-border credit, EA-12 leverage, US broker-dealer
# leverage, and GFAC vs. the broad USD index). Produces Figures 4, 5, 6, 7,
# 10, and several correlation figures quoted directly in the thesis text.
#
# Not network-dependent: reads outputs of 05, 11, 04, 12, 06/07 (ticker
# file), 03, 08, 09, and the hand-obtained inputs in data/raw/misc/
# (DTWEXBGS.csv, IFDP_Note_Data_Appendix.xlsx,
# see DATA.md) and data/raw/svar/FRB_Z1_brokers_dealers.csv.

source(here::here("R", "00_config.R"))

suppressMessages({
  library(ggplot2)
  library(zoo)
  library(dplyr)
  library(tidyr)
  library(patchwork)
  library(lubridate)
  library(readr)
  library(readxl)
  library(tibble)
  library(forcats)
  library(stringr)
  library(scales)
})

misc_dir <- path$raw$misc
out_fig <- path$output$figures
sink(file.path(path$output$tables, "supp_intext_correlations_gfac_credit_vix.txt"))

recessions <- read.csv(file.path(path$build$misc, "nber_recessions.csv"))
recessions$start <- as.Date(recessions$start)
recessions$end <- as.Date(recessions$end)

theme_times_new_roman <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Times New Roman") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5, linetype = 1)
    )
}

# Plot of global Inflows and VIX (Figure 4) ----

Inflows.glbl.pctGDP <- read.csv(file.path(path$build$capital_flows, "global_inflows_pct_gdp.csv")) %>%
  mutate(quarter = as.yearqtr(quarter), asset.class = dplyr::recode(asset.class,
    "Port. Equity" = "Equity", "Port. Debt" = "Debt", "Other Inv" = "Credit"
  ))

VIX.neg.Q <- read.csv(file.path(path$build$vix, "vix_quarterly_negated.csv")) %>%
  mutate(DATE = ymd(DATE), quarter = as.yearqtr(DATE)) %>%
  dplyr::select(quarter, CLOSE.neg)

Credit.glbl.pctGDP <- Inflows.glbl.pctGDP %>%
  filter(asset.class == "Credit") %>%
  slice(-141)
cat("Credit vs -VIX, full sample:\n")
print(cor(Credit.glbl.pctGDP$pctGDP, VIX.neg.Q$CLOSE.neg, use = "complete.obs"))

Credit.glbl.pctGDP.rey <- Credit.glbl.pctGDP %>% filter(quarter < 2013)
VIX.neg.Q.rey <- VIX.neg.Q %>% filter(quarter < 2013)
cat("Credit vs -VIX, Rey sample (<2013):\n")
print(cor(Credit.glbl.pctGDP.rey$pctGDP, VIX.neg.Q.rey$CLOSE.neg, use = "complete.obs"))

Credit.glbl.pctGDP.pre <- Credit.glbl.pctGDP %>% filter(quarter <= as.yearqtr("2009 Q2"))
VIX.neg.Q.pre <- VIX.neg.Q %>% filter(quarter <= as.yearqtr("2009 Q2"))
cat("Credit vs -VIX, pre-GFC (thesis p.7: 0.35):\n")
print(cor(Credit.glbl.pctGDP.pre$pctGDP, VIX.neg.Q.pre$CLOSE.neg, use = "complete.obs"))

Credit.glbl.pctGDP.post <- Credit.glbl.pctGDP %>% filter(quarter > as.yearqtr("2009 Q2"))
VIX.neg.Q.post <- VIX.neg.Q %>% filter(quarter > as.yearqtr("2009 Q2"))
cat("Credit vs -VIX, post-GFC (thesis p.7: -0.40):\n")
print(cor(Credit.glbl.pctGDP.post$pctGDP, VIX.neg.Q.post$CLOSE.neg, use = "complete.obs"))

Debt.glbl.pctGDP <- Inflows.glbl.pctGDP %>%
  filter(asset.class == "Debt") %>%
  slice(-141)
cat("Credit vs Debt, full sample:\n")
print(cor(Credit.glbl.pctGDP$pctGDP, Debt.glbl.pctGDP$pctGDP, use = "complete.obs"))

Debt.glbl.pctGDP.pre <- Debt.glbl.pctGDP %>% filter(quarter <= as.yearqtr("2009 Q2"))
cat("Credit vs Debt, pre-GFC:\n")
print(cor(Credit.glbl.pctGDP.pre$pctGDP, Debt.glbl.pctGDP.pre$pctGDP, use = "complete.obs"))
cat("Debt vs -VIX, pre-GFC:\n")
print(cor(Debt.glbl.pctGDP.pre$pctGDP, VIX.neg.Q.pre$CLOSE.neg, use = "complete.obs"))

Debt.glbl.pctGDP.post <- Debt.glbl.pctGDP %>% filter(quarter > as.yearqtr("2009 Q2"))
cat("Credit vs Debt, post-GFC:\n")
print(cor(Credit.glbl.pctGDP.post$pctGDP, Debt.glbl.pctGDP.post$pctGDP, use = "complete.obs"))
cat("Debt vs -VIX, post-GFC:\n")
print(cor(Debt.glbl.pctGDP.post$pctGDP, VIX.neg.Q.post$CLOSE.neg, use = "complete.obs"))

plot_df <- Inflows.glbl.pctGDP %>%
  filter(asset.class %in% c("FDI", "Equity", "Debt", "Credit")) %>%
  dplyr::select(quarter, asset.class, pctGDP) %>%
  left_join(VIX.neg.Q, by = "quarter")

vix1 <- -25
inflow1 <- -10
vix2 <- 0
inflow2 <- 0
scale_fac <- (inflow2 - inflow1) / (vix2 - vix1)
shift_fac <- inflow1 - scale_fac * vix1

vix_df <- plot_df %>%
  dplyr::select(quarter, CLOSE.neg) %>%
  mutate(asset.class = "(-)VIX (rhs)", pctGDP = CLOSE.neg * scale_fac + shift_fac) %>%
  dplyr::select(quarter, asset.class, pctGDP)

final_df <- plot_df %>%
  dplyr::select(quarter, asset.class, pctGDP) %>%
  bind_rows(vix_df) %>%
  mutate(asset.class = factor(asset.class, levels = c("FDI", "Equity", "Debt", "Credit", "(-)VIX (rhs)")))

p <- ggplot(final_df, aes(x = as.Date(quarter), y = pctGDP, colour = asset.class)) +
  geom_line(aes(linetype = asset.class), linewidth = 0.5) +
  scale_colour_manual(values = c("FDI" = "#F0F0F0", "Equity" = "#A0A0A0", "Debt" = "#5A5A5A", "Credit" = "#0A0A0A", "(-)VIX (rhs)" = "black")) +
  scale_linetype_manual(values = c("FDI" = "solid", "Equity" = "solid", "Debt" = "solid", "Credit" = "solid", "(-)VIX (rhs)" = "dashed")) +
  scale_y_continuous(
    name = "Gross Capital Inflows", labels = scales::label_percent(scale = 1),
    sec.axis = sec_axis(transform = ~ (. - shift_fac) / scale_fac, name = "(-)VIX", breaks = c(25, 0, -25, -50))
  ) +
  scale_x_date(limits = as.Date(c("1990-01-01", "2024-10-01")), date_breaks = "5 years", date_labels = "%Y", expand = c(0, 0)) +
  labs(title = NULL, x = NULL, colour = NULL, linetype = NULL) +
  theme_times_new_roman(12) +
  theme(legend.position = "bottom")

ggsave(file.path(out_fig, "supp_inflows_and_vix_alt_scaling.png"), plot = p, width = 8, height = 4.5, dpi = 300)

# Plot of MAR and my GFAC (Figure 6) ----

GFAC.MAR <- read.csv(file.path(path$build$global_factor, "gfac_mar_standardized.csv"))
GFAC.MAR$date <- seq(as.Date("1980-01-01"), as.Date("2024-12-01"), by = "month")
GFAC <- read.csv(file.path(path$build$global_factor, "gfac_standardized.csv"))
GFAC$date <- seq(as.Date("2000-02-01"), as.Date("2024-12-01"), by = "month")

plot <- ggplot() +
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = GFAC.MAR, aes(x = date, y = MAR15, colour = "MAR GFAC", linetype = "MAR GFAC")) +
  geom_line(data = GFAC.MAR, aes(x = date, y = MANR24, colour = "MANR GFAC", linetype = "MANR GFAC")) +
  geom_line(data = GFAC, aes(x = date, y = x, colour = "GFAC", linetype = "GFAC")) +
  scale_colour_manual(name = NULL, values = c("MAR GFAC" = "black", "MANR GFAC" = "black", GFAC = "blue")) +
  scale_linetype_manual(name = NULL, values = c("MAR GFAC" = "solid", "MANR GFAC" = "longdash", GFAC = "solid")) +
  labs(x = NULL, y = "log units, standardized", title = NULL, subtitle = NULL, caption = NULL) +
  scale_x_date(limits = as.Date(c("1980-01-01", "2024-12-01")), date_breaks = "5 years", date_labels = "%Y", expand = c(0, 0)) +
  theme_times_new_roman(base_size = 12) +
  theme(legend.position = "bottom", axis.title.x = element_blank())

ggsave(file.path(out_fig, "figure06_global_factors_risky_asset_prices.png"), plot = plot, width = 8, height = 4.5, dpi = 300)

GFACs <- GFAC.MAR %>% left_join(GFAC, by = "date")
cat("GFAC vs MAR15 (thesis p.15: 0.96):\n")
print(cor(GFACs$x, GFACs$MAR15, use = "complete.obs"))
cat("GFAC vs MANR24 (thesis p.15: 0.64):\n")
print(cor(GFACs$x, GFACs$MANR24, use = "complete.obs"))

# Plot of Factor Loadings (Figure 5) ----

dfm_output <- readRDS(file.path(path$build$global_factor, "dfm_model_output.rds"))
L <- dfm_output$C

tickers_region <- read_excel(file.path(path$raw$asset_prices, "asset prices_ticker.xlsx"))

map_long <- tickers_region %>%
  pivot_longer(everything(), names_to = "Region", values_to = "Ticker", values_drop_na = TRUE) %>%
  mutate(Ticker = str_trim(Ticker), Ticker = if_else(str_starts(Ticker, "\\^"), str_replace(Ticker, "^\\^", "X."), Ticker)) %>%
  distinct(Ticker, .keep_all = TRUE)

loadings_long <- as.data.frame(L) %>%
  rownames_to_column("Ticker") %>%
  pivot_longer(-Ticker, names_to = "Factor", values_to = "Loading")

by_region <- loadings_long %>%
  inner_join(map_long, by = "Ticker") %>%
  group_by(Factor, Region) %>%
  summarize(median_loading = mean(Loading, na.rm = TRUE) * 100, .groups = "drop") %>%
  mutate(Region = fct_recode(Region,
    "North America" = "NorthAm", "Latin America" = "LatAm", "Europe" = "W",
    "Asia" = "Asia", "Australia" = "AUS", "Commodities" = "Cmdty", "Corporate Bonds" = "Corporate"
  )) %>%
  mutate(Region = fct_relevel(Region, c("North America", "Latin America", "Europe", "Asia", "Australia", "Commodities", "Corporate Bonds")))

loading_plot <- ggplot(by_region, aes(x = Region, y = median_loading)) +
  geom_col(fill = "grey") +
  labs(title = NULL, x = NULL, y = "100 x \nMedian loading") +
  theme_times_new_roman(base_size = 12) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(out_fig, "figure05_median_factor_loadings_by_market.png"), plot = loading_plot, width = 6, height = 2, dpi = 300)

# Plot of MAR and my GFAC and the VIX (Figure 7) ----

VIX.M <- read.csv(file.path(path$build$vix, "vix_monthly.csv"))
VIX.M.logst <- VIX.M %>%
  mutate(vix_log = log(CLOSE)) %>%
  mutate(vix_logst = (vix_log - mean(vix_log)) / sd(vix_log))
VIX.M$DATE <- as.Date(VIX.M$DATE)
VIX.M.logst$DATE <- as.Date(VIX.M$DATE)

GFAC.MAR.90 <- GFAC.MAR[GFAC.MAR$date >= as.Date("1990-01-01"), ]
recessions.90 <- recessions[recessions$start >= as.Date("1990-01-01"), ]

plot.VIX <- ggplot() +
  geom_rect(data = recessions.90, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = GFAC.MAR.90, aes(x = date, y = MAR15, colour = "MAR GFAC", linetype = "MAR GFAC"), linewidth = 0.5) +
  geom_line(data = GFAC, aes(x = date, y = x, colour = "GFAC", linetype = "GFAC"), linewidth = 0.5) +
  geom_line(data = VIX.M.logst, aes(x = DATE, y = vix_logst, colour = "VIX", linetype = "VIX"), linewidth = 0.5) +
  scale_colour_manual(name = NULL, values = c("MAR GFAC" = "black", "VIX" = "orange", GFAC = "blue")) +
  scale_linetype_manual(name = NULL, values = c("MAR GFAC" = "solid", "VIX" = "solid", GFAC = "solid")) +
  labs(x = NULL, y = "log units, standardized", title = NULL, subtitle = NULL) +
  scale_x_date(limits = as.Date(c("1990-01-01", "2024-12-01")), date_breaks = "2 years", date_labels = "%Y", expand = c(0, 0)) +
  theme_times_new_roman(base_size = 12) +
  theme(legend.position = "bottom", axis.title.x = element_blank())

ggsave(file.path(out_fig, "figure07_global_factors_and_vix.png"), plot = plot.VIX, width = 8, height = 4.5, dpi = 300)

VIX.M.logst.cor <- VIX.M.logst %>%
  mutate(date = seq(as.Date("1990-01-01"), as.Date("2024-12-01"), by = "month")) %>%
  dplyr::select(date, vix_logst)

GFACs.vix <- GFACs %>% left_join(VIX.M.logst.cor, by = "date")

GFACs.vix.pre <- GFACs.vix %>% filter(date <= as.Date("2009-06-01"))
cat("MAR15 vs VIX (log, std.), pre-GFC (thesis p.16: -0.27):\n")
print(cor(GFACs.vix.pre$MAR15, GFACs.vix.pre$vix_logst, use = "complete.obs"))

GFACs.vix.post <- GFACs.vix %>% filter(date > as.Date("2009-06-01"))
cat("GFAC vs VIX (log, std.), post-GFC (thesis p.16: -0.54):\n")
print(cor(GFACs.vix.post$x, GFACs.vix.post$vix_logst, use = "complete.obs"))

GFACs.vix <- GFACs.vix %>%
  mutate(x_mar15_recon = if_else(is.na(x) | is.na(MAR15), coalesce(MAR15, x), (x + MAR15) / 2))
cat("Reconciled GFAC/MAR15 vs VIX, full sample:\n")
print(cor(GFACs.vix$x_mar15_recon, GFACs.vix$vix_logst, use = "complete.obs"))

# Plot of domestic credit by block and med leverage by block (Figure 10) ----

block_credit <- read.csv(file.path(path$build$credit, "domestic_credit_by_block.csv")) %>%
  mutate(date = as.yearqtr(date, format = "%Y Q%q"), date_d = as.Date(date), block = dplyr::recode(block,
    "Central and E. Europe" = "Central, Eastern Europe", "W. Europe" = "Western Europe", "ME/Africa" = "Africa, Middle East"
  ))

lev_block <- read.csv(file.path(path$build$credit, "leverage_by_block.csv")) %>%
  mutate(date = as.yearqtr(date, format = "%Y Q%q"), date_d = as.Date(date), block = dplyr::recode(block,
    "Central and E. Europe" = "Central, Eastern Europe", "W. Europe" = "Western Europe", "ME/Africa" = "Africa, Middle East"
  ))

linetypes <- c(
  "North America" = "solid", "Latin America" = "dashed", "Central, Eastern Europe" = "dotted",
  "Western Europe" = "longdash", "Emerging Asia" = "twodash", "Asia Pacific" = "dotdash", "Africa, Middle East" = "1234"
)

p1 <- ggplot(block_credit, aes(x = date_d, y = block_ndc_usd_bil, linetype = block, group = block)) +
  geom_rect(data = recessions, aes(xmin = as.Date(start), xmax = as.Date(end), ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(color = "black", linewidth = 0.5) +
  scale_linetype_manual(name = NULL, values = linetypes) +
  scale_x_date(limits = c(as.Date("1990-01-01"), as.Date("2024-12-31")), breaks = seq(as.Date("1990-01-01"), as.Date("2025-01-01"), by = "5 years"), date_labels = "%Y", expand = c(0, 0)) +
  labs(title = NULL, subtitle = NULL, x = NULL, y = NULL, linetype = "Region") +
  theme_times_new_roman(base_size = 12)

p2 <- ggplot(lev_block, aes(x = date_d, y = med_leverage, linetype = block, group = block)) +
  geom_rect(data = recessions, aes(xmin = as.Date(start), xmax = as.Date(end), ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(color = "black", linewidth = 0.5) +
  scale_linetype_manual(name = NULL, values = linetypes) +
  scale_x_date(limits = c(as.Date("1990-01-01"), as.Date("2024-12-31")), breaks = seq(as.Date("1990-01-01"), as.Date("2025-01-01"), by = "5 years"), date_labels = "%Y", expand = c(0, 0)) +
  labs(title = NULL, subtitle = NULL, x = NULL, y = NULL, linetype = "Region") +
  theme_times_new_roman()

combined <- p1 + p2 + plot_layout(ncol = 2, guides = "collect") & theme(legend.position = "bottom")
ggsave(file.path(out_fig, "figure04_domestic_credit_and_leverage_by_region.png"), plot = combined, width = 8, height = 4.5, dpi = 300)

# GDC and Global Cross-Border Credit Inflows ----

gdc <- read_csv(file.path(path$build$credit, "global_domestic_credit.csv"), show_col_types = FALSE) %>%
  mutate(date = as.yearqtr(date, format = "%Y Q%q"), date_d = as.Date(date))

global_infl <- read_csv(file.path(misc_dir, "Global Cross Border Credit Inflows.csv"), show_col_types = FALSE)

p1 <- ggplot() +
  geom_line(data = gdc, aes(x = date_d, y = global_ndc_usd_bil), colour = "black", linewidth = 0.5, linetype = "solid") +
  labs(x = NULL, y = NULL, title = NULL) +
  scale_x_date(limits = c(as.Date("1990-01-01"), as.Date("2024-12-31")), breaks = seq(as.Date("1990-01-01"), as.Date("2025-01-01"), by = "5 years"), date_labels = "%Y", expand = c(0, 0)) +
  theme_times_new_roman(base_size = 12)

p2 <- ggplot() +
  geom_line(data = global_infl, aes(x = quarter, y = inflows), colour = "black", linewidth = 0.5, linetype = "solid") +
  labs(x = NULL, y = NULL, title = NULL) + # fix: original had a duplicate comma here (syntax error) -- see header note
  scale_x_date(limits = c(as.Date("1990-01-01"), as.Date("2024-12-31")), breaks = seq(as.Date("1990-01-01"), as.Date("2025-01-01"), by = "5 years"), date_labels = "%Y", expand = c(0, 0)) +
  theme_times_new_roman(base_size = 12)

combined <- p1 + p2 + plot_layout(ncol = 2, guides = "collect") + plot_annotation(title = NULL, caption = NULL) & theme_times_new_roman(base_size = 12)
ggsave(file.path(out_fig, "supp_domestic_credit_and_cross_border_credit.png"), plot = combined, width = 6.2, height = 3, dpi = 300)

# EA12 Leverage Plot ----

EA12_lev <- read_csv(file.path(path$build$credit, "leverage_ea12_median.csv"), show_col_types = FALSE) %>%
  mutate(date = as.yearqtr(date, format = "%Y Q%q"), date_d = as.Date(date))

plot_EULEV <- ggplot() +
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = EA12_lev, aes(x = date_d, y = eu_med_leverage), colour = "black", linewidth = 0.5, linetype = "solid") +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL, caption = NULL) +
  scale_x_date(limits = c(as.Date("1990-01-01"), as.Date("2024-12-31")), breaks = seq(as.Date("1990-01-01"), as.Date("2025-01-01"), by = "5 years"), date_labels = "%Y", expand = c(0, 0)) +
  theme_times_new_roman(base_size = 12)

ggsave(file.path(out_fig, "supp_ea12_banking_sector_leverage.png"), plot = plot_EULEV, width = 4.5, height = 4.5, dpi = 300)

# US BD Leverage Plot ----

BDLEV <- read.csv(file.path(path$raw$svar, "FRB_Z1_brokers_dealers.csv"), skip = 5) %>%
  mutate(quarter = as.yearqtr(Time.Period), quarter_d = as.Date(quarter), leverage = FL664090005.Q / (FL664090005.Q - FL664190005.Q)) %>%
  dplyr::select(quarter_d, leverage)

plot_BDLEV <- ggplot() +
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(data = BDLEV, aes(x = quarter_d, y = leverage), colour = "black", linewidth = 0.5, linetype = "solid") +
  labs(x = NULL, y = NULL, title = NULL, subtitle = NULL, caption = NULL) +
  scale_x_date(limits = c(as.Date("1990-01-01"), as.Date("2024-12-31")), breaks = seq(as.Date("1990-01-01"), as.Date("2025-01-01"), by = "5 years"), date_labels = "%Y", expand = c(0, 0)) +
  theme_times_new_roman(base_size = 12)

ggsave(file.path(out_fig, "supp_us_broker_dealer_leverage.png"), plot = plot_BDLEV, width = 4.5, height = 4.5, dpi = 300)

combined_lev <- plot_EULEV + plot_BDLEV + plot_layout(ncol = 2, guides = "collect") & theme_times_new_roman(base_size = 12)
ggsave(file.path(out_fig, "supp_ea12_and_broker_dealer_leverage_combined.png"), plot = combined_lev, width = 6, height = 3, dpi = 300)

# GFAC and broad USD index ----

GFAC <- read.csv(file.path(path$build$global_factor, "gfac_standardized.csv"))
GFAC$date <- seq(as.Date("2000-02-01"), as.Date("2024-12-01"), by = "month")

BTWEX.d <- read.csv(file.path(misc_dir, "DTWEXBGS.csv")) %>% mutate(observation_date = as.Date(observation_date))

BTWEX.m <- BTWEX.d %>%
  arrange(observation_date) %>%
  filter(!is.na(DTWEXBGS)) %>%
  group_by(date = floor_date(observation_date, "month")) %>%
  slice_tail(n = 1) %>%
  ungroup() %>%
  transmute(date, usd_broad = DTWEXBGS)

TWEXB.m <- read_excel(file.path(misc_dir, "IFDP_Note_Data_Appendix.xlsx"), sheet = "Nominal Dollar Indexes Monthly") %>%
  mutate(date = as.Date(Period), usd_broad = `Nominal Broad Monthly`) %>%
  dplyr::select(date, usd_broad) %>%
  filter(date >= as.Date("1990-01-01"), date < as.Date("2006-01-01"))

USD <- bind_rows(TWEXB.m, BTWEX.m)

GFAC.MAR15.cut <- GFAC.MAR %>%
  filter(date < as.Date("2001-12-31")) %>%
  dplyr::select(date, MAR15)

plot_df <- data.frame(date = seq(as.Date("1990-01-01"), as.Date("2024-12-01"), by = "month"), usd_broad = USD$usd_broad) %>%
  left_join(GFAC.MAR15.cut, by = "date") %>%
  left_join(GFAC, by = "date")

gfac1 <- -1.6
usd1 <- 75
gfac2 <- 2.4
usd2 <- 125
scale_fac <- (gfac2 - gfac1) / (usd2 - usd1)
shift_fac <- gfac1 - scale_fac * usd1

plot <- ggplot(plot_df, aes(x = date)) +
  geom_rect(data = recessions, aes(xmin = start, xmax = end, ymin = -Inf, ymax = Inf), fill = "grey70", alpha = 0.4, inherit.aes = FALSE) +
  geom_line(aes(y = x, colour = "GFAC", linetype = "GFAC"), na.rm = TRUE) +
  geom_line(aes(y = MAR15, colour = "MAR GFAC", linetype = "MAR GFAC"), na.rm = TRUE) +
  geom_line(aes(y = usd_broad * scale_fac + shift_fac, colour = "Nominal Broad US Dollar Index (rhs)", linetype = "Nominal Broad US Dollar Index (rhs)"), na.rm = TRUE) +
  scale_y_continuous(name = "log units, standardized", sec.axis = sec_axis(transform = ~ (. - shift_fac) / scale_fac, name = "Index Jan 2006 = 100")) +
  scale_colour_manual(name = NULL, values = c("Nominal Broad US Dollar Index (rhs)" = "red", "GFAC" = "blue", "MAR GFAC" = "black")) +
  scale_linetype_manual(name = NULL, values = c("Nominal Broad US Dollar Index (rhs)" = "solid", "MAR GFAC" = "longdash", "GFAC" = "solid")) +
  scale_x_date(limits = as.Date(c("1990-01-01", "2024-12-01")), date_breaks = "5 years", date_labels = "%Y", expand = c(0, 0)) +
  theme_times_new_roman(base_size = 12) +
  theme(axis.title.y.left = element_text(color = "black"), axis.title.y.right = element_text(color = "black"), legend.title = element_blank(), legend.position = "bottom", axis.title.x = element_blank())

ggsave(file.path(out_fig, "figure10_gfcy_factors_and_us_dollar_index.png"), plot = plot, width = 8, height = 4.5, dpi = 300)

# correlations ----

GFAC.MAR15.cut2 <- GFAC.MAR15.cut %>%
  mutate(GFAC = MAR15) %>%
  filter(date <= as.Date("2000-02-01"), GFAC != is.na(MAR15)) %>%
  dplyr::select(date, GFAC)

GFAC2 <- GFAC %>%
  mutate(GFAC = x) %>%
  slice(-1) %>%
  dplyr::select(date, GFAC)

GFAC.compl <- bind_rows(GFAC.MAR15.cut2, GFAC2)
cor_df <- GFAC.compl %>% left_join(plot_df, by = "date")

cor_df_post2000 <- cor_df %>% filter(date >= as.Date("2000-01-01"))
cor_df_postGFC <- cor_df %>% filter(date > as.Date("2009-06-01"))

cat("GFAC vs broad USD index, full sample:\n")
print(cor(cor_df$GFAC, cor_df$usd_broad))
cat("GFAC vs broad USD index, post-2000:\n")
print(cor(cor_df_post2000$GFAC, cor_df_post2000$usd_broad))
cat("GFAC vs broad USD index, post-GFC:\n")
print(cor(cor_df_postGFC$GFAC, cor_df_postGFC$usd_broad))

sink()

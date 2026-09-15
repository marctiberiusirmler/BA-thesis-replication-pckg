# 15_plot_inflows_vix.R
#
# Dual-axis plot of global gross capital inflows (% of world GDP) by asset
# class against a rescaled, negated VIX. Produces Figure 3.

source(here::here("R", "00_config.R"))

suppressMessages({
  library(zoo)
  library(lubridate)
  library(dplyr)
  library(ggplot2)
  library(scales)
})

Inflows.glbl.pctGDP <- read.csv(file.path(path$build$capital_flows, "global_inflows_pct_gdp.csv"))
VIX.neg.Q <- read.csv(file.path(path$build$vix, "vix_quarterly_negated.csv"))

VIX.neg.Q <- VIX.neg.Q %>%
  mutate(DATE = ymd(DATE), quarter = as.yearqtr(DATE)) %>%
  dplyr::select(quarter, CLOSE.neg)

Inflows.glbl.pctGDP <- Inflows.glbl.pctGDP %>%
  mutate(quarter = as.yearqtr(quarter))

plot_df <- Inflows.glbl.pctGDP %>%
  filter(asset.class %in% c("FDI", "Port. Equity", "Port. Debt", "Other Inv")) %>%
  dplyr::select(quarter, asset.class, pctGDP) %>%
  left_join(VIX.neg.Q, by = "quarter")

left_rng <- range(plot_df$pctGDP, na.rm = TRUE)
right_rng <- range(plot_df$CLOSE.neg, na.rm = TRUE)

flow_mid <- mean(left_rng)
vix_mid <- 0

scale_fac <- diff(left_rng) / diff(right_rng)
shift_fac <- flow_mid - vix_mid * scale_fac

p <- ggplot(plot_df, aes(as.Date(quarter))) +
  geom_line(aes(y = pctGDP, colour = asset.class), size = 1) +
  geom_line(aes(y = CLOSE.neg * scale_fac + shift_fac), linetype = "dashed", colour = "black", size = 0.8) +
  scale_colour_manual(values = c(
    "FDI" = "#F0F0F0", "Port. Equity" = "#A0A0A0",
    "Port. Debt" = "#5A5A5A", "Other Inv" = "#0A0A0A"
  )) +
  scale_y_continuous(
    name = "Gross inflows (% of world GDP)",
    labels = scales::label_percent(scale = 1),
    sec.axis = sec_axis(~ (. - shift_fac) / scale_fac, name = "VIX", breaks = scales::pretty_breaks())
  ) +
  labs(title = "Global Capital Inflows and the VIX", x = NULL, colour = NULL) +
  theme_minimal() +
  theme(
    axis.title.y.right = element_text(colour = "black"),
    axis.line.y.right  = element_line(colour = "black"),
    legend.position    = "top"
  )

ggsave(file.path(path$output$figures, "figure03_capital_inflows_and_vix.pdf"), plot = p)

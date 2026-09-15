# 13_heatmaps.R
#
# Builds correlation-matrix heatmaps (full / pre-GFC / post-GFC / pre-ex-GFC)
# for regional capital inflows, outflows, and net flows (28 series = 7
# regions x 4 asset classes). Writes 10 PNGs; 5 are used in the thesis
# (Figures 1, 2, B.1, B.2, B.3), the other 5 (full-sample and post-ex-GFC
# variants) are exploratory/robustness checks not carried into the final
# draft.
#
# Not network-dependent: reads the output of 10_capital_flows_cleaning.R.

source(here::here("R", "00_config.R"))

suppressMessages({
  library(tidyverse)
  library(reshape2)
  library(ggplot2)
})

inp <- path$build$capital_flows
out <- path$output$figures

theme_times_new_roman <- function(base_size = 12) {
  theme_minimal(base_size = base_size, base_family = "Times New Roman") +
    theme(
      plot.title = element_text(hjust = 0.5, face = "bold"),
      plot.subtitle = element_text(hjust = 0.5),
      panel.grid.minor = element_blank(),
      axis.line = element_line(color = "black", linewidth = 0.5, linetype = 1)
    )
}

get_lower_tri <- function(cormat) {
  cormat[upper.tri(cormat)] <- NA
  cormat
}

col.names <- c(
  "Afr.ME FDI", "Afr.ME Credit", "Afr.ME Debt", "Afr.ME Equity",
  "As.Pac FDI", "As.Pac Credit", "As.Pac Debt", "As.Pac Equity",
  "CE.EU FDI", "CE.EU Credit", "CE.EU Debt", "CE.EU Equity",
  "Em.As FDI", "Em.As Credit", "Em.As Debt", "Em.As Equity",
  "LatAm FDI", "LatAm Credit", "LatAm Debt", "LatAm Equity",
  "N.Am FDI", "N.Am Credit", "N.Am Debt", "N.Am Equity",
  "W.EU FDI", "W.EU Credit", "W.EU Debt", "W.EU Equity"
)

make_heatmap <- function(melted) {
  ggplot(data = melted, aes(Var2, Var1, fill = value)) +
    geom_tile(color = "white") +
    scale_fill_gradientn(
      colours = c("white", "white", "grey80", "grey30"),
      values  = c(0, 0.5, 0.505, 1),
      limits  = c(-1, 1),
      name    = "Correlation"
    ) +
    theme_times_new_roman(base_size = 12) +
    theme(
      legend.position = "none",
      axis.text.x = element_text(angle = 90, vjust = 1, hjust = 1),
      axis.title.x = element_blank(),
      axis.title.y = element_blank()
    ) +
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 2.5)
}

save_heatmap <- function(df, file, use_complete_obs = FALSE) {
  cormat <- round(cor(df, use = if (use_complete_obs) "complete.obs" else "everything"), 2)
  melted <- melt(get_lower_tri(cormat), na.rm = TRUE)
  ggsave(file.path(out, file), plot = make_heatmap(melted), width = 8, height = 8)
}

# Inflows ----
inflows <- read.csv(file.path(inp, "gross_capital_inflows.csv"), header = TRUE)
inflows <- inflows[, -1] # remove quarter col
inflows <- inflows[-141, ] # NA row
colnames(inflows) <- col.names

save_heatmap(inflows, "supp_heatmap_inflows_full_sample.png")
save_heatmap(inflows %>% slice(1:78), "figure01_correlation_heatmap_inflows_preGFC.png") # 1990Q1-2009Q2
save_heatmap(inflows %>% slice(1:70), "supp_heatmap_inflows_preGFC_excl_crisis.png") # 1990Q1-2007Q3
save_heatmap(inflows %>% slice(79:140), "figure02_correlation_heatmap_inflows_postGFC.png") # 2009Q3-2024Q4

# Outflows ----
outflows <- read.csv(file.path(inp, "gross_capital_outflows.csv"), header = TRUE)
outflows <- outflows[, -1]
outflows <- outflows[-141, ]
colnames(outflows) <- col.names

save_heatmap(outflows, "supp_heatmap_outflows_full_sample.png", use_complete_obs = TRUE)
save_heatmap(outflows %>% slice(1:78), "figureB1_correlation_heatmap_outflows_preGFC.png", use_complete_obs = TRUE)
save_heatmap(outflows %>% slice(79:140), "figureB2_correlation_heatmap_outflows_postGFC.png", use_complete_obs = TRUE)

# Net flows ----
# netflows = Inflows - Outflows where both are available
netflows <- as.data.frame(
  mapply(function(i, o) ifelse(is.na(i) | is.na(o), NA, i - o), inflows, outflows),
  row.names = rownames(inflows)
)

save_heatmap(netflows, "supp_heatmap_netflows_full_sample.png", use_complete_obs = TRUE)
save_heatmap(netflows %>% slice(1:78), "figureB3_correlation_heatmap_netflows_preGFC.png", use_complete_obs = TRUE)
save_heatmap(netflows %>% slice(79:140), "supp_heatmap_netflows_postGFC.png", use_complete_obs = TRUE)

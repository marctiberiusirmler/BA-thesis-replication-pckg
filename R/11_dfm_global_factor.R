# 11_dfm_global_factor.R
#
# Fits a 1-factor, 1-lag dynamic factor model on first-differenced log asset
# prices to construct the "Global Factor in Risky Asset Prices" (GFAC).
# Feeds Figures 5, 6, 7, 10.
#
# Not network-dependent: reads the output of 07_asset_prices_cleaning.R.

source(here::here("R", "00_config.R"))

suppressMessages({
  library(dfms)
  library(xts)
  library(vars)
  library(dplyr)
})

prices <- read.csv(file.path(path$build$asset_prices, "asset_price_panel_2000_2024.csv"))

dates <- prices[[1]][-1]

prices.diff.log <- prices %>%
  dplyr::select(-1) %>%
  log() %>%
  mutate(across(everything(), ~ . - lag(.))) %>%
  slice(-1)

prices.diff.log <- bind_cols(tibble::tibble(date = dates), prices.diff.log)
prices.diff.log <- xts(prices.diff.log[, -1], order.by = as.Date(prices.diff.log[[1]]))

drop.all.NA.cols <- function(df) {
  all.NA.cols <- names(df)[colSums(!is.na(df)) == 0]
  list(clean = df[, colSums(!is.na(df)) > 0], dropped.cols = all.NA.cols)
}

results <- drop.all.NA.cols(prices.diff.log)
prices.diff.log <- results$clean

ics <- ICr(prices.diff.log, max.r = 30)
print(ics)

pdf(file.path(path$output$figures, "supp_dfm_bai_ng_information_criteria.pdf"), width = 8, height = 6)
plot(ics)
screeplot(ics, type = c("pve", "ev", "cum.pve"), show.grid = TRUE, max.r = 30)
dev.off()

ics.var <- VARselect(ics$F_pca[, 1])

dfm <- DFM(
  prices.diff.log,
  r = 1, p = 1, rQ = "none", rR = "diagonal",
  em.method = "BM", min.iter = 25L, max.iter = 100L, tol = 1e-04, pos.corr = TRUE
)

saveRDS(dfm, file = file.path(path$build$global_factor, "dfm_model_output.rds"))

sink(file.path(path$output$tables, "supp_dfm_variance_decomposition.txt"))
print(summary(dfm))
sink()

GFAC <- c(NA, cumsum(dfm$F_qml[-1, 1]))
write.csv(GFAC, file.path(path$build$global_factor, "gfac.csv"), row.names = FALSE)

GFAC.stand <- (GFAC - mean(GFAC, na.rm = TRUE)) / sd(GFAC, na.rm = TRUE)
write.csv(GFAC.stand, file.path(path$build$global_factor, "gfac_standardized.csv"), row.names = FALSE)

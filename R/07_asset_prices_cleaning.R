# 07_asset_prices_cleaning.R
#
# Applies an iterative ~80% coverage filter to each region's monthly price 
# panel, then joins all seven regions and restricts to 2000m1-2024m12. 
# Feeds Table 4 and, via 11_dfm_global_factor.R, Figures 5-7/10.
#
# Not network-dependent: reads the frozen regional price panels in
# data/build/asset_prices/ (outputs of 06_asset_prices_pull.R).
#
# IMPORTANT FIX (found only by executing the code, not by reading it):

source(here::here("R", "00_config.R"))

suppressMessages({
  library(dplyr)
  library(lubridate)
  library(purrr)
})

inp <- path$build$asset_prices

read_region <- function(f) read.csv(file.path(inp, f), header = TRUE) %>% mutate(date = ymd(date))

if (!file.exists(file.path(inp, "latin_america_prices.csv"))) {
  stop(
    "Missing data/build/asset_prices/LatAm_1990m12024m12.csv. Run ",
    "R/06_asset_prices_pull.R (requires network access) to regenerate it, ",
    "then re-run this script."
  )
}

# Drops the sparsest column, one at a time, until `ref_row` has >= 80%
# coverage. 
drop_sparsest_until <- function(df, ref_row, threshold = 0.2) {
  na_counts <- sapply(seq_len(ncol(df) - 1), function(i) sum(is.na(df[[i]])))
  while (sum(is.na(df[ref_row, ])) > threshold * (ncol(df) - 1)) {
    drop_col <- which.max(na_counts)
    df <- df[-drop_col]
    na_counts <- na_counts[-drop_col]
  }
  df
}

# North America ----
NorthAm.cov <- read_region("north_america_prices.csv")
NorthAm.cov <- drop_sparsest_until(NorthAm.cov, 121) # 80% coverage at 2000m1 only

# Latin America ----
LatAm.cov <- read_region("latin_america_prices.csv")

pad.dates <- tibble::tibble(
  date = seq(ymd("1990-01-01"), ymd("1991-10-01"), by = "1 month") |>
    ceiling_date("month") - days(1)
)
pad.block <- pad.dates |>
  bind_cols(as_tibble(matrix(NA_real_,
    nrow = nrow(pad.dates), ncol = ncol(LatAm.cov) - 1,
    dimnames = list(NULL, names(LatAm.cov)[-1])
  )))
LatAm.cov <- bind_rows(pad.block, LatAm.cov) |> arrange(date)
LatAm.cov <- drop_sparsest_until(LatAm.cov, 121) # 80% coverage at 2000m1

# Europe ----
EU.cov <- read_region("europe_prices.csv")
EU.cov <- drop_sparsest_until(EU.cov, 121)

# Asia ----
Asia.cov <- read_region("asia_prices.csv")
Asia.cov <- drop_sparsest_until(Asia.cov, 121)

# Australia ----
AUS.cov <- read_region("australia_prices.csv")
AUS.cov <- AUS.cov[-1, ]
AUS.cov <- drop_sparsest_until(AUS.cov, 121)

# Commodities ----
# used unfiltered
Cmdty.cov <- read_region("commodities_prices.csv")
Cmdty.cov$date <- NorthAm.cov$date[seq_len(nrow(Cmdty.cov))] # align to NorthAm calendar

# Corporate bonds ----
# used unfiltered
Corp.cov <- read_region("corporate_bond_prices.csv")

# Combine ----
all.regions.2000cov <- list(NorthAm.cov, LatAm.cov, EU.cov, Asia.cov, AUS.cov, Cmdty.cov, Corp.cov) |>
  reduce(full_join, by = "date") |>
  arrange(date)

all.2000m12024m12.2000cov <- filter(all.regions.2000cov, date >= ymd("2000-01-01"))

cov_2000 <- 1 - sum(is.na(all.2000m12024m12.2000cov[1, ])) / (ncol(all.2000m12024m12.2000cov) - 1)
cov_2005 <- 1 - sum(is.na(all.2000m12024m12.2000cov[61, ])) / (ncol(all.2000m12024m12.2000cov) - 1)
cat(sprintf("2000m1 coverage: %.0f%%\n", 100 * cov_2000))
cat(sprintf("2005m1 coverage: %.0f%%\n", 100 * cov_2005))

# Table 4: regional composition of the panel
composition <- data.frame(
  region = c("North America", "Latin America", "Europe", "Asia", "Australia", "Commodities", "Corporate Bonds"),
  series = c(ncol(NorthAm.cov), ncol(LatAm.cov), ncol(EU.cov), ncol(Asia.cov), ncol(AUS.cov), ncol(Cmdty.cov), ncol(Corp.cov)) - 1,
  thesis_table4 = c(488, 33, 282, 143, 41, 27, 16)
)
composition <- rbind(composition, data.frame(region = "Total", series = sum(composition$series), thesis_table4 = sum(composition$thesis_table4)))
print(composition)

write.csv(composition, file.path(path$output$tables, "table04_risky_asset_price_panel_composition.csv"), row.names = FALSE)

write.csv(all.2000m12024m12.2000cov,
  file.path(path$build$asset_prices, "asset_price_panel_2000_2024.csv"),
  row.names = FALSE
)

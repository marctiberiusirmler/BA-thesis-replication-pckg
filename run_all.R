# run_all.R
#
# This does NOT run the network-dependent scripts (06, 08, 09,
# and 16), since they pull from live APIs (IMF IFS, Yahoo Finance, FRED)
# that return current-vintage data, not the historical vintage the thesis
# was built on. Their historical outputs are in in data/build/, so
# the scripts below run against them directly.
#
# The MATLAB half of the SVAR (matlab/main_svar.m) is not run from here

if (!requireNamespace("here", quietly = TRUE)) {
  stop("Package 'here' is required. Run environment/install_packages.R first.")
}

if (!dir.exists(here::here("data")) || !dir.exists(here::here("R"))) {
  stop(
    "Could not locate the repository root from here::here() (resolved to ",
    here::here(), "). Make sure a .here file, .Rproj file, or .git/ folder ",
    "is present at the top of the replication_package/ folder."
  )
}

scripts <- c(
  "R/01_world_gdp_growth.R",
  "R/02_world_short_rate.R",
  "R/03_vix_cleaning.R",
  "R/04_mar_gfac_cleaning.R",
  "R/05_nber_recessions.R",
  "R/07_asset_prices_cleaning.R",
  "R/10_capital_flows_cleaning.R",
  #"R/11_dfm_global_factor.R", # long run-time, output is in data/build/global_factor
  "R/12_correlation_tables.R",
  "R/13_heatmaps.R",
  "R/14_plot_gfac.R",
  "R/15_plot_inflows_vix.R",
  "R/17_panel_regressions.R",
  "R/18_svar_data_construction.R",
  "R/19_chow_fevd.R"
)

for (s in scripts) {
  cat("\n==============================================================\n")
  cat("Running", s, "\n")
  cat("==============================================================\n")
  source(here::here(s), echo = FALSE, chdir = FALSE)
}

cat("\nDone. Pipeline finished, see output/figures/ and output/tables/.\n")
cat("Tables 1-3, 5, C.1 and most figures are now reproduced from the\n")
cat("frozen data. To run the MATLAB SVAR (Figures 8, 9, D.1-D.4), open\n")
cat("and run matlab/main_svar.m in MATLAB.\n")
cat("To re-derive the historical inputs from live sources instead of the\n")
cat("snapshots, run 06, 08, 09, 16 manually (each requires network\n")
cat("access).\n")

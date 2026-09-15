# The Global Financial Cycle and the US Dollar -- Replication Package

Replication package for the Bachelor's thesis, *"The Global
Financial Cycle and the US Dollar"* (University of Zurich, 2025)

## Content

```
R/                     -- 19 numbered R scripts (00 is shared config), one
                          pipeline stage each, run in numeric order
matlab/                -- the SVAR estimation engine (Cholesky-identified
                          VAR), in MATLAB
data/
  raw/                 -- frozen inputs, by source category
  build/               -- intermediate outputs each stage writes/reads
output/
  figures/             -- figures the pipeline produces
  tables/              -- tables/statistics the pipeline produces
environment/           -- R version, package versions, install script
run_all.R              -- runs the offline part of the pipeline
DATA.md                -- data: what's included and what has to be repulled (requires network access)
```

## Reproducing the results

1. Install R packages: `Rscript environment/install_packages.R`
2. From this folder, run: `Rscript run_all.R`
3. For the SVAR (Figures 8, 9, D.1-D.4): open and run `matlab/main_svar.m` in MATLAB

`run_all.R` reproduces every table and figure that can be reproduced fully
offline, from the data in
`data/raw/` and `data/build/`. That covers Tables 1-3, 5, and C.1, and every
figure except the SVAR's (8, 9, D.1-D.4).

## Overview over thesis results

| Thesis item | file | script |
|---|---|---|
| Table 1 | `table01_unconditional_correlations_inflows_vix*.csv` | `12_correlation_tables.R` |
| Table 2 | `table02_partial_correlations_inflows_vix*.csv` | `12_correlation_tables.R` |
| Table 3 | `table03_partial_correlations_credit_leverage*.csv` | `12_correlation_tables.R` |
| Table 4 | `table04_risky_asset_price_panel_composition.csv` | `07_asset_prices_cleaning.R` |
| Table 5 | `table05_unrestricted_panel_regression.csv` | `17_panel_regressions.R` |
| Table C.1 | `tableC1_panel{A,B,C}_*_{cluster,driscoll_kraay}_se.csv` | `17_panel_regressions.R` |
| Figure 1 | `figure01_correlation_heatmap_inflows_preGFC.png` | `13_heatmaps.R` |
| Figure 2 | `figure02_correlation_heatmap_inflows_postGFC.png` | `13_heatmaps.R` |
| Figure 3 | `figure03_capital_inflows_and_vix.pdf` | `15_plot_inflows_vix.R` |
| Figure 4 | `figure04_domestic_credit_and_leverage_by_region.png` | `14_plot_gfac.R` |
| Figure 5 | `figure05_median_factor_loadings_by_market.png` | `14_plot_gfac.R` (via `11_dfm_global_factor.R`) |
| Figure 6 | `figure06_global_factors_risky_asset_prices.png` | `14_plot_gfac.R` (via `11_dfm_global_factor.R`) |
| Figure 7 | `figure07_global_factors_and_vix.png` | `14_plot_gfac.R` (via `11_dfm_global_factor.R`) |
| Figure 8 | SVAR IRF output, full sample | `matlab/main_svar.m` |
| Figure 9 | SVAR IRF output, pre-/post-GFC | `matlab/main_svar.m` |
| Figure 10 | `figure10_gfcy_factors_and_us_dollar_index.png` | `14_plot_gfac.R` |
| Figure B.1/B.2/B.3 | `figureB{1,2,3}_correlation_heatmap_*.png` | `13_heatmaps.R` |
| Figure D.1-D.4 | commented out by default, see `matlab/main_svar.m` | `matlab/main_svar.m` |
| Chow test / FEVD shares (p. 26, text) | `supp_svar_chow_test_and_fevd.txt` | `19_chow_fevd.R` |
| DFM variance explained (p. 12/14, text) | `supp_dfm_variance_decomposition.txt` | `11_dfm_global_factor.R` |
| In-text correlations (p. 7, 15, 16) | `supp_intext_correlations_gfac_credit_vix.txt` | `14_plot_gfac.R` |

Files prefixed `supp_` are supplementary files, e.g., diagnostic plots, robustness checks or exploratory plots that are not included in the thesis.

### Network access

- **Not network-dependent (run by `run_all.R`):** every script that only
  reads local files.
- **Network-dependent (NOT run by `run_all.R`):** `06`, `08`, `09`,
  and `16` pull from live external sources (IMF IFS API, Yahoo Finance/
  Stooq, FRED). These sources return *current-vintage* data, which is
  revised over time and will not exactly reproduce the historical vintage
  the thesis was built on. Their outputs are saved as frozen data
  directly in `data/build/`, so the rest of the pipeline uses these. 

## Scripts

| # | Script | Stage |
|---|---|---|
| 00 | `config.R` | shared path setup (sourced by every script, not run directly) |
| 01-05 | `world_gdp_growth`, `world_short_rate`, `vix_cleaning`, `mar_gfac_cleaning`, `nber_recessions` | small, independent conditioning-variable/series builders |
| 06-07 | `asset_prices_pull`, `asset_prices_cleaning` | the risky-asset price panel (Table 4) |
| 08-09 | `credit_construction`, `leverage_construction` | domestic credit and banking-sector leverage (Table 3, SVAR inputs) |
| 10 | `capital_flows_cleaning` | gross capital flow panels (Tables 1-2, Figures 1-3, B.1-B.3) |
| 11 | `dfm_global_factor` | the global factor in risky asset prices (Figures 5-7, 10) |
| 12-15 | `correlation_tables`, `heatmaps`, `plot_gfac`, `plot_inflows_vix` | Section-3/4 tables and figures |
| 16-17 | `panel_reg_cleaning`, `panel_regressions` | Section-5 panel regression (Table 5, C.1) |
| 18-19 | `svar_data_construction`, `chow_fevd` | Section-6 SVAR data and R-side cross-check |

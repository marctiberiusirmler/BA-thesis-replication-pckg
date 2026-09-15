# DATA.md

## Sources, by category (`data/raw/<category>/`)

| Category | Sources | Access |
|---|---|---|
| `credit_leverage/` | IMF IFS (ODC/DMB surveys), BIS locational banking stats, Bank of Canada, Bank of England, FRED | IMF/BIS/FRED public; IFS access via the `imf.data` R package |
| `asset_prices/` | Yahoo Finance, Stooq (via `quantmod`/`tidyquant`) | Public, free, rate-limited |
| `capital_flows/` | IMF Balance of Payments (BOP), IMF country codes, IMF World GDP | Public |
| `macro/` | IMF WEO, World Bank GDP (PPP), OECD policy rates & inflation forecasts | Public |
| `vix/` | CBOE VIX historical data | Public |
| `global_factor/` | Miranda-Agrippino & Rey's published global factor vintages | Public (published alongside their papers) |
| `svar/` | BEA/BLS (US GDP, GDPDEF via FRED-style extracts), Fed Flow of Funds (Z.1, broker-dealer leverage), FRED (FEDFUNDS), Wu & Xia's shadow rate | Public |
| `panel/` | Bloomberg (`World Indices_EoQ Prices.xlsx`, end-of-quarter equity index levels) | **Not public**, Bloomberg terminal data -- excluded from this repo, see note below. |
| `misc/` | BIS locational banking stats (nonbank credit), FRED broad USD index (DTWEXBGS), an IFDP data appendix note (nominal USD indexes) | Public

## Note on `data/raw/panel/World Indices_EoQ Prices.xlsx`

This file is a raw export from a Bloomberg terminal (end-of-quarter equity
index levels, one column per country/index) and is not redistributable
under Bloomberg's data license, so it is excluded from this repository (see
`.gitignore`). To re-run `16_panel_reg_cleaning.R` from scratch you would
need to supply your own copy of this file with the same structure -- a
sheet of end-of-quarter closing levels, one column per country index,
starting on row 3 (the script reads with `skip = 2`). This is not required
to reproduce the thesis's published results: the derived output,
`data/build/panel/panel_regression_dataset.csv` (already-computed quarterly
returns, not raw price levels), is shipped frozen and is what
`17_panel_regressions.R` (run by `run_all.R`) actually consumes.

## Note on `data/build/credit/leverage_by_block.csv`

This frozen build artifact was lost in a folder migration and could not be
recovered from the original vintage. It has been reconstructed from
`leverage_by_block_country_growth_median.csv` (the companion file from the
same `09_leverage_construction.R` stage, which survived the migration
intact): block-median leverage levels are taken from that file, and
`lev_qoq` is recomputed as the quarter-on-quarter growth of that block
median, per the formula documented in `09_leverage_construction.R`. The two
files' block-median levels are identical except for blocks containing
Greece/Estonia (excluded only in the country-growth-median variant), and
the reconstructed table reproduces the thesis's published Table 3
coefficients exactly (p-values match to within +/-0.001, i.e. floating-point
noise). Re-running `08_credit_construction.R` + `09_leverage_construction.R`
from scratch (network access required) would regenerate this file exactly.

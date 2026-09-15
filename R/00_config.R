# 00_config.R
#
# Shared path configuration, sourced by every script in R/. Every script
# reads from data/raw/<category>/ and data/build/<category>/, and writes only
# under data/build/ or output/.
#
# Paths are anchored via here::here().

suppressMessages(library(here))

path <- list(
  raw = list(
    credit_leverage = here("data", "raw", "credit_leverage"),
    asset_prices    = here("data", "raw", "asset_prices"),
    capital_flows   = here("data", "raw", "capital_flows"),
    macro           = here("data", "raw", "macro"),
    vix             = here("data", "raw", "vix"),
    global_factor   = here("data", "raw", "global_factor"),
    svar            = here("data", "raw", "svar"),
    panel           = here("data", "raw", "panel"),
    misc            = here("data", "raw", "misc")
  ),
  build = list(
    credit          = here("data", "build", "credit"),
    asset_prices    = here("data", "build", "asset_prices"),
    vix             = here("data", "build", "vix"),
    global_factor   = here("data", "build", "global_factor"),
    macro           = here("data", "build", "macro"),
    capital_flows   = here("data", "build", "capital_flows"),
    panel           = here("data", "build", "panel"),
    svar            = here("data", "build", "svar"),
    misc            = here("data", "build", "misc")
  ),
  output = list(
    figures = here("output", "figures"),
    tables  = here("output", "tables")
  )
)

if (!dir.exists(here("data")) || !dir.exists(here("R"))) {
  stop(
    "Could not locate the repository root from here::here() (resolved to ",
    here(), "). Make sure a .here file, .Rproj file, or .git/ folder is ",
    "present at the top of the replication_package/ folder."
  )
}

invisible(lapply(path$build, function(d) dir.create(d, showWarnings = FALSE, recursive = TRUE)))
invisible(lapply(path$output, function(d) dir.create(d, showWarnings = FALSE, recursive = TRUE)))

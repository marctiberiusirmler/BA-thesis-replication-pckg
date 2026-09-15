# install_packages.R
#
# Installs every R package used anywhere in this pipeline. Run once before
# run_all.R. See R_packages.txt for the exact versions this package was
# built and tested against.

pkgs <- c(
  "here", "tidyverse", "zoo", "readxl", "openxlsx", "countrycode", "lubridate",
  "imf.data", "quantmod", "tidyquant", "tempdisagg", "janitor", "plm",
  "lmtest", "car", "tseries", "gplots", "vars", "svars", "dfms", "xts",
  "purrr", "stringr", "reshape2", "ggplot2", "forcats", "scales", "tibble"
)

missing <- pkgs[!vapply(pkgs, requireNamespace, logical(1), quietly = TRUE)]
if (length(missing) > 0) install.packages(missing)

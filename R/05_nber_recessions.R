# 05_nber_recessions.R
#
# Hardcodes NBER US recession start/end dates, used to shade recession bands
# in Figures 4, 6, 7, 10.

source(here::here("R", "00_config.R"))

recessions <- data.frame(
  start = as.Date(c(
    "1980-01-01", "1981-07-01", "1990-07-01", "2001-03-01",
    "2007-12-01", "2020-02-01"
  )),
  end = as.Date(c(
    "1980-07-01", "1982-11-01", "1991-03-01", "2001-11-01",
    "2009-06-01", "2020-04-01"
  ))
)

write.csv(recessions, file.path(path$build$misc, "nber_recessions.csv"), row.names = FALSE)

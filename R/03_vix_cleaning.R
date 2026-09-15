# 03_vix_cleaning.R
#
# Cleans daily VIX history into daily/monthly/quarterly series, plus negated
# and standardized variants used across Figures 1-3, 6-10 and Tables 1-3.

source(here::here("R", "00_config.R"))

suppressMessages({
  library(dplyr)
  library(lubridate)
})

VIX.D <- read.csv(file.path(path$raw$vix, "VIX_History_18032025.csv"), header = TRUE)
VIX.D <- VIX.D[-c(2, 3, 4)]

VIX.D <- VIX.D %>%
  mutate(DATE = mdy(DATE)) %>%
  filter(DATE <= ymd("2024-12-31"))

write.csv(VIX.D, file.path(path$build$vix, "vix_daily.csv"), row.names = FALSE)

VIX.M <- VIX.D %>%
  group_by(year = year(DATE), month = month(DATE)) %>%
  filter(DATE == max(DATE)) %>%
  ungroup()
VIX.M <- VIX.M[-c(3, 4)]

VIX.M.neg <- data.frame(DATE = VIX.M$DATE, CLOSE.neg = (-1) * VIX.M$CLOSE)

mean.VIX.M <- mean(VIX.M$CLOSE)
sd.VIX.M <- sd(VIX.M$CLOSE)
VIX.M.st <- data.frame(DATE = VIX.M$DATE, CLOSE.st = (VIX.M$CLOSE - mean.VIX.M) / sd.VIX.M)

write.csv(VIX.M, file.path(path$build$vix, "vix_monthly.csv"), row.names = FALSE)
write.csv(VIX.M.st, file.path(path$build$vix, "vix_monthly_standardized.csv"), row.names = FALSE)
write.csv(VIX.M.neg, file.path(path$build$vix, "vix_monthly_negated.csv"), row.names = FALSE)

VIX.Q <- VIX.D %>%
  group_by(year = year(DATE), quarter = quarter(DATE)) %>%
  filter(DATE == max(DATE)) %>%
  ungroup()
VIX.Q <- VIX.Q[-c(3, 4)]

VIX.Q.neg <- data.frame(DATE = VIX.Q$DATE, CLOSE.neg = (-1) * VIX.Q$CLOSE)

write.csv(VIX.Q, file.path(path$build$vix, "vix_quarterly.csv"), row.names = FALSE)
write.csv(VIX.Q.neg, file.path(path$build$vix, "vix_quarterly_negated.csv"), row.names = FALSE)

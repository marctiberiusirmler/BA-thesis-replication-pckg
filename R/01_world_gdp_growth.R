# 01_world_gdp_growth.R
#
# Converts annual IMF WEO world real-GDP growth into a quarterly series via
# Denton-Cholette temporal disaggregation. Feeds the "world GDP growth"
# conditioning variable used in Tables 1-3 (Correlation Tables).

source(here::here("R", "00_config.R"))

suppressMessages({
  library(tempdisagg)
  library(tidyverse)
  library(stringr)
  library(zoo)
})

GDP.g <- read.csv(file.path(path$raw$macro, "world real growth rate_1990-2024_IMF_WEO.csv"))
GDP.g <- GDP.g[, -c(1:7)] # remove descriptive columns
GDP.g <- ts(GDP.g, start = 1990, frequency = 1) # transform data into a ts object

# transform growth data into levels
GDP.L <- cumprod(c(100, 1 + GDP.g / 100))[-1] # annual level index
GDP.L <- ts(GDP.L, start = start(GDP.g), frequency = 1)

# interpolation, denton method: smoothest path consistent with annual
# averages, no additional indicator
model <- td(GDP.L ~ 1,
  to = "quarterly",
  method = "denton-cholette",
  conversion = "average"
)

GDP.L.Q <- predict(model)

# transform back to growth rates
GDP.g.Q.QoQ <- diff(log(GDP.L.Q)) * 100 # QoQ % growth
GDP.g.Q.YoY <- (GDP.L.Q / stats::lag(GDP.L.Q, -4) - 1) * 100 # YoY % growth

df <- data.frame(
  quarter   = as.yearqtr(time(GDP.L.Q)),
  lvl.index = as.numeric(GDP.L.Q),
  QoQ.pct   = c(NA, as.numeric(GDP.g.Q.QoQ)), # pad the first Q with NA
  YoY.pct   = c(rep(NA, 4), as.numeric(GDP.g.Q.YoY)) # pad the first 4 Q with NA
)

write.csv(df, file.path(path$build$macro, "world_gdp_growth.csv"), row.names = FALSE)

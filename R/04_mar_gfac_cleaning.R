# 04_mar_gfac_cleaning.R
#
# Reshapes Miranda-Agrippino & Rey's own published global factor vintages
# (MAR15 / MANR24) used as a comparison series in Figures 6-7.

source(here::here("R", "00_config.R"))

library(readxl)

MAR.stand <- read_excel(file.path(path$raw$global_factor, "GFC Factor Updates 2024.xlsx"), sheet = 2)
colnames(MAR.stand) <- c("date", "MAR15", "MANR24")

write.csv(MAR.stand, file.path(path$build$global_factor, "gfac_mar_standardized.csv"), row.names = FALSE)

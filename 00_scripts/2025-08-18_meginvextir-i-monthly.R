# Meginvextir á mánuði


# SETUP ----
library(tidyverse)


# Data
vextir_tbl <- read_csv2("00_data/meginvextir.csv")

vextir_tbl |> 
  mutate(
    date = floor_date(dmy(Dagsetning), "month"),
    meginvextir = Meginvextir / 100
  ) |> 
  group_by(date) |> 
  summarise(meginvextir = mean(meginvextir)) |> 
  writexl::write_xlsx("01_output/meginvextir_monthly.xlsx")

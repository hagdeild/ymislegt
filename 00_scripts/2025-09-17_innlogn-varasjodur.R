# Innlögn í varasjóð

library(tidyverse)
library(vr)

con <- vr_gagnagrunnur()


data_tbl <- tbl(con, "VSJ_V") |> 
  filter(ID == 136772) |> 
  as_tibble()



data_tbl |> 
  filter(ID == 18292) |> 
  arrange(ARMA)


tbl(con, "FG_V") |> 
  filter(
    ID == 136772,
  ARMA >= "202401", ARMA <= "202412"
) |> 
  summarise(laun = sum(LAUN))

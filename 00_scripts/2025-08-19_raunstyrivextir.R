# Raunvextir á Íslandi

# SETUP ----
library(tidyverse)


# Data ----
meginvextir_tbl <- read_csv2("00_data/meginvextir_langir.csv")

meginvextir_tbl <- meginvextir_tbl |> 
  mutate(
    date = floor_date(dmy(Dagsetning), "month")
  ) |> 
  group_by(date) |> 
  summarise(meginvextir = mean(Meginvextir) / 100)


verdbolga_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/48b8431a-f744-414b-a3af-cca414f7e55e") |> 
  set_names("date", "verdbolga") |> 
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)),
    verdbolga = verdbolga / lag(verdbolga, 12) - 1
  ) |> 
  drop_na()


data_tbl <- meginvextir_tbl |> 
  left_join(verdbolga_tbl) |> 
  mutate(raunvextir = (1 + meginvextir) / (1 + verdbolga) - 1) |> 
  drop_na()


# Analysis ----
data_tbl |> 
  ggplot(aes(date, raunvextir)) +
  geom_line() + 
  theme_minimal() +
  labs(
    x = NULL,
    y = NULL,
    title = "Raunstýrivextir"
  ) +
  scale_y_continuous(labels = scales::percent, breaks =  seq(-0.05, 0.10, by = 0.02))

vr::vr_ggsave("01_output/raunvextir.png")

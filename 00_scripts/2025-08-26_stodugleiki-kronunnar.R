# Stöðugleiki krónunnar


# SETUP ----
library(tidyverse)


# Data ----
data_tbl <- read_csv2("00_data/gengi_gjaldmidla_sedlabanki_20120102_20250825.csv") |> 
  janitor::clean_names() |> 
  select(dagsetning, contains("midgengi")) |> 
  set_names("date", "EUR", "USD", "GBP", "NOK", "SEK")

month_name_tbl <- tibble(
  month_name = c("Júlí", "Júní", "Maí", "Apríl", "Mars", "Febrúar", "Janúar", "Desember", "Nóvember", "Október", "September", "Ágúst"),
  month_no = c(7, 6, 5, 4, 3, 2, 1, 12, 11, 10, 9, 8)
)

unnid_tbl <- data_tbl |> 
  separate_wider_delim(cols = date, delim = " ", names = c("month_name", "year")) |> 
  left_join(month_name_tbl) |> 
  mutate(date = make_date(year, month_no)) |> 
  select(-c(month_name, month_no, year)) |> 
  pivot_longer(cols = -c(USD, date)) |> 
  mutate(value = value / USD) |> 
  pivot_wider(names_from = name, values_from = value) |> 
  rename("ISK" = "USD") |> 
  pivot_longer(cols = -date) |> 
  arrange(date, name) |> 
  group_by(name) |> 
  mutate(change = value / lag(value) - 1) |> 
  drop_na()


# Plots and summary statistics
unnid_tbl |> 
  filter(date >= "2016-01-01") |> 
  group_by(name) |> 
  summarise(
    mean = mean(change),
    sd = sd(change)
  )

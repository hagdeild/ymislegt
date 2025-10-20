# Aldurssamsetning félaga

# 1.0.0 SETUP ----
library(tidyverse)
library(vr)

con <- vr_gagnagrunnur()


# 1.1.0 Data ----
aldur_tbl <- tbl(con, "FG_V") |>
  group_by(ARMA) |>
  summarise(aldur = mean(ALDUR)) |>
  as_tibble()

aldur_tbl <- aldur_tbl |>
  mutate(date = make_date(str_sub(ARMA, 1, 4), str_sub(ARMA, 5, 6))) |>
  select(date, aldur) |>
  filter(date >= "2000-01-01", date <= "2025-07-01")


aldursdreifing_raw_tbl <- tbl(con, "FG_V") |>
  filter(ARMA >= "200010", ARMA <= "202507") |>
  select(ID, ARMA, ALDUR) |>
  janitor::clean_names() |>
  as_tibble()


# 2.0.0 GREINING ----

# 2.1.0 Meðalaldur ----
aldur_tbl |>
  ggplot(aes(date, aldur)) +
  geom_line()


# 2.2.0 Aldursdreifing ----
aldursdreifing_tbl <- aldursdreifing_tbl |>
  mutate(date = make_date(str_sub(arma, 1, 4), str_sub(arma, 5, 6)))

# aldursdreifing_tbl |>
#   mutate(ar = year(date)) |>

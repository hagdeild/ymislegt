# Greining á iðgjaldagreiðslum
# Bókhaldskerfi vs gagnagrunnur

# 1.0.0 SETUP ----
library(tidyverse)
library(vr)
library(patchwork)

con <- vr_gagnagrunnur()


# 1.2.0 Data ----

# iðgjöld úr gagnagrunni
vmst_arin <- c(
  "4404070640",
  "5508952699",
  "5501984369",
  "4301693949",
  "6602692669"
)

felagsgjold_tbl <- tbl(con, "FG_V") %>%
  filter(MERKING == "fg") |>
  select(ARMA_DAGS, KEFY, NAFN, LAUN) %>%
  mutate(
    laun_new = case_when(
      KEFY %in% vmst_arin | NAFN == "Almannatryggingar" ~ 0,
      TRUE ~ LAUN
    )
  ) %>%
  group_by(ARMA_DAGS) %>%
  summarise(
    felagsgjald_1_pct = sum(laun_new) * 0.01,
    felagsgjald_07_pct = sum(LAUN) * 0.007
  ) %>%
  as_tibble() %>%
  janitor::clean_names() %>%
  mutate(date = date(arma_dags)) %>%
  select(date, contains("felagsgjald")) %>%
  filter(date >= "1990-01-01") %>%
  arrange(date)


# iðgjöld úr bókhaldskerfi
bokhald_raw_tbl <- readxl::read_excel("00_data/Tekjugreining 2025.xlsx") |>
  janitor::clean_names()

bokhald_tbl <- bokhald_raw_tbl |>
  mutate(date = date(floor_date(bokunardags, "month"))) |>
  group_by(date) |>
  summarise(idgjald = -sum(upphaed)) |>
  mutate(key = "Bókhaldskerfi")


bokhald_tbl |>
  ggplot(aes(date, idgjald)) +
  geom_line()


# 2.0.0 GREINING ----
# Sameina gögn
data_tbl <- felagsgjold_tbl |>
  pivot_longer(cols = -date) |>
  set_names("date", "key", "idgjald") |>
  bind_rows(bokhald_tbl)

data_tbl |>
  filter(date >= "2021-01-01", date <= "2025-06-01") |>
  ggplot(aes(date, idgjald, col = key)) +
  geom_line()


# summary
data_tbl |>
  filter(date >= "2025-01-01", date <= "2025-06-01") |>
  group_by(key) |>
  summarise(idgjald = sum(idgjald))


data_tbl |>
  filter(!key == "felagsgjald_1_pct") |>
  filter(date >= "2021-01-01", date < "2025-01-01") |>
  mutate(date = floor_date(date, "quarter")) |>
  group_by(date, key) |>
  summarise(idgjald = sum(idgjald)) |>
  ggplot(aes(date, idgjald, col = key)) +
  geom_line()


data_tbl |>
  filter(!key == "felagsgjald_1_pct") |>
  filter(date >= "2021-01-01", date < "2025-01-01") |>
  mutate(date = floor_date(date, "quarter")) |>
  group_by(date, key) |>
  summarise(idgjald = sum(idgjald)) |>
  group_by(key) |>
  summarise(idgjald = sum(idgjald)) |>
  mutate(munur = idgjald - lag(idgjald))


# 2.1.0 Tefja félagatal um 2-3 mánuði ----
p1 <- data_tbl |>
  filter(key != "felagsgjald_1_pct") |>
  pivot_wider(names_from = key, values_from = idgjald) |>
  drop_na() |>
  mutate(
    felagsgjald_lag = lag(felagsgjald_07_pct, 0),
    date = floor_date(date, "quarter")
  ) |>
  group_by(date) |>
  summarise(
    felagatal = sum(felagsgjald_lag),
    bokhaldskerfi = sum(Bókhaldskerfi)
  ) |>
  pivot_longer(cols = -date) |>
  drop_na() |>

  ggplot(aes(date, value, col = name)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = "Iðgjöld - Samanburður á félagatali og bókhalskerfi"
  ) +
  scale_y_continuous(labels = scales::number_format()) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )


p2 <- data_tbl |>
  filter(key != "felagsgjald_1_pct") |>
  pivot_wider(names_from = key, values_from = idgjald) |>
  drop_na() |>
  mutate(
    felagsgjald_lag = lag(felagsgjald_07_pct, 3),
    date = floor_date(date, "quarter")
  ) |>
  group_by(date) |>
  summarise(
    felagatal = sum(felagsgjald_lag),
    bokhaldskerfi = sum(Bókhaldskerfi)
  ) |>
  pivot_longer(cols = -date) |>
  drop_na() |>

  ggplot(aes(date, value, col = name)) +
  geom_line() +
  labs(
    x = NULL,
    y = NULL,
    title = "Iðgjöld - Samanburður á félagatali og bókhalskerfi",
    subtitle = "Félagatalið tafið um 3 mánuði"
  ) +
  scale_y_continuous(labels = scales::number_format()) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )


(p1 / p2)


ggsave(
  "01_output/idgjold_felagatal_samanburdur.png",
  dpi = 96,
  width = 8,
  height = 5
)


# tölfræðilegur samanburður
data_tbl |>
  filter(key != "felagsgjald_1_pct") |>
  pivot_wider(names_from = key, values_from = idgjald) |>
  drop_na() |>
  mutate(
    lag_2 = lag(felagsgjald_07_pct, 2),
    lag_3 = lag(felagsgjald_07_pct, 3),
    date = floor_date(date, "quarter"),
    year = year(date)
  ) |>
  drop_na() |>
  select(-felagsgjald_07_pct) |>
  pivot_longer(cols = -c(date, year)) |>
  group_by(year, name) |>
  summarise(value = sum(value)) |>
  pivot_wider(names_from = name, values_from = value) |>
  mutate(
    diff_2 = Bókhaldskerfi - lag_2,
    diff_3 = Bókhaldskerfi - lag_3
  )


# reiknaður munur, graf
data_tbl |>
  filter(key != "felagsgjald_1_pct") |>
  pivot_wider(names_from = key, values_from = idgjald) |>
  drop_na() |>
  mutate(
    felagsgjald_lag = lag(felagsgjald_07_pct, 3),
    date = floor_date(date, "quarter")
  ) |>
  group_by(date) |>
  summarise(
    felagatal = sum(felagsgjald_lag),
    bokhaldskerfi = sum(Bókhaldskerfi)
  ) |>
  mutate(munur = bokhaldskerfi - felagatal) |>
  select(date, munur) |>
  drop_na() |>

  ggplot(aes(date, munur)) +
  geom_col() +
  labs(
    x = NULL,
    y = NULL
  ) +
  scale_y_continuous(labels = scales::number_format()) +
  theme_minimal() +
  theme(
    legend.position = "bottom",
    legend.title = element_blank()
  )

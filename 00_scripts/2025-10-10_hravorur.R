# Hrávörur

# 1.0.0 SETUP ----
library(tidyverse)


# 1.1.0 Data ----
data_tbl <- list.files(
  "00_data/commodities/",
  pattern = "\\.csv$",
  full.names = TRUE
) |>
  map_dfr(~ read_csv(.x) |> mutate(source = basename(.x))) |>
  janitor::clean_names() |>
  select(date, price, source) |>
  mutate(
    source = str_remove(source, " Data.csv"),
    date = mdy(date)
  ) |>
  set_names("date", "value", "name")

hagstofa_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/e4bfafe2-3c15-405b-8618-358956d66891"
) |>
  set_names("date", "Kaffi", "Kakó") |>
  pivot_longer(cols = -date) |>
  mutate(date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)))

# 2.0.0 PLOTS ----

data_tbl |>
  bind_rows(hagstofa_tbl) |>
  arrange(date, name) |>
  filter(date >= "2015-01-01") |>
  group_by(name) |>
  mutate(value = value / first(value) * 100) |>
  ungroup() |>
  mutate(
    key = case_when(
      str_detect(name, "Kaffi|Coffee") ~ "Kaffi",
      TRUE ~ "Kakó"
    )
  ) |>
  ggplot(aes(date, value, color = name)) +
  geom_line(linewidth = 1) +
  facet_wrap(~key, scales = "free_y") +
  scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
  theme_minimal(base_size = 13) +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    strip.text = element_text(face = "bold"),
    plot.title = element_text(face = "bold", size = 15),
    plot.subtitle = element_text(size = 12)
  ) +
  labs(
    x = NULL,
    y = "Index (2015 = 100)",
    title = "Heimsmarkaðsverð á kaffi og kakó",
    subtitle = "Samanburður við verð úti í búð"
  ) +
  scale_color_manual(
    values = c(
      "Kaffi" = "#8B4513", # brown coffee
      "Kakó" = "#D2691E", # cocoa brown
      "London Cocoa Futures Historical" = "#C77CFF",
      "London Robusta Coffee Futures Historical" = "#00BFC4",
      "US Cocoa Futures Historical" = "#7CAE00",
      "US Coffee C Futures Historical" = "#F8766D"
    )
  )

ggsave("01_output/commodities/plot.png", width = 8, height = 5, dpi = 96)

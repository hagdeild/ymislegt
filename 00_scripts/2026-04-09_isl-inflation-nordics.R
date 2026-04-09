# Verðbólga á Íslandi og norðurlöndunum

# 1.0.0 SETUP ----
library(tidyverse)
library(eurostat)


# 2.0.0 DATA ----

hicp_raw_tbl <- get_eurostat(
  id = "prc_hicp_midx",
  filters = list(
    geo = c("IS", "DK", "NO", "FI", "SE"),
    coicop = "CP00",
    unit = "I15",
    freq = "M"
  ),
  time_format = "date",
  cache = FALSE
) |>
  as_tibble()


# 3.0.0 PREPARATION ----

land_tbl <- tibble(
  geo = c("DK", "FI", "IS", "NO", "SE"),
  country = c("Danmörk", "Finnland", "Ísland", "Noregur", "Svíþjóð")
)

hicp_tbl <- hicp_raw_tbl |>
  rename(date = time) |>
  select(geo, date, values) |>
  left_join(land_tbl, by = "geo")

# Index to 100 in January 2020
base_values <- hicp_tbl |>
  filter(date == as.Date("2020-01-01")) |>
  select(geo, base = values)

hicp_indexed_tbl <- hicp_tbl |>
  left_join(base_values, by = "geo") |>
  mutate(index = values / base * 100)


# 4.0.0 PLOT ----

nordic_colors <- c(
  "Ísland" = "#b02a37",
  "Danmörk" = "#087f5b",
  "Finnland" = "#1864ab",
  "Noregur" = "#e67700",
  "Svíþjóð" = "#5f3dc4"
)

hicp_indexed_tbl |>
  filter(date >= as.Date("2020-01-01")) |>
  mutate(country = fct_reorder2(country, date, index)) |>
  ggplot(aes(date, index, color = country)) +
  geom_line(linewidth = 0.8) +
  scale_color_manual(values = nordic_colors) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y"
  ) +
  labs(
    x = NULL,
    y = "Vísitala (janúar 2020 = 100)",
    title = "Samræmd vísitala neysluverðs á Norðurlöndunum",
    subtitle = "HICP, janúar 2020 = 100",
    caption = "Heimild: Eurostat (prc_hicp_midx)"
  ) +
  theme_classic() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom",
    plot.title = element_text(face = "bold")
  )

# ggsave(
#   filename = "01_output/2026-04-09_hicp-nordics.png",
#   plot     = p,
#   width    = 10,
#   height   = 6,
#   dpi      = 150
# )

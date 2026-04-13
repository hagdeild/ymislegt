# Raunlaun eftir starfsstétt og starfi

# 1.0.0 SETUP ----
library(tidyverse)

# 1.1.0 Data ----
laun_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/c70f7277-7d33-4cd6-8984-19d8ac997fb7"
) |>
  set_names(
    "date",
    "hopur",
    "starfsstett",
    "kyn",
    "maeling",
    "eining",
    "laun"
  ) |>
  select(-c("kyn", "maeling", "eining"))

laun_tbl <- laun_tbl |>
  mutate(
    hopur = case_when(
      str_detect(hopur, "almennum") ~ "Almennur markaður",
      str_detect(hopur, "ríkis") ~ "Ríkisstarfsmenn",
      TRUE ~ "Sveitarfélög"
    ),
    starfsstett = str_remove(starfsstett, "\\s*\\(\\d+\\)\\s*$")
  )

vnv_tbl <-
  read_csv2(
    "https://px.hagstofa.is:443/pxis/sq/afdd7eb3-95a6-402b-830b-f722632cc34c",
    na = "."
  ) |>
  drop_na() |>
  set_names("date", "vnv")


# 2.0.0 GREINING ----

litir <- c("#ef383f", "#0ead69", "#028fbc")

laun_tbl |>
  mutate(laun = as.numeric(laun)) |>
  left_join(vnv_tbl) |>
  mutate(raunlaun = laun / vnv) |>
  arrange(date, hopur, starfsstett) |>
  group_by(hopur, starfsstett) |>
  mutate(index = raunlaun / raunlaun[1] * 100) |>
  ungroup() |>

  ggplot(aes(date, index, col = hopur)) +
  geom_line() +
  geom_point() +
  facet_wrap(~starfsstett) +
  scale_color_manual(values = litir) +
  theme_minimal(base_size = 16) +
  theme(
    legend.title = element_blank(),
    plot.title = element_text(face = "bold"),
    legend.position = "bottom",
    legend.direction = "vertical"
  ) +
  labs(
    x = NULL,
    y = NULL,
    title = "Raunlaun eftir launþegahópi og starfsstétt",
    subtitle = "Vísitala = 100 árið 2014"
  )

# launahækkanir og verðbólga

# 1.0.0 SETUP ----
library(tidyverse)


laun_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/d218151f-ac18-498e-8aaf-8544cd856ebe"
) |>
  set_names("date", "laun") |>
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)),
    laun = laun / lag(laun, 12) - 1
  ) |>
  drop_na()


vnv_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/06529b72-2df1-41b1-b954-80f3031b85f9"
) |>
  set_names("date", "vnv") |>
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)),
    vnv = vnv / lag(vnv, 12) - 1
  ) |>
  drop_na()


(laun_tbl |>
  left_join(vnv_tbl) |>
  pivot_longer(cols = -date) |>
  ggplot(aes(date, value, col = name)) +
  geom_line()) |>
  plotly::ggplotly()

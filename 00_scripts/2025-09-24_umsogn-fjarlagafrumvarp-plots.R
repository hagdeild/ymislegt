# Myndir fyrir umsögn um frumvarp til fjárlaga 2026
#
# Umsögn: H:\VR\Umsagnir\Fjárlög 2026
#

# 1.0.0 SETUP ----
library(tidyverse)

husnaedi_tbl <- readxl::read_excel(
  "00_data/umsogn_fjarlog_gogn.xlsx",
  sheet = 1
)
barnabaetur_tbl <- readxl::read_excel(
  "00_data/umsogn_fjarlog_gogn.xlsx",
  sheet = 2
)

mannfjoldi_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/b7270af8-f998-4ba2-8107-547fca27a929"
) |>
  select(3, 4) |>
  set_names("ar", "mannfjoldi") |>
  group_by(ar) |>
  summarise(mannfjoldi = sum(mannfjoldi))

# 2.0.0 PLOT ----
litir <- c(
  "#1f546c",
  "#62c0ce"
)

# 2.1.0 Húsnæði ----
husnaedi_tbl |>
  set_names("date", "Laun", "Leiguverð") |>
  mutate(date = date(date), year = year(date)) |>
  group_by(year) |>
  summarise(Laun = mean(Laun), Leiguverð = mean(Leiguverð)) |>
  pivot_longer(cols = -year) |>
  ggplot(aes(year, value, col = name)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values = litir) +
  labs(
    x = NULL,
    y = NULL,
    title = "Vísitasla íbúðarhúsnæðis sett í hlutfall við laun og leiguverð"
  )

ggsave("01_output/umsogn_fjarlog_husnaedi.png", width = 8, height = 4, dpi = 96)


# 2.2.0 Barnabætur ----
barnabaetur_tbl |>
  janitor::clean_names() |>
  select(-c(heimild, vaxtabaetur)) |>
  left_join(mannfjoldi_tbl) |>
  #filter(ar >= 2020) |>
  mutate(barnabaetur = (barnabaetur / cpi) / mannfjoldi) |>

  ggplot(aes(ar, barnabaetur)) +
  geom_line() +
  theme_minimal() +
  theme(legend.position = "bottom", legend.title = element_blank()) +
  scale_color_manual(values = litir) +
  labs(
    x = NULL,
    y = NULL,
    title = "Barnabætur á föstu verðlagi"
  )

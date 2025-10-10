# Gagnasöfnun í gegnum Mínar síður að klikka. Skoða hvað þetta nær langt aftur.



# 1.0.0 - SETUP - ---------------------------------------------------------

library(tidyverse)
library(vr)

con <- vr_gagnagrunnur()


minar_sidur_tbl <- tbl(con, "MINARSIDUR_V") %>% 
  filter(ARMA >= "201801") %>% 
  as_tibble() %>% 
  janitor::clean_names()

minar_sidur_endurb_tbl <- tbl(con, "MINARSIDUR_ENDURBAETT_V") %>% 
  filter(ARMA >= "201801") %>% 
  as_tibble() %>% 
  janitor::clean_names()


# Eldri gögn
minar_sidur_202302_tbl <- read_csv("W:/Rwd/Launarannsokn/01_data_output/2023_Feb/2023-02-01_minar_sidur.csv")
minar_sidur_202209_tbl <- read_csv("W:/Rwd/Launarannsokn/01_data_output/2022_Sep/2022-09-01_minar_sidur.csv")
minar_sidur_202402_tbl <- read_csv("C:/Users/vidar/Documents/Rwd/Launarannsokn/01_data_output/2024_Feb/2024-02-01_minar_sidur.csv")
minar_sidur_202409_tbl <- read_csv("C:/Users/vidar/Documents/Rwd/Launarannsokn/01_data_output/2024_Sep/2024-09-01_minar_sidur.csv")



# 2.0.0 - GREINING - ------------------------------------------------------

minar_sidur_endurb_tbl %>% 
  janitor::clean_names() %>% 
  mutate(
    menntun_missing = if_else(is.na(menntun), 1, 0),
    mannaforrad_missing = if_else(is.na(mannaforrad), 1, 0)
  ) %>% 
  mutate(date = make_date(str_sub(arma, 1, 4), str_sub(arma, 5 ,6))) %>% 
  group_by(date) %>% 
  summarise(
    mannaforrad = mean(mannaforrad_missing),
    menntun = mean(menntun_missing)
  ) %>% 
  filter(date <= "2024-12-01") %>% 
  
  pivot_longer(cols = -date) %>% 
  ggplot(aes(date, value, col = name)) +
  geom_line() +
  theme_minimal() +
  theme(
    legend.title = element_blank()
  ) +
  labs(x = NULL, y = NULL)



minar_sidur_tbl %>% 
  filter(arma == "202201") %>% 
  filter(!is.na(menntun))


minar_sidur_tbl %>% 
  filter(arma == "202402") %>% 
  count(menntun)

minar_sidur_tbl %>% 
  filter(arma == "202402", menntun == "Ekki skráð")

minar_sidur_endurb_tbl %>% 
  filter(id == 70, arma == "202409") %>%
  glimpse()


#
# 2.1.0 Eldri gögn --------------------------------------------------------

calc_minar <- function(data) {
  data %>% 
    count(mannaforrad) %>% 
    arrange(desc(n)) %>% 
    head(6) 
}

minar_calc_202302_tbl <- calc_minar(minar_sidur_202302_tbl) %>% 
  mutate(date = "Febrúar 2023")

minar_calc_202209_tbl <- calc_minar(minar_sidur_202209_tbl) %>% 
  mutate(date = "September 2022")

minar_calc_202402_tbl <- calc_minar(minar_sidur_202402_tbl) %>% 
  mutate(date = "Febrúar 2024")

minar_calc_202409_tbl <- calc_minar(minar_sidur_202409_tbl) %>% 
  mutate(date = "September 2024")

bind_rows(
  minar_calc_202209_tbl,
  minar_calc_202302_tbl,
  minar_calc_202402_tbl,
  minar_calc_202409_tbl
) %>% 
  pivot_wider(names_from = date, values_from = n) %>% 
  writexl::write_xlsx("01_output/2025-03-10_minar-sidur-missing.xlsx")

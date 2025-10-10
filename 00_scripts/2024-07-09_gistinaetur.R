# Ferðaiðnaðurinn

library(tidyverse)


date_seq <- seq.Date(from = as.Date("1998-01-01"), to = as.Date("2024-12-01"), by = "month")

data_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/c388f022-984a-41ce-b416-1912fd74e001",
  locale = locale(encoding = "latin1")
) 

data_tbl <- data_tbl %>% 
  select(-c(1:2)) %>% 
  mutate(
    date = date_seq
  ) %>% 
  set_names("Íslendingar", "Útlendingar", "date") %>% 
  pivot_longer(cols = -date) %>% 
  mutate(value = as.numeric(value)) %>% 
  drop_na()


data_tbl %>% 
  filter(month(date) == 5) %>% 
  ggplot(aes(x = date, y = value, col = name)) +
  geom_line() +
  geom_point()

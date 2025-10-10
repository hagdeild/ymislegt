# Plot fyrir niðurstöður kannana í forsetakosningum 2024


# 1.0.0 Setup -------------------------------------------------------------

library(tidyverse)

data_tbl <- readxl::read_excel("00_data/forsetakosningar_2024.xlsx") %>% 
  mutate(date = date(date))




# 2.0.0 Greining ----------------------------------------------------------

data_tbl %>% 
  pivot_longer(cols = -date) %>% 
  ggplot(aes(x = date, y = value, col = name)) +
  geom_line() 
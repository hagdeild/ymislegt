# Skortur á efnislegum gæðum



# 1.0.0 - SETUP - ---------------------------------------------------------

library(tidyverse)


data_tbl <- read_csv2("https://px.hagstofa.is:443/pxis/sq/698adc45-2a0a-4eda-9ffe-edf6adc0ca43")


litir <- c(
  "#2f2e2e",
  "#00a08a",
  "#cca71c",
  "#dc1e35"
)

# 2.0.0 - PLOT - ----------------------------------------------------------

data_tbl %>% 
  set_names("date", "stada", "skortur") %>% 
  mutate(skortur = as.numeric(skortur) / 1000) %>% 
  
  ggplot(aes(date, skortur, col = stada)) + 
  geom_line() +
  geom_point() + 
  scale_y_continuous(labels = scales::percent, limits = c(0, 0.2)) +
  scale_color_manual(values = litir[1:2]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Hlutfall heimila sem býr við efnislegan skort eftir stöðu á húsnæðismarkaði"
  ) + 
  theme_minimal() +
  theme(
    legend.title = element_blank(),
    legend.position = "bottom"
  ) +
  scale_x_continuous(breaks = 2008:2023)
  

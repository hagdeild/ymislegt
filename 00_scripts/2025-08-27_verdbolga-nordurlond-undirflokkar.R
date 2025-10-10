# Samræmdur mælikvarði á verðlag - Samanburður undirflokka


# SETUP ----
library(tidyverse)
library(eurostat)


# Data

hicp_raw_tbl <- get_eurostat(
  id = "prc_hicp_midx",
  filters = list(
    geo   = c("IS", "DK", "NO", "FI", "SE", "EU27_2020"),
    coicop = c(
      paste0("CP0", 0:9),
      "CP10", "CP11", "CP12",
      "TOT_X_NRG", "SERV", "GD", "FOOD"
    ),
    unit  = "I15",
    freq  = "M"
  ),
  time_format = "date",
  cache = FALSE
) %>% 
  as_tibble()


if (class(unique(hicp_raw_tbl$time)) == "Date") {
  hicp_tbl <- hicp_raw_tbl %>% 
    select(-c(unit, freq)) %>% 
    rename(
      "date" = "time",
      "flokkur" = "coicop",
      "svaedi" = "geo",
      "value" = "values"
      )
  
} else {
  hicp_tbl <- hicp_raw_tbl %>% 
    mutate(date = ym(time)) %>% 
    select(-c(time, unit, freq)) %>% 
    rename(
      "flokkur" = "coicop",
      "svaedi" = "geo",
      "value" = "values"
    )
  
}


hicp_infl_tbl <- hicp_tbl %>% 
  arrange(date, flokkur, svaedi) %>% 
  group_by(flokkur, svaedi) %>% 
  mutate(infl = value / lag(value, 12) - 1,
         infl_3m = value / lag(value, 3) - 1) %>% 
  drop_na() %>% 
  ungroup()


# Finn minnsta samnefnara í date
max_date_hicp <- hicp_infl_tbl %>% 
  filter(flokkur == "CP00") %>% 
  group_by(svaedi) %>% 
  filter(date == max(date)) %>%
  ungroup() %>% 
  count(date) %>% 
  filter(n == max(n)) %>% 
  pull(date)

hicp_infl_tbl <- hicp_infl_tbl %>% 
  filter(date <= max_date_hicp)


# Preparation for power bi
land_tbl <- tibble(
  svaedi = c("DK", "FI", "IS", "NO", "SE", "EU27_2020"),
  country = c("Danmörk", "Finnland", "Ísland", "Noregur", "Svíþjóð", "EU")
)

hicp_pbi_tbl <- hicp_infl_tbl %>% 
  filter(flokkur == "CP00") %>% 
  left_join(land_tbl)


# 5.2.0 By Coicip ----
coicop_flokkar_tbl <- tibble(
  coicop = paste0("CP", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")),
  flokkur = c("Matur og drykkjarvörur", "Áfengi og tóbak", "Föt og skór", "Húsnæði, hiti og rafmagn", "Húsgögn, heimilisbúnaður o.fl.", "Heilsa",
              "Ferðir og flutningar", "Póstur og sími", "Tómstundir og menning", "Menntun", "Hótel og veitingastaðir", "Aðrar vörur og þjónusta")
)

coicop_inflation_tbl <- hicp_raw_tbl %>% 
  filter(coicop %in% paste0("CP", c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12"))) %>% 
  filter(
    #geo %in% c("IS", "EU27_2020"),
    unit == "I15",
    freq == "M"
  ) %>%
  arrange(coicop, geo, time) %>% 
  group_by(coicop, geo) %>% 
  mutate(inflation = values / lag(values, 12) - 1) %>% 
  left_join(coicop_flokkar_tbl) %>% 
  rename("date" = "time")



# Plot
date_from <- c("2000-01-01", "2005-01-01", "2010-01-01", "2015-01-01", "2020-01-01")

for (d in date_from) {
  
  # Extract year for labeling and filenames
  year_d <- lubridate::year(d)
  
  # Generate plot
  p <- coicop_inflation_tbl |> 
    ungroup() |> 
    drop_na(values) |> 
    filter(date >= d) |> 
    arrange(date, geo, flokkur) |> 
    group_by(geo, flokkur) |> 
    mutate(values = values / values[1] * 100) |> 
    rename("svaedi" = "geo") |> 
    left_join(land_tbl) |> 
    ggplot(aes(date, values, col = country)) + 
    geom_line() + 
    facet_wrap(~ flokkur) +
    scale_color_brewer(palette = "Dark2") +
    labs(
      x = NULL, 
      y = NULL, 
      title = paste0("Þróun verðlags frá janúar ", year_d, " á Norðurlöndunum")
    ) +
    theme(
      legend.title = element_blank(), 
      legend.position = "bottom"
    )
  
  # Save plot with year in filename
  ggsave(
    filename = paste0("01_output/vnv_undirlidir_nordics_", year_d, ".png"), 
    plot = p,
    width = 14, 
    height = 8, 
    dpi = 96
  )
}


# Húsnæði hiti og rafmagn ----

# Original code with COICOP 04 subcategories added
hicp_04_tbl <- get_eurostat(
  id = "prc_hicp_midx",
  filters = list(
    geo   = c("IS", "DK", "NO", "FI", "SE", "EU27_2020"),
    coicop = c(     
      # COICOP 04 subcategories (3-digit level)
      "CP041",  # Actual rentals for housing
      "CP042",  # Calculated rent
      "CP043",  # Maintenance and repair of the dwelling
      "CP044",  # Water supply and miscellaneous services relating to the dwelling
      "CP045",  # Electricity, gas and other fuels
      
      # COICOP 04 subcategories (4-digit level)
      "CP0431", # Materials for the maintenance and repair of the dwelling
      "CP0432", # Services for the maintenance and repair of the dwelling
      "CP0441", # Water supply
      "CP0442", # Refuse collection
      "CP0443", # Sewerage collection
      "CP0444", # Other services relating to the dwelling n.e.c.
      "CP0451", # Electricity
      "CP0452", # Gas
      
      # COICOP 04 subcategories (5-digit level) - available from 2016
      "CP04321", # Services of plumbers
      "CP04322", # Services of electricians
      "CP04323", # Maintenance services for heating systems
      "CP04324", # Services of painters
      "CP04325", # Services of carpenters
      "CP04329", # Other services for maintenance and repair of the dwelling
      "CP04441", # Maintenance charges in multi-occupied buildings
      "CP04442", # Security services
      "CP04449", # Other services related to dwelling
      "CP04521" # Natural gas and town gas
    ),
    unit  = "I15",
    freq  = "M"
  ),
  time_format = "date",
  cache = FALSE
) %>% 
  as_tibble()

coicop_names_tbl <- tibble(
  coicop = c("CP041", "CP043", "CP0431", "CP0432", "CP044", "CP0441", "CP0442", "CP0443", "CP045", "CP0451"),
  flokkur = c("Greidd húsaleiga", "Viðhald og viðgerðir á húsnæðis", "Viðhald efni", "Viðhaldsþjónusta", "Annað vegna húsnæsðis",
"Sorphreinsun", "Holræsi", "Vatn", "Rafmagn og hiti", "Rafmagn")
)

for (d in date_from) {
  year_d <- year(ymd(d))

  p <- hicp_04_tbl |>
    filter(
      unit == "I15",
      freq == "M"
    ) |>
    drop_na() |>
    filter(!coicop %in% c("CP0451", "CP0444")) |>
    select(-c(freq, unit)) |>
    arrange(coicop, geo, time) |>
    rename(date = time) |>
    ungroup() |>
    filter(date >= as.Date(d)) |>
    arrange(date, geo, coicop) |>
    group_by(geo, coicop) |>
    mutate(values = values / values[1] * 100) |>
    rename(svaedi = geo) |>
    left_join(land_tbl) |>
    group_by(coicop) |>
    filter("Ísland" %in% country) |>
    ungroup() |>
    left_join(coicop_names_tbl) |>
    ggplot(aes(date, values, col = country)) +
    geom_line() +
    facet_wrap(~ flokkur, scales = "free_y") +
    scale_color_brewer(palette = "Dark2") +
    labs(
      x = NULL, y = NULL,
      title = paste0("Þróun verðlags frá janúar ", year_d,
                     " á Norðurlöndunum - Húsnæðis, hiti og rafmagn")
    ) +
    theme(legend.title = element_blank(), legend.position = "bottom")

  ggsave(
    filename = paste0("01_output/vnv_undirlidir_04_nordics_", year_d, ".png"),
    plot = p, width = 14, height = 8, dpi = 96
  )
}
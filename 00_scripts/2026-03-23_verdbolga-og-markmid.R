# Hve oft hefur verðbólga verið við eða undir markmiði Seðlabankans?

# 1.0.0 SETUP ----
library(tidyverse)


data_tbl <- read_csv2(
  "https://px.hagstofa.is:443/pxis/sq/b5fadd50-1f3b-4070-b164-37f024141547"
) |>
  set_names("date", "value") |>
  mutate(
    date = make_date(str_sub(date, 1, 4), str_sub(date, 6, 7)),
    value = value / lag(value, 12) - 1
  ) |>
  drop_na()


# 2.0.0 ÍSLENSKA ----
target <- 0.025

plot_tbl <- data_tbl %>%
  arrange(date) %>%
  mutate(
    on_target = value <= target,
    spell_id = cumsum(
      on_target != dplyr::lag(on_target, default = first(on_target))
    )
  )

spell_tbl <- plot_tbl %>%
  filter(on_target) %>%
  count(spell_id, name = "months_in_spell")

total_months <- nrow(plot_tbl)
months_on_target <- sum(plot_tbl$on_target, na.rm = TRUE)
pct_on_target <- mean(plot_tbl$on_target, na.rm = TRUE)
episodes_on_target <- sum(
  plot_tbl$on_target & !dplyr::lag(plot_tbl$on_target, default = FALSE),
  na.rm = TRUE
)
longest_spell <- if (nrow(spell_tbl) == 0) {
  0L
} else {
  max(spell_tbl$months_in_spell)
}

pct_txt <- scales::percent(
  pct_on_target,
  accuracy = 0.1,
  decimal.mark = ","
)

timabil_ord <- if (episodes_on_target == 1) {
  "aðskilið tímabil"
} else {
  "aðskilin tímabil"
}

manudur_ord <- if (longest_spell == 1) {
  "mánuður"
} else {
  "mánuðir"
}

title_txt <- sprintf(
  "Í %s af %s mánuðum mældist verðbólga 2,5%% eða lægri",
  scales::comma(months_on_target, big.mark = ".", decimal.mark = ","),
  scales::comma(total_months, big.mark = ".", decimal.mark = ",")
)

subtitle_txt <- sprintf(
  "Það jafngildir %s tímabilsins | %s %s | lengsta samfellda tímabil: %s %s",
  pct_txt,
  scales::comma(episodes_on_target, big.mark = ".", decimal.mark = ","),
  timabil_ord,
  scales::comma(longest_spell, big.mark = ".", decimal.mark = ","),
  manudur_ord
)

label_txt <- sprintf(
  "2,5%% eða lægri\n%s af %s mánuðum\n%s tímabilsins",
  scales::comma(months_on_target, big.mark = ".", decimal.mark = ","),
  scales::comma(total_months, big.mark = ".", decimal.mark = ","),
  pct_txt
)

ggplot(plot_tbl, aes(date, value)) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = target,
    fill = "#d9f0d3",
    alpha = 0.55
  ) +
  geom_line(color = "#0b3954") +
  geom_point(
    data = subset(plot_tbl, on_target),
    size = 0.8,
    color = "#087f5b"
  ) +
  geom_hline(
    yintercept = target,
    linetype = "22",
    linewidth = 0.9,
    color = "#b02a37"
  ) +
  # annotate(
  #   "label",
  #   x = Inf,
  #   y = Inf,
  #   hjust = 1.05,
  #   vjust = 1.1,
  #   label = label_txt,
  #   size = 3.8,
  #   linewidth = 0.25,
  #   fill = "white",
  #   color = "#111111"
  # ) +
  scale_y_continuous(
    labels = scales::label_percent(accuracy = 0.1, decimal.mark = ",")
  ) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = NULL,
    y = NULL,
    caption = "Rauða línan sýnir 2,5% verðbólgumarkmið Seðlabankans"
  ) +
  theme_minimal(base_size = 18) +
  theme(panel.grid.minor = element_blank())


ggsave(
  "01_output/2026-03-23_verdbolga-og-markmid.png",
  width = 10,
  height = 6,
  dpi = 150
)


# 3.0.0 ENSKA ----
target <- 0.025

plot_tbl <- data_tbl %>%
  arrange(date) %>%
  mutate(
    on_target = value <= target,
    spell_id = cumsum(
      on_target != dplyr::lag(on_target, default = first(on_target))
    )
  )

spell_tbl <- plot_tbl %>%
  filter(on_target) %>%
  count(spell_id, name = "months_in_spell")

total_months <- nrow(plot_tbl)
months_on_target <- sum(plot_tbl$on_target, na.rm = TRUE)
pct_on_target <- mean(plot_tbl$on_target, na.rm = TRUE)
episodes_on_target <- sum(
  plot_tbl$on_target & !dplyr::lag(plot_tbl$on_target, default = FALSE),
  na.rm = TRUE
)
longest_spell <- if (nrow(spell_tbl) == 0) {
  0L
} else {
  max(spell_tbl$months_in_spell)
}

title_txt <- sprintf(
  "Inflation at or below the 2.5%% target in %s of months",
  scales::percent(pct_on_target, accuracy = 0.1)
)

subtitle_txt <- sprintf(
  "%s of %s months | %s separate periods | longest stretch: %s months",
  scales::comma(months_on_target),
  scales::comma(total_months),
  episodes_on_target,
  longest_spell
)

label_txt <- sprintf(
  "At/below target\n%s / %s months\n%s",
  scales::comma(months_on_target),
  scales::comma(total_months),
  scales::percent(pct_on_target, accuracy = 0.1)
)

ggplot(plot_tbl, aes(date, value)) +
  annotate(
    "rect",
    xmin = -Inf,
    xmax = Inf,
    ymin = -Inf,
    ymax = target,
    fill = "#d9f0d3",
    alpha = 0.55
  ) +
  geom_line(color = "#0b3954") +
  geom_point(
    data = subset(plot_tbl, on_target),
    size = 0.8,
    color = "#087f5b"
  ) +
  geom_hline(
    yintercept = target,
    linetype = "22",
    linewidth = 0.9,
    color = "#b02a37"
  ) +
  # annotate(
  #   "label",
  #   x = Inf, y = Inf,
  #   hjust = 1.05, vjust = 1.1,
  #   label = label_txt,
  #   size = 3.8,
  #   linewidth = 0.25,
  #   fill = "white", color = "#111111"
  # ) +
  scale_y_continuous(labels = scales::label_percent(accuracy = 0.1)) +
  labs(
    title = title_txt,
    subtitle = subtitle_txt,
    x = NULL,
    y = NULL,
    caption = "Red line = inflation target (2.5%)"
  ) +
  theme_minimal(base_size = 16) +
  theme(panel.grid.minor = element_blank())

ggsave(
  "01_output/2026-03-23_verdbolga-og-markmid_en.png",
  width = 10,
  height = 6,
  dpi = 150
)

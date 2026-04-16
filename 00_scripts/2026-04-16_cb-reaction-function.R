# Seðlabankis reaction function
# RS_t = 0.6*RS_{t-1} + 0.4*[(RRN_t + IT_t) + 1.5*(INFUL_t - IT_t) + 0.5*GAPAV_t]

# 1.0.0 SETUP ----
library(tidyverse)
library(readxl)
library(zoo)

# 2.0.0 LOAD DATA ----
raw <- read_excel("00_data/cb_reaction.xlsx") |>
  mutate(date = as.Date(as.yearqtr(date, format = "%YQ%q")))

# 3.0.0 FORECAST INPUTS ----
h <- 8 # forecast horizon in quarters
fc_dates <- seq(max(raw$date) %m+% months(3), by = "quarter", length.out = h)

# INFUL: CB quarterly CPI forecast (decimal)
inful_fc <- c(
  0.050,
  0.042,
  0.040,
  0.038, # 2026Q1-Q4
  0.029,
  0.028,
  0.027,
  0.026
) # 2027Q1-Q4

# GAPAV: CB annual forecast, broadcast to 4 quarters per year
gapav_annual <- c("2026" = -0.002, "2027" = -0.001)
gapav_fc <- rep(gapav_annual, each = 4)

# IT: constant
it_fc <- rep(0.025, h)

# RRN: CB is drifting it linearly toward IT. Last obs 2025Q3 = 0.0225,
# step ~0.00075/qtr recently. Extrapolate, cap at IT. h+1 values cover 2025Q4 + 8 fc qtrs.
rrn_last <- tail(na.omit(raw$RRN), 1)
rrn_step <- 0.00075
rrn_fc_full <- pmin(rrn_last + rrn_step * seq_len(h + 1), 0.025)

# 4.0.0 BUILD FORECAST FRAME ----
# Patch 2025Q4 (NA in RRN/GAPAV). GAPAV 2025 annual = -0.003.
raw$RRN[raw$date == as.Date("2025-10-01")] <- rrn_fc_full[1]
raw$GAPAV[raw$date == as.Date("2025-10-01")] <- -0.003

fc <- tibble(
  date = fc_dates,
  RS = NA_real_,
  RRN = rrn_fc_full[-1],
  IT = it_fc,
  INFUL = inful_fc,
  GAPAV = gapav_fc
)

df <- bind_rows(raw, fc) |> arrange(date)

# 5.0.0 RECURSIVE POLICY RATE ----
rho <- 0.6
lambda <- 0.4
phi_pi <- 1.5
phi_y <- 0.5

start_idx <- which(is.na(df$RS))[1]
for (i in start_idx:nrow(df)) {
  target <- (df$RRN[i] + df$IT[i]) +
    phi_pi * (df$INFUL[i] - df$IT[i]) +
    phi_y * df$GAPAV[i]
  df$RS[i] <- rho * df$RS[i - 1] + lambda * target
}

# 6.0.0 OUTPUT ----
df |>
  filter(date >= as.Date("2024-01-01")) |>
  mutate(across(c(RS, RRN, IT, INFUL, GAPAV), ~ round(.x * 100, 2))) |>
  print(n = Inf)

plot_df <- df |> filter(date >= as.Date("2015-01-01"))
cutoff <- as.Date("2025-10-01")

p_cb <- ggplot(plot_df, aes(date, RS)) +
  geom_line(data = plot_df |> filter(date <= cutoff), linewidth = 0.8) +
  geom_line(
    data = plot_df |> filter(date >= cutoff),
    linewidth = 0.8,
    linetype = "dashed"
  ) +
  geom_vline(xintercept = cutoff, linetype = "dashed") +
  scale_y_continuous(labels = scales::percent_format(accuracy = 0.1)) +
  labs(
    title = "Viðbragðsfall fyrir stýrivexti Seðlabankans",
    x = NULL,
    y = NULL,
    caption = "Stuðs við spár seðlabankans fyrir framleiðsluspennu og verðbólgu. Í raun ætti að nota undirliggjandi verðbólgu (INFUL) en seðlabankinn birtir hana ekki"
  ) +
  theme_minimal() +
  scale_x_date(breaks = "2 years")

plotly::ggplotly(p_cb)

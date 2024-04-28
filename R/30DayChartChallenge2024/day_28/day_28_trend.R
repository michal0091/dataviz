# 30DayChartChallenge2024 / day_28_trend

# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(ggforce)
library(ggtext)
library(ggrepel)
library(emojifont)
library(sysfonts)
library(showtext)
library(ggplot2)
library(tidyverse)
library(zoo)
library(seasonal)
library(forecast)
library(Timese)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_28 <- cfg$day_28


# Data --------------------------------------------------------------------
dt <- fread("R/30DayChartChallenge2024/day_28/trend_tourist.csv")

# Tráfico de pasajeros registrados en los aeropuertos de las islas de Canarias
# Llegadas de pasajeros extranjeros y España (excluida Canarias)
#
setnames(dt,
         c("Fecha", "Extranjeros y España (excluida Canarias)"),
         c("date", "passengers"))

dt <- dt[nchar(date) != 4]
dt[, date := as.Date(as.yearmon(date, "%m/%Y"))]
dt <- dt[order(date)]

# Fix annomaly
n_periods <- dt[date >= "2020-02-01" & date <= "2021-12-01", .N]
dt_sub <- dt[date < "2020-02-01"]

dt_sub_ts <-
  ts(dt_sub$passengers,
     frequency = 12,
     start = c(2004, 1))

fit_arima <- dt_sub_ts %>% auto.arima()
fit_arima_name <- as.character(fit_arima)
fc <- forecast(fit_arima, h = n_periods)[["mean"]]
fc_dt <-
  data.table(date = dt[date >= "2020-02-01" &
                         date <= "2021-12-01", date], passengers = as.numeric(fc))
dt <-
  merge(dt,
        fc_dt,
        by = "date",
        suffixes = c("", "_fc"),
        all = TRUE)

dt[, passengers_with_fc := fifelse(is.na(passengers_fc), passengers, passengers_fc)]


dt_ts <-
  ts(dt$passengers_with_fc,
     frequency = 12,
     start = c(2004, 1))
fit_arima_2 <- dt_ts %>% auto.arima()
fit_arima_2_name <- as.character(fit_arima_2)
fitted_dt <-
  data.table(date = dt$date, passengers = as.integer(fitted(fit_arima_2)))

dt <-
  merge(
    dt,
    fitted_dt,
    by = "date",
    suffixes = c("", "_fitted"),
    all = TRUE
  )


dt_ts <- ts(dt, frequency = 12, start = c(2004, 1))

trend_fitted <- dt_ts[, "passengers_fitted"] %>%
  stl(t.window = 13,
      s.window = "periodic",
      robust = TRUE)
trend_original <- dt_ts[, "passengers"] %>%
  stl(t.window = 13,
      s.window = "periodic",
      robust = TRUE)

trend_fitted <- as.numeric(trend_fitted$time.series[, "trend"])
trend_original <- as.numeric(trend_original$time.series[, "trend"])

# add trend to data
dt[, `:=`(trend_fitted = trend_fitted,
          trend_original = trend_original)]



# Plot --------------------------------------------------------------------
color_background <- "#fffefd"
col_1 <- "#0768A9"
col_2 <- "#043b60"
col_3 <- "#d41538"
col_4 <- "#8E0E26"
col_text_1 <- "#28334b"
col_text_2 <- "#3b495b"


plot <- ggplot(dt[date >= "2015-01-01"], aes(x = date)) +
  geom_line(aes(
    y = passengers,
    color = "Original",
    linetype = "Original"
  ),
  linewidth = .65) +
  geom_line(aes(
    y = passengers_fitted,
    color = "Estimated",
    linetype = "Estimated"
  ),
  linewidth = .45) +
  geom_line(
    aes(
      y = trend_original,
      color = "Trend original",
      linetype = "Trend original"
    ),
    linewidth = .75
  ) +
  geom_line(
    aes(
      y = trend_fitted,
      color = "Trend estimated",
      linetype = "Trend estimated"
    ),
    linewidth = .55
  ) +
  scale_color_manual(
    values = c(
      "Original" = col_1,
      "Trend original" = col_2,
      "Estimated" = col_3,
      "Trend estimated" = col_4
    )
  ) +
  scale_linetype_manual(
    values = c(
      "Original" = "solid",
      "Estimated" = "solid",
      "Trend original" = "dashed",
      "Trend estimated" = "dashed"
    ),
    guide = "none"
  ) +
  labs(
    title = "Passenger traffic at Canary Islands airports",
    subtitle = "Foreign passenger arrivals and Spain (excluding Canary Islands)",
    x = NULL,
    y = NULL,
    color = NULL
  ) +
  theme_my(
    font_regular     = "inter_regular",
    font_bold        = "inter_bold",
    font_light       = "inter_light",
    color_text_1 = col_text_1,
    color_text_2 = col_text_2,
    color_background = color_background
  ) +
  theme(
    legend.position = "bottom",
    axis.text = element_text(
      family = "roboto_regular",
      size = 26,
      hjust = .5
    ),
    plot.margin = margin(25, 45, 25, 25, "pt"),
    plot.title.position = "plot",
    legend.spacing = unit(5, "pt"),
    legend.text = element_text(family = "roboto_regular",
                               size = 26)
  )



# Titles theme
tit_theme <-
  theme_void() +
  theme(plot.background = element_rect(color = color_background, fill =
                                         color_background))



n_visitors <- dt[date >= "2020-01-01" & date <= "2022-01-01", sum(trend_fitted) - sum(trend_original)]

n_visitors <- formatC(as.integer(n_visitors), big.mark = ",")

subtitle_text <- data.table(
  x = 0,
  y = 0,
  label = paste(
    "The blue lines represent the actual number of foreign visitors over",
    "time and their trend. The red lines represent a statistical estimate of",
    "the number of foreign visitors and their trend, excluding the",
    "influence of the COVID-19 pandemic. This estimate incorporates",
    "time series models such as ARIMA and a Seasonal-Trend-Loess (STL)",
    "decomposition model that is used to estimate the underlying trend in arrivals.",
    "As a consequence between January 2020 and January 2022 there is an estimated gap of",
    n_visitors, "arrivals."
  )
)

subtitle <-
  ggplot(subtitle_text, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = color_background,
    box.padding = unit(c(25, 15, 25, 35), "pt"),
    fill = color_background,
    family = "roboto_regular",
    box.r = unit(0, "pt"),
    width = unit(22, "cm"),
    size = rel(11),
    lineheight = .5,
    color = "#1f2937"
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  tit_theme

combined_plot <- subtitle / plot +
  plot_layout(heights = c(.25, .75), design = "
                         AAAAA#
                         BBBBBB
                         ") +
  plot_annotation(
    title = "Covid-19 impact on passenger traffic at Canary Islands airports",
    caption = caption_text(
      source       = cfg_day_28$source,
      day_type     = cfg_day_28$category,
      day_hashtag  = cfg_day_28$theme,
      day          = cfg_day_28$day,
      color_text_1 = col_text_1,
      color_text_2 = col_text_2
    ),
    theme =   theme_my(
      font_regular     = "inter_regular",
      font_bold        = "inter_bold",
      font_light       = "inter_light",
      color_text_1 = col_text_1,
      color_text_2 = col_text_2,
      color_background = color_background
    )  +
      theme(
        plot.margin = margin(15, 0, 10, 0, "pt"),
        plot.title.position = "plot",
        plot.title = element_text(size = 56),
        plot.caption.position = "plot",
        plot.caption =  element_textbox_simple(
          size = 26,
          lineheight = .5,
          padding = margin(0, 0, 0, 0, "pt"),
          margin = margin(5, 0, 5, 0, "pt"),
        )
      )
  )

# Save --------------------------------------------------------------------
ggsave(
  "R/30DayChartChallenge2024/day_28/day_28_trend.png",
  combined_plot,
  width = 3106.56,
  height = 1920,
  units = "px",
  dpi = 320
)


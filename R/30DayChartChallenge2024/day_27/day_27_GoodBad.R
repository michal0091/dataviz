# 30DayChartChallenge2024 / day_27_GoodBad

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
library(mlmhelpr)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_27 <- cfg$day_27


# Data --------------------------------------------------------------------
dt <-
  fread("R/30DayChartChallenge2024/day_27/forecast.csv", dec = ",")
dt <-
  melt(dt,
       id.vars = "previsiones",
       variable.name = "year",
       value.name = "forecast")
dt[, year := fifelse(year == "pib_2025", "GDP 2025 forecast", "GDP 2024 forecast")]
dt[, forecast := forecast / 100]
dt[, x_jit := jitter(as.numeric(as.factor(year)), factor = .5)]


# Get min forecast of each year
min_forecast <-
  dt[, .(x_jit = x_jit[forecast == min(forecast)], forecast = min(forecast)), year]
max_forecast <-
  dt[, .(x_jit = x_jit[forecast == max(forecast)], forecast = max(forecast)), year]
max_forecast <- max_forecast[, first(.SD), year]

plot <- dt[, ggplot(.SD, aes(year, forecast)) +
             geom_boxplot(
               width = .5,
               outliers = FALSE,
               staplewidth = 0.5,
               fill = NA,
               color = "#1f2937"
             ) +
             
             geom_point(aes(x_jit, forecast), color = "#4b5563") +
             geom_text_repel(
               data = min_forecast,
               aes(x_jit, forecast, label = paste0(
                 "Worst: ", scales::percent(forecast, accuracy = .01)
               )),
               family = "inter_bold",
               size = rel(12),
               direction = "y",
               color = "#030712",
               segment.size = 0.5,
               nudge_x = 0.5,
               nudge_y = c(0.001, 0.0015),
               segment.curvature = 0.2,
               segment.color = "#1f2937"
             ) +
             geom_text_repel(
               data = max_forecast,
               aes(x_jit, forecast, label = paste0(
                 "Best: ", scales::percent(forecast, accuracy = .01)
               )),
               family = "inter_bold",
               size = rel(12),
               direction = "y",
               color = "#030712",
               segment.size = 0.5,
               nudge_x = 0.5,
               nudge_y = c(0.0015,-0.001),
               segment.curvature = 0.2,
               segment.color = "#1f2937"
             ) +
             
             scale_y_continuous(labels = scales::percent) +
             labs(
               title = "GDP Forecast 2024 and 2025",
               subtitle = "for Spanish economy",
               x = NULL,
               y = NULL,
               caption = caption_text(
                 source       = cfg_day_27$source,
                 day_type     = cfg_day_27$category,
                 day_hashtag  = cfg_day_27$theme,
                 day          = cfg_day_27$day,
                 color_text_1 = "#030712",
                 color_text_2 = "#1f2937"
               )
             ) +
             theme_my(
               font_regular     = "inter_regular",
               font_bold        = "inter_bold",
               font_light       = "inter_light",
               color_text_1     = "#030712",
               color_text_2     = "#1f2937",
               color_background = "#d1d5db",
               title_size       = 48
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
               plot.caption.position = "plot",
               plot.caption =  element_textbox_simple(
                 size = 26,
                 lineheight = .5,
                 padding = margin(0, 0, 0, 0, "pt"),
                 margin = margin(25, 0, 0, 0, "pt"),
               )
             )]



# Save --------------------------------------------------------------------
ggsave(
  "R/30DayChartChallenge2024/day_27/day_27_GoodBad.png",
  plot,
  width = 1920,
  height = 1920,
  units = "px",
  dpi = 320
)


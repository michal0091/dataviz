# 30DayChartChallenge2024 / day_8_circular

# Libraries ---------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggforce)
library(ggtext)
library(emojifont)
library(sysfonts)
library(showtext)
library(patchwork)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_8 <- cfg$day_8


# Data ----------------------------------------------------------------
dt <-
  readRDS("R/30DayChartChallenge2024/day_08/pm10_mean_zona_2001.RDS")

dt[, rolling_mean_10y := frollmean(mean_zona, 10, align = "right", fill = NA), zona]
dt[, rolling_sd_10y := frollapply(mean_zona, 10, sd, align = "right", fill = NA), zona]
dt[, yday := yday(fecha)]
dt[, ymin := rolling_mean_10y - 2 * rolling_sd_10y]
dt[, ymax := rolling_mean_10y + 2 * rolling_sd_10y]


plot <- dt[fecha >= "2023-01-01" &
             zona == "Interior M30", ggplot(.SD, aes(yday, mean_zona)) +
             geom_ribbon(data = .SD[fecha <= "2023-12-31"],
                         aes(ymin = ymin,
                             ymax = ymax,
                             fill = "± 2sd"),
                         alpha = 0.33) +
             geom_line(data = .SD[fecha <= "2023-12-31"], aes(color = "PM10 concentration")) +
             geom_line(data = .SD[fecha <= "2023-12-31"],
                       aes(y = rolling_mean_10y, color = "10y roll mean")) +
             scale_y_continuous(labels = scales::number_format(accuracy = 1)) +
             scale_color_manual(values = c("PM10 concentration" = c2_color_text_2,
                                           "10y roll mean" = c2_color_accent_2)) +
             scale_fill_manual(values = c("± 2sd" = c2_color_accent_1)) +
             labs(
               title = "Daily air quality in Madrid in 2023",
               subtitle = "PM10 mean concentration Interior M30 (Central Madrid)",
               x = NULL,
               y = "PM10 concentration (µg/m³)",
               fill = NULL,
               color = NULL,
               caption = caption_text(
                 source_text = cfg_day_8$source,
                 day_type =  cfg_day_8$category,
                 day_hashtag = cfg_day_8$theme,
                 day = cfg_day_8$day,
                 color_text_1 = c2_color_text_1,
                 color_text_2 = c2_color_text_2
               )
             ) +
             coord_radial(
               inner.radius = .25,
               rotate_angle = TRUE,
               expand = FALSE,
               r_axis_inside = FALSE,
               direction = 1
             ) +
             scale_x_continuous(breaks = cumsum(c(31, 28, 31, 30 , 31 , 31, 30, 31, 30, 31, 30, 31))) +
             theme_my(
               font_regular     = "roboto_regular",
               font_bold        = "roboto_bold",
               font_light       = "roboto_light",
               color_text_1     = c2_color_text_1,
               color_text_2     = c2_color_text_2,
               color_background = c2_color_background,
               title_size       = 24
             ) +
             theme(plot.margin = margin(25, 5, 5, 0, "pt"),
                   legend.position = "bottom",
                   legend.margin = margin(0, 0, 0, 0, "pt"),
                   legend.spacing = unit(5, "pt"),
                   legend.key.spacing = unit(5, "pt"),
                   legend.key.height = unit(6, 'pt'),
                   legend.key.width = unit(9.708, 'pt'),
                   axis.title.y = element_text(
                     hjust = 0.5,
                     vjust = 5
                   ))]

# Save --------------------------------------------------------------------
ggsave(
  filename = "day_8_circular.png",
  path = normalizePath("R/30DayChartChallenge2024/day_08"),
  plot = plot,
  device = "png",
  units = "px",
  width = 1080,
  height = 1080,
  dpi = 320
)

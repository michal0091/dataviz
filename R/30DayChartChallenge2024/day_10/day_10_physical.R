# 30DayChartChallenge2024 / day_10_physical

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
cfg_day_10 <- cfg$day_10


# Data --------------------------------------------------------------------
dt <- fread("R/30DayChartChallenge2024/day_10/physical.csv")
dt[, date := as.Date(date)]

# Plot --------------------------------------------------------------------
plot <- dt[, ggplot(.SD, aes(date, position)) +
             geom_hline(
               yintercept = 1,
               color = c2_color_text_2,
               linetype = "dashed",
               linewidth = 0.10
             ) +
             geom_line(color = c2_color_set_1, linewidth = 0.5) +
             geom_ribbon(aes(date, ymin = position, ymax = 100),
                         fill = c2_color_set_1,
                         alpha = .25) +
             
             scale_y_reverse(limits = c(100,-10),
                             breaks = c(1, 5, seq(10, 100, 10))) +
             scale_x_date(breaks = .SD[seq(1, 26, 3), date], date_labels = "%d %b %y") +
             labs(
               title = "Physical by Olivia Newton-John",
               subtitle = "weeks on top of the Billboard Hot 100 chart in 1981/82",
               x = NULL,
               y = "position",
               caption = caption_text(
                 source_text = cfg_day_10$source,
                 day_type =  cfg_day_10$category,
                 day_hashtag = cfg_day_10$theme,
                 day = cfg_day_10$day,
                 color_text_1 = c2_color_text_1,
                 color_text_2 = c2_color_text_2
               )
             ) +
             theme_my(
               font_regular     = "roboto_regular",
               font_bold        = "roboto_bold",
               font_light       = "roboto_light",
               color_text_1     = c2_color_text_1,
               color_text_2     = c2_color_text_2,
               color_background = c2_color_background,
               title_size       = 36
             ) +
             theme(
               plot.margin = margin(25, 10, 0, 15, "pt"),
               panel.grid.major = element_blank(),
               axis.text.y = element_text(
                 size = 14,
                 family = "roboto_bold",
                 color = c2_color_text_2
               ),
               axis.text.x = element_text(
                 size = 12,
                 hjust = 0,
                 family = "roboto_bold"
               )
             )]

# Save --------------------------------------------------------------------
ggsave(
  filename = "day_10_physical.png",
  path = normalizePath("R/30DayChartChallenge2024/day_10"),
  plot = plot,
  device = "png",
  units = "px",
  width = 1440,
  height = 1080,
  dpi = 320
)


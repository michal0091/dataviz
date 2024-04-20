# 30DayChartChallenge2024 / day_19_dinosaurs

# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(ggforce)
library(ggtext)
library(ggrepel)
library(emojifont)
library(sysfonts)
library(showtext)
library(patchwork)
library(lubridate)
library(zoo)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_19 <- cfg$day_19


# Data --------------------------------------------------------------------
dinos <- fread("R/30DayChartChallenge2024/day_19/multiTimeline.csv")

setnames(dinos, 
         old = c("Mes", "dinosaurs: (Todo el mundo)"),
         new = c("date", "searches"))

dinos[, date := lubridate::as_date(as.yearmon(date, format = "%Y-%m"))]


# 2009 - Ice Age: Dawn of the Dinosaurs - 1 July 2009
# 2013 - Walking with Dinosaurs - 20 December 2013
# 2015 - Jurassic World - 12 June 2015 
# 2018 - Jurassic World: Fallen Kingdom - 22 June 2018 
# 2022 - Jurassic World: Dominion - 10 June 2022

labels <- data.table(
  date = as.Date(c("2009-07-01", "2013-12-01", "2015-06-01", "2018-06-01", "2022-07-01")),
  label = c("Ice Age: Dawn of the Dinosaurs", "Walking with Dinosaurs", "Jurassic World", "Jurassic World: Fallen Kingdom", "Jurassic World: Dominion")
)
labels <- merge(labels, dinos, by = "date")

# Plot --------------------------------------------------------------------

plot <- dinos[, ggplot(.SD, aes(date, searches)) +
  geom_line(color = c4_color_set_1) +
  geom_point(data = .SD[date %in% labels$date], color = c4_color_set_1) +
  geom_text(
    data = labels,
    aes(label = label, x = date, y = searches),
    hjust = 0,
    vjust = -1,
    nudge_x = 1,
    family = "josefin_bold",
    color = c4_color_text_1,
    size = rel(9)
  ) +
  scale_x_date(
    date_breaks = "1 year",
    date_labels = "%Y",
    limits = c(as.Date("2005-01-01"), as.Date("2025-01-01")),
    expand = c(0, 0)
  ) +
  scale_y_continuous(
    expand = c(0, 0),
    limits = c(0, 100),
    labels = scales::number_format(accuracy = 1)
  ) +
    labs(
      title = "Dinosaurs searches on Google",
      subtitle = "2005 - 2024",
      x = "Year",
      y = "Interest over time", 
      caption = caption_text(
        source_text = cfg_day_19$source,
        day_type =  cfg_day_19$category,
        day_hashtag = cfg_day_19$theme,
        day = cfg_day_19$day, 
        color_text_1 = c4_color_text_1, 
        color_text_2 = c4_color_text_2
      )
    ) +
  theme_my(
    font_regular     = "josefin_regular",
    font_bold        = "josefin_bold",
    font_light       = "josefin_light",
    color_text_1     = c4_color_text_1,
    color_text_2     = c4_color_text_2,
    color_background = c4_color_background,
    title_size       = 50
  ) +
  theme(
    axis.title = element_blank(),
    legend.position = "none"
  ) +
  coord_cartesian(clip = "off") +
  theme(
    plot.margin = margin(25, 25, 10, 10, "pt"),
    plot.caption =  element_textbox_simple(
      size = 20,
      lineheight = .5,
      padding = margin(4, 0, 4, 0, "pt"),
      margin = margin(20, 0, 0, 0, "pt"),
    ),
    axis.text = element_text(
      size = rel(2),
      family = "josefin_bold",
      color = c4_color_text_2,
      margin = margin(2, 2, 2, 0, "pt")
    ),
    axis.title.y = element_text(
      size = rel(3),
      family = "josefin_bold",
      color = c4_color_text_2,
      margin = margin(5, 5, 5, 5, "pt"), 
      angle = 90
    ), 
    panel.grid.major = element_line(
      color = c4_color_text_2, 
      linetype = "dotted",
      size = .3, 
      
    )
  )]


# Save --------------------------------------------------------------------
ggsave(
  filename = "R/30DayChartChallenge2024/day_19/day_19_dinosaurs.png",
  plot = plot,
  width = 3106.56,
  height = 1920,
  units = "px",
  dpi = 320
)


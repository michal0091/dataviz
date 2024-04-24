# 30DayChartChallenge2024 / day_24_ILO_Africa

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
library(terra)
library(tidyterra)
library(giscoR)
library(sf)
# devtools::install_github("mtennekes/tmaptools")
library(tmaptools)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_24 <- cfg$day_24


# Data --------------------------------------------------------------------
dt <- fread("R/30DayChartChallenge2024/day_24/ilo_poverty.csv")
dt <- dt[!(ref_area.label %in% c("World", "Africa"))]

# Plot --------------------------------------------------------------------

plot <- dt[, ggplot(.SD, aes(time, obs_value / 100, color = ref_area.label )) +
     geom_line(linewidth = 1) +
     theme_my(
       font_regular     = "josefin_regular",
       font_bold        = "josefin_bold",
       font_light       = "josefin_light",
       color_text_1     = c4_color_text_1,
       color_text_2     = c4_color_text_2,
       color_background = c4_color_background,
       title_size       = 50
     ) +
     scale_color_manual(values = c4_col_set) +
     scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
     labs(
       title = " Working poverty rate (percentage of employed living below US$2.15 PPP)",
       subtitle = "Age (Youth, adults): 15+ years",
       caption = caption_text(
         source_text  = cfg_day_24$source,
         day_type     = cfg_day_24$category,
         day_hashtag  = cfg_day_24$theme,
         day          = cfg_day_24$day, 
         color_text_1 = c4_color_text_1, 
         color_text_2 = c4_color_text_2
       ),
       x = NULL,
       y = "Working poverty rate (%)",
       color = NULL
     ) +
     theme(
       legend.position = "bottom",
       plot.margin = margin(25, 0, 15, 0, "pt"),
       plot.title = element_text(
         margin = margin(5, 5, 5, 0, "pt")
       ),
       plot.subtitle = element_text(
         margin = margin(5, 5, 5, 5, "pt")
       ),
       axis.text = element_text(
         size = rel(2),
         family = "josefin_bold",
         color = c4_color_text_2,
         margin = margin(2, 2, 2, 0, "pt")
       ),
       axis.title.y = element_text(
         size = 32,
         family = "josefin_bold",
         color = c4_color_text_2,
         margin = margin(5, 5, 5, 5, "pt"), 
         angle = 90
       ), 
       panel.grid.major = element_line(
         color = c4_color_text_2, 
         linetype = "dotted",
         linewidth = .3, 
       )
     )]


# Save --------------------------------------------------------------------
ggsave(
  filename = "R/30DayChartChallenge2024/day_24/day_24_ILO_Africa.png",
  plot = plot,
  width = 2688,
  height = 1920,
  units = "px",
  dpi = 320
)

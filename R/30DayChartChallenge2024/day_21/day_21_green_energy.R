# 30DayChartChallenge2024 / day_21_green_energy

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
cfg_day_21 <- cfg$day_21



# Data --------------------------------------------------------------------
energy <- fread("R/30DayChartChallenge2024/day_21/energy.csv", dec = ",")

energy <- melt(energy, id.vars = "year", variable.name = "Country", value.name = "Share")
energy[, Country := factor(Country, levels = c("Portugal", "Italy", "Greece", "Spain", "EU27"),
                           labels = c("Portugal", "Italy", "Greece", "Spain", "EU27"))]
energy[, Share := Share / 100]


# Plot --------------------------------------------------------------------

plot <- energy[, ggplot(.SD, aes(year, Share, color = Country)) +
         geom_line() +
         geom_point() +
         labs(title = "Share of renewable energy in gross final energy consumption",
              subtitle = "Renewable energy sources in electricity in PIGS countries",
              x = NULL,
              y = "% of renewable energy",
              color = NULL,
              caption = caption_text(
                source_text  = cfg_day_21$source,
                day_type     = cfg_day_21$category,
                day_hashtag  = cfg_day_21$theme,
                day          = cfg_day_21$day, 
                color_text_1 = c4_color_text_1, 
                color_text_2 = c4_color_text_2
              )) +
         scale_y_continuous(labels = scales::percent_format(accuracy = 1),
                            limits = c(0, .8),
                            breaks = seq(0, .8, .2)) +
         scale_color_manual(values = c(
           "Portugal" = "#0D6938",
           "Italy"    = "#ce2b37",
           "Greece"   = "#0d5eaf",
           "Spain"    = "#F1BF00",
           "EU27"     = "#001489"
         )) +
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
           legend.position = "bottom",
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
             size = 32,
             family = "josefin_bold",
             color = c4_color_text_2,
             margin = margin(5, 5, 5, 5, "pt"), 
             angle = 90
           ), 
           panel.grid.major = element_line(
             color = c4_color_text_2, 
             linetype = "dotted",
             size = .3, 
           ),
           axis.title.y.right = element_text(
             size = 32,
             family = "josefin_bold",
             color = c4_color_text_2,
             margin = margin(5, 5, 5, 5, "pt"), 
             angle = -90
           )
         )
         ]


# Save --------------------------------------------------------------------
ggsave(
  filename = "R/30DayChartChallenge2024/day_21/day_21_green_energy.png",
  plot = plot,
  width = 3106.56,
  height = 1920,
  units = "px",
  dpi = 320
)


# 30DayChartChallenge2024 / day_14_heatmap

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


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_14 <- cfg$day_14


# Data --------------------------------------------------------------------
dt <- fread("R/30DayChartChallenge2024/day_14/no2_calendar_data.csv", encoding = "Latin-1")

# Update to english
Sys.setlocale("LC_TIME", "English")

dt[, wday := factor(wday(fecha, label = TRUE),
                    levels = c("Mon", "Tue", "Wed", "Thu", "Fri", "Sat", "Sun"))]
dt[, month := factor(
  month(fecha, label = TRUE),
  levels = c(
    "Jan",
    "Feb",
    "Mar",
    "Apr",
    "May",
    "Jun",
    "Jul",
    "Aug",
    "Sep",
    "Oct",
    "Nov",
    "Dec"
  )
)]


# Plot --------------------------------------------------------------------

plot <- dt[,
   ggplot(.SD, aes(
     x = wmonth,
     y = reorder(wday, -as.numeric(wday)),
     fill = valor_promedio
   )) +
     geom_tile(colour = "white") +
     facet_grid(year ~ month) +
     scale_x_continuous(breaks = 1:5, limits = c(0, 6)) +
     scale_fill_gradient(low = "#fedbd2", high = "#C20000", ) +
     labs(
       x = "Week of the month",
       y = NULL,
       title = "Nitrogen dioxide concentration in the city of Madrid",
       subtitle = "Average concentration of NO2 in the air by day of the week",
       fill = paste(unique(nom_abv), unique(ud_med)),
       caption = caption_text(
         source_text = cfg_day_14$source,
         day_type = cfg_day_14$category,
         day_hashtag = cfg_day_14$theme,
         day = cfg_day_14$day,
         color_text_1 = c2_color_text_1,
         color_text_2 = c2_color_text_2
       )
     ) +  
     theme_my(
       font_regular     = "redhat_regular",
       font_bold        = "redhat_bold",
       font_light       = "redhat_light",
       color_text_1     = c3_color_text_1,
       color_text_2     = c3_color_text_2,
       color_background = c3_color_background,
       title_size       = 56
     ) +
     theme(
       plot.margin = margin(15, 15, 5, 15, "pt"),
       plot.caption =  element_textbox_simple(
         size = 26,
         lineheight = .5,
         padding = margin(5, 5, 5, 5, "pt"),
         margin = margin(10, 0, 0, 0, "pt"),
       ),
       legend.position = "bottom",
       legend.spacing = unit(5, "pt"),
       legend.key.spacing = unit(5, "pt"),
       legend.key.height= unit(10, 'pt'),
       legend.key.width = unit(16.18, 'pt'),
       panel.grid.major = element_line(
         colour = c3_color_text_2,
         linetype = "solid",
         linewidth = .3
       ),
       axis.text = element_text(
         size = rel(1.6),
         family = "redhat_regular",
         color = c3_color_text_2, 
         margin = margin(2, 2, 2, 0, "pt") 
       )
     )
   
   ]


# Save --------------------------------------------------------------------
ggsave(
  filename = "R/30DayChartChallenge2024/day_14/day_14_heatmap.png",
  plot = plot,
  width = 2560,
  height = 2560,
  units = "px",
  dpi = 320)




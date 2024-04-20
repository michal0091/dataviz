# 30DayChartChallenge2024 / day_20_correlation

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
cfg_day_20 <- cfg$day_20


# Data --------------------------------------------------------------------
cpi_brent <- fread("R/30DayChartChallenge2024/day_20/cpi_brent.csv", dec = ",")
cpi_brent[, y_m := as.Date(as.yearmon(y_m, "%YM%m"))]
cpi_brent[, cpi := cpi / 100]


# Plot --------------------------------------------------------------------

b <- diff(cpi_brent[, c(min(brent_crude_oil_12m), max(brent_crude_oil_12m))]) / 
  diff(cpi_brent[,  c(min(cpi), max(cpi))])

plot <- cpi_brent[, ggplot(.SD, aes(y_m)) +
            geom_line(aes(y_m, brent_crude_oil_12m, color = "brent")) +
            geom_line(aes(y_m, cpi * b, color = "cpi")) +
            scale_y_continuous(labels = scales::percent,
                               limits = c(min(brent_crude_oil_12m), max(brent_crude_oil_12m)),
                               name = "Brent Crude Oil price (yoy)",
                               sec.axis = sec_axis(~ (. )/b, name = "CPI Spain (yoy)",
                                                   labels = scales::percent)) +
            scale_color_manual(values = c("cpi" = c4_color_set_1, "brent" = c4_color_set_2)) +
            annotate("text", x = as.Date("2022-01-01"), 
                     y = -0.5, 
                     label = paste("ρ:", round(cor(cpi, brent_crude_oil_12m, use = "complete.obs"), 2)), 
                     color = c4_color_text_2, 
                     hjust = 0,
                     size = rel(10),
                     family = "roboto_bold") +
            labs(
              title = "CPI and Brent Crude Oil year-on-year change",
              subtitle = "Correlation between Spanish CPI and the Brent Crude Oil Price",
              x = "Year",
              y = "Value",
              color = NULL,
              caption = caption_text(
                source_text = cfg_day_20$source,
                day_type =  cfg_day_20$category,
                day_hashtag = cfg_day_20$theme,
                day = cfg_day_20$day, 
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
  filename = "R/30DayChartChallenge2024/day_20/day_20_correlation.png",
  plot = plot,
  width = 3106.56,
  height = 1920,
  units = "px",
  dpi = 320
)





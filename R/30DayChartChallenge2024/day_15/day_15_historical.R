# 30DayChartChallenge2024 / day_15_historical

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
cfg_day_15 <- cfg$day_15


# Data --------------------------------------------------------------------
dt <- fread("R/30DayChartChallenge2024/day_15/fredgraph.csv")


# Plot --------------------------------------------------------------------
dt_sub <- dt[DATE >="1950-01-01" & DATE <="1984-12-01"]
dt_sub[, date_braek := fifelse(DATE <= "1970-12-01", "Yaers 1950-1970", "Years 1971-1984")]


plot <- dt_sub[, ggplot(.SD, aes(UNRATE, CPIAUCSL_PC1)) +
                 geom_point(aes(color = date_braek), size = 2.5, alpha = .5) +
                 scale_color_manual(values = c(c3_color_accent_1, c3_color_accent_2)) +
                 geom_smooth(
                   data = .SD[DATE <= "1970-12-01"],
                   method = "glm",
                   formula = y ~ log(x),
                   se = FALSE,
                   color = c3_color_text_2,
                   linewidth = 1.05,
                   show.legend = FALSE
                 ) +
                 scale_x_continuous(breaks = seq(0, 12, 2), limits = c(0,12)) +
                 scale_y_continuous(breaks = seq(-2.5, 15, 2.5), limits = c(-3,15)) +
                 labs(
                   title = "Fall of the Phillips Curve",
                   subtitle = "Unemployment and Inflation in the United States\nYears 1950-1984",
                   x = "Unemployment rate (%)",
                   y = "Inflation rate (%)",
                   color = NULL,
                   caption = caption_text(
                     source_text = cfg_day_15$source,
                     day_type =  cfg_day_15$category,
                     day_hashtag = cfg_day_15$theme,
                     day = cfg_day_15$day,
                     color_text_1 = c3_color_text_1,
                     color_text_2 = c3_color_text_2
                   )
                 ) +
                 theme_my(
                   font_regular     = "redhat_regular",
                   font_bold        = "redhat_bold",
                   font_light       = "redhat_light",
                   color_text_1     = c3_color_text_1,
                   color_text_2     = c3_color_text_2,
                   color_background = c3_color_background,
                   title_size       = 52
                 ) +
                 theme(
                   plot.margin = margin(20, 25, 10, 20, "pt"),
                   plot.caption =  element_textbox_simple(
                     size = 24,
                     lineheight = .5,
                     padding = margin(5, 5, 5, 5, "pt"),
                     margin = margin(10, 0, 0, 0, "pt"),
                   ),
                   legend.position = "bottom",
                   legend.spacing = unit(5, "pt"),
                   legend.key.spacing = unit(5, "pt"),
                   legend.key.height = unit(10, 'pt'),
                   legend.key.width = unit(16.18, 'pt'),
                   panel.grid.major = element_line(
                     colour = c3_color_text_2,
                     linetype = "solid",
                     linewidth = .3
                   ),
                   axis.text = element_text(
                     size = rel(1.8),
                     family = "redhat_regular",
                     color = c3_color_text_2,
                     margin = margin(2, 2, 2, 0, "pt")
                   )
                 )]


# Save --------------------------------------------------------------------
ggsave(
  filename = "R/30DayChartChallenge2024/day_15/day_15_historical.png",
  plot = plot,
  width = 1920,
  height = 1920,
  units = "px",
  dpi = 320
)

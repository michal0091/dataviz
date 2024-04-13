# 30DayChartChallenge2024 / day_13_family

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

# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_13 <- cfg$day_13


# Data --------------------------------------------------------------------
storks <- read.csv("R/30DayChartChallenge2024/day_13/storks.csv") %>% 
  as.data.table()


# Plot --------------------------------------------------------------------

plot <- storks[, ggplot(.SD, aes(x = Storks, y = Birth)) +
    geom_smooth(method = "lm", color = c3_color_accent_1, se = FALSE) +
  geom_point(color = c3_color_accent_2, size = 2) +
    scale_x_log10() +
    scale_y_log10() +
    # Add correlation label
    geom_text(
      aes(x = 20000, y = 220, label = paste0("Correlation: ", round(cor(Storks, Birth), 3))),
      family = "redhat_light",
      color = c3_color_text_1,
      size = rel(6),
      hjust = 1,
      vjust = 1
    ) +
  labs(
    title = "Storks Deliver Babies?",
    subtitle = "Correlation is not causation",
    x = "Number of storks breeding pairs",
    y = "Birth rate (thousands per year)",
    caption = caption_text(
      source_text = cfg_day_13$source,
      day_type =  cfg_day_13$category,
      day_hashtag = cfg_day_13$theme,
      day = cfg_day_13$day,
      color_text_1 = c3_color_text_2,
      color_text_2 = c3_color_text_1
    )
  ) +
    theme_my(
      font_regular     = "redhat_regular",
      font_bold        = "redhat_bold",
      font_light       = "redhat_light",
      color_text_1     = c3_color_text_2,
      color_text_2     = c3_color_text_1,
      color_background = c3_color_background,
      title_size       = 36 
    ) +
    theme(
      plot.margin = margin(25, 5, 5, 10, "pt"),
      plot.caption =  element_textbox_simple(
      size = 13,
      lineheight = .5,
      padding = margin(5, 5, 5, 5, "pt"),
      margin = margin(10, 0, 0, 0, "pt"),
    ))]


# Save --------------------------------------------------------------------
ggsave(
  filename = "day_13_family.png",
  path = normalizePath("R/30DayChartChallenge2024/day_13"),
  plot = plot,
  device = "png",
  units = "px",
  width = 1080,
  height = 1080,
  dpi = 320
)

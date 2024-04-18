# 30DayChartChallenge2024 / day_17_networks

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
library(geomnet)
library(ggnetwork)
library(igraph)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_17 <- cfg$day_17


# Data --------------------------------------------------------------------
blood_igraph <- graph_from_data_frame(d = blood$edges,  directed = TRUE)
data <-
  ggnetwork(blood.igraph,
            layout = layout_in_circle(blood_igraph),
            arrow.gap = 0.025)


# Plot --------------------------------------------------------------------

plot <- ggplot(data = data, aes(
  x = x,
  y = y,
  xend = xend,
  yend = yend
)) +
  geom_edges(
    linewidth = 0.5,
    curvature = 0.05,
    colour = c3_color_text_2,
    arrow = arrow(length = unit(2, "pt"), type = "closed", angle = 25)
  ) +
  geom_nodes(color = c3_color_accent_1,
             size = 4,
             show.legend = FALSE) +
  geom_nodetext(
    aes(label = name),
    color =  c3_color_text_1,
    family = "redhat_bold",
    fontface = "bold",
    show.legend = FALSE, 
    size = rel(7)
  ) +
  theme_net() +
  labs(
    title = "Blood donation diagram",
    x = NULL,
    y = NULL,
    color = NULL,
    fill = NULL,
    caption = caption_text(
      source_text = cfg_day_17$source,
      day_type =  cfg_day_17$category,
      day_hashtag = cfg_day_17$theme,
      day = cfg_day_17$day,
      color_text_1 = c3_color_text_2,
      color_text_2 = c3_color_text_1
    )
  ) +
  theme_my(
    font_regular     = "redhat_regular",
    font_bold        = "redhat_bold",
    font_light       = "redhat_light",
    color_text_1     = c3_color_text_1,
    color_text_2     = c3_color_text_2,
    color_background = c3_color_background,
    title_size       = 46
  ) +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank(),
        plot.caption =  element_textbox_simple(
          size = 13,
          lineheight = .5,
          padding = margin(1, 1, 1, 1, "pt"),
          margin = margin(25, 0, 0, 0, "pt"),
        ))
  


# Save --------------------------------------------------------------------
ggsave(
  filename = "R/30DayChartChallenge2024/day_17/day_17_networks.png",
  plot = plot,
  width = 1080,
  height = 1350,
  units = "px",
  dpi = 320
)


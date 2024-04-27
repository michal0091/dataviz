# 30DayChartChallenge2024 / day_26_AI

# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(ggforce)
library(ggtext)
library(ggrepel)
library(emojifont)
library(sysfonts)
library(showtext)
library(ggplot2)
library(tidyverse)
library(mlmhelpr)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_26 <- cfg$day_26


# Data --------------------------------------------------------------------
dt <- fread("R/30DayChartChallenge2024/day_26/ai.csv")

dt <- melt(dt, id.vars = c("Type", "Group"))



# Plot --------------------------------------------------------------------


plot <- ggplot(data = dt, aes(x = Group, y = value, fill = variable)) +
  
  scale_fill_manual(values = c("#00f1bc", "#81d8d0", "#ea899a")) +
  labs(
    title = "Awareness of AI use in UK",
    subtitle = "Proportion of adults reporting how often they think\nthey can recognise when they are using AI\nGreat Britain, 26 July to 1 October 2023",
    x = NULL,
    y = "Proportion (%)",
    fill = "Frequency",
    caption = caption_text(
      source = cfg_day_26$source,
      day_type = cfg_day_26$category,
      day_hashtag = cfg_day_26$theme,
      day = cfg_day_26$day,
      color_text_1 = c2_color_text_1,
      color_text_2 = c2_color_text_2
    )
  ) +
  
  # Stacked bar chart
  geom_bar(stat = "identity", position = position_fill()) +
  
  # Display data labels within bars (optional)
  geom_text(
    aes(label = paste0(value, "%")),
    stat = "identity",
    position = position_fill(vjust = .5),
    vjust = 0.5,
    size = rel(7)
  ) +
  
  # Avoid overlapping x-axis labels if needed (consider rotating or wrapping)
  coord_flip() + # Flips x and y axes for better readability
  facet_grid(Type ~ ., scales = "free", space = "free") +
  scale_y_continuous(labels = scales::percent_format()) +
  guides(fill = guide_legend(nrow = 3, theme = theme(legend.byrow = TRUE))) +
  theme_my(
    font_regular = "roboto_regular",
    font_bold = "roboto_bold",
    font_light = "roboto_light",
    color_text_1 = c2_color_text_1,
    color_text_2 = c2_color_text_2,
    color_background = c2_color_background,
    title_size = 48
  ) + 
  theme(
    legend.position = "bottom",
    axis.text = element_text(family = "roboto_regular",
                               size = 26, hjust = 1),
    plot.margin = margin(25, 45, 25, 25, "pt"),
    plot.title.position = "plot",
    plot.caption.position = "plot",
    plot.caption =  element_textbox_simple(
      size = 26,
      lineheight = .5,
      padding = margin(0, 0, 0, 0, "pt"),
      margin = margin(25, 0, 0, 0, "pt"),
    )
      
    
  )


# Save --------------------------------------------------------------------
ggsave(
  "R/30DayChartChallenge2024/day_26/day_26_AI.png",
  plot,
  width = 1920,
  height = 2560,
  units = "px",
  dpi = 320
)


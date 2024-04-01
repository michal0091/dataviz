# 30DayChartChallenge2024 / day_01_part_to_whole

# Libraries ---------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggtext)
library(emojifont)
library(sysfonts)
library(showtext)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")

# Config ------------------------------------------------------------------
cfg <- yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_1 <- cfg$day_1 

# Data --------------------------------------------------------------------
data <- data.table(
  dependency = c("Number of crops", "Crop production"),
  value = c(.75, .35),
  comment = c(
    "Three-quarters of crops get a little help from their buzzing buddies",
    "75% of crops need a pollinator's help, but only ⅓ of our food actually depends on them!\nWhy? Cereals and other heavy hitters don't need the buzz."
  )
)

data[, dependency := factor(dependency, levels = c("Number of crops", "Crop production"))]


# Plot --------------------------------------------------------------------
plot <- data %>%
  ggplot(aes(value, dependency)) +
  geom_col(fill = color_set_1, just = 1) +
  facet_wrap( ~ dependency, ncol = 1, scales = "free_y") +
  geom_text(
    aes(label = paste0("  ", sprintf("%2.0f", 100 * value), "%  "),
        hjust = 1.1, vjust = 3),
    color = color_background,
    size = rel(10),
    fontface = "bold",
    family = "inter_regular"
  ) +
  geom_text(
    aes(x = I(0), y = I(1.4), label = comment),
    color = color_text_1,
    size = rel(6),
    hjust = 0,
    family = "inter_light",
    lineheight = 0.4
  ) +
  
  scale_x_continuous(
    name = NULL,
    expand = c(0, 0),
    limits = c(0, 1),
    guide = "none"
  ) +
  scale_y_discrete(guide = "none") +
  scale_color_manual(values = c(color_text_1, color_background),
                     guide = "none") +
  labs(title = "Without a Buzz, No Brussels Sprouts: How Pollinators Keep Our Plates Full",
       subtitle = "Percentage of Food We Owe to Busy Bees (and Their Buddies)",
       x = NULL,
       y = NULL,
       caption = caption_text(
         source_text = cfg_day_1$source,
         day_type =  cfg_day_1$category,
         day_hashtag = cfg_day_1$theme,
         day = cfg_day_1$day
         )) +
  
  theme_my(title_size = 36) +
  theme(
    strip.background = element_rect(fill = color_background, color = color_background),
    strip.text = element_text(
      color = color_text_2,
      family = "inter_regular",
      hjust = 0,
      margin = margin(1, 1, 1, 1),
      face = "bold"
    ),
    panel.grid.major = element_blank(),
    panel.spacing = unit(1, "lines")
  )


# Save --------------------------------------------------------------------
ggsave(
  filename = "day_01_part_to_whole.png",
  path = normalizePath("R/30DayChartChallenge2024/day_01"),
  plot = plot,
  device = "png",
  units = "px",
  width = 1920,
  height = 1080,
  dpi = 320
)


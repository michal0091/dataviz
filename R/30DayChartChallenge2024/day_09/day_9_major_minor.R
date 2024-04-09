# 30DayChartChallenge2024 / day_9_major_minor

# Libraries ---------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggforce)
library(ggtext)
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
cfg_day_9 <- cfg$day_9


# Data --------------------------------------------------------------------
dt <- fread("R/30DayChartChallenge2024/day_09/symphonies_by_key.csv")
dt <- dt[, .N, .(note, key)] %>% dcast(note ~ key, value.var = "N") 

cols <- c("major", "minor")
dt <- dt[, (cols) := lapply(.SD, nafill, fill = 0), .SDcols = cols]
dt <- dt[order(major, minor, decreasing = TRUE)]
dt <- melt(dt, id.vars = "note", variable.name = "key", value.name = "count")
dt[, perc := round(count / sum(count) * 100, 1), by = note]
dt <- dt[order(key, perc)]
dt[, note := factor(note, levels = .SD[key == "major", note])]
dt[, my_hjust := fifelse(key == "major", 1.1, -0.1)]


# Plot --------------------------------------------------------------------

plot <- dt[, ggplot(.SD, aes(
  x = note,
  y = fifelse(key == "major", perc, -perc),
  fill = key
)) +
  geom_bar(stat = "identity") +
  geom_text(
    aes(
      label = paste0("  ", sprintf("%2.0f", perc), "%  "),
      hjust = my_hjust
    ),
    color = c2_color_text_1,
    size = rel(5),
    family = "roboto_bold"
  ) +
  scale_fill_manual(breaks = c("minor", "major"), 
                    values = c("major" = "#13ae8c", 
                               "minor" = c2_color_accent_2),
                    labels = c("Minor", "Major")) +
  labs(
    title = "Symphonies by Key",
    subtitle = "Major and Minor",
    x = NULL,
    y = NULL,
    fill = NULL,
    caption = caption_text(
      source_text = cfg_day_9$source,
      day_type =  cfg_day_9$category,
      day_hashtag = cfg_day_9$theme,
      day = cfg_day_9$day,
      color_text_1 = c2_color_text_1,
      color_text_2 = c2_color_text_2
    )
  ) +
  coord_flip() +
  scale_y_continuous(guide = "none") +
  theme_my(
    font_regular     = "roboto_regular",
    font_bold        = "roboto_bold",
    font_light       = "roboto_light",
    color_text_1     = c2_color_text_1,
    color_text_2     = c2_color_text_2,
    color_background = c2_color_background,
    title_size       = 28
  ) +
  theme(
    plot.margin = margin(25, 10, 0, 15, "pt"),
    legend.position = "bottom",
    legend.margin = margin(0, 0, 0, 0, "pt"),
    legend.spacing = unit(5, "pt"),
    legend.key.spacing = unit(5, "pt"),
    legend.key.height = unit(6, 'pt'),
    legend.key.width = unit(9.708, 'pt'),
    axis.text.y = element_text(
      size = 12,
      family = "roboto_bold",
      color = c2_color_text_2
    )
  )]

# Save --------------------------------------------------------------------
ggsave(
  filename = "day_9_major_minor.png",
  path = normalizePath("R/30DayChartChallenge2024/day_09"),
  plot = plot,
  device = "png",
  units = "px",
  width = 1080,
  height = 1080,
  dpi = 320
)

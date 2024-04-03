# 30DayChartChallenge2024 / day_3_makeover

# Libraries ---------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggforce)
library(ggtext)
library(emojifont)
library(sysfonts)
library(showtext)
library(ggpubr)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_3 <- cfg$day_3

# Data --------------------------------------------------------------------
# From chart
# Values less than 0.5 percent are indicated by an asterisk (*)
#'
#'Q11. To the best of your knowledge, will the overuse of antibiotics lead to
#' (INSERT ITEM), will the overuse of antibiotics not lead to this, or do
#' you not know enough to say? How about (INSERT NEXT ITEM)?
#' (IF NECESSARY: Will the overuse of antibiotics lead to (INSERT ITEM), will
#'  the overuse of antibiotics not lead to this, or do you not know enough to say?)
#'   (scramble items a-f)
#'   c. The spread of measles or other contagious viruses 26 39 35 *
#'

dt <- data.table(
  correct = c("Correct", "Incorrect", "Incorrect"),
  anwser = c("No", "Yes", "Don't know enough to say"),
  value = c(0.39, 0.26, 0.35)
)
dt[, label := paste0(correct, " ", sprintf("%2.0f", 100 * sum(value)), "%"), by = correct]

# Plot --------------------------------------------------------------------
color_text_2 <- "#2948FF"
title_text <- glue::glue(
  "<span style='color:{color_text_1}'>Most of the public
<span style='color:#cc222b;'>unaware</span>, antibiotic resistance<br>is
<span style='color:#537c78;'>unrelated</span> to the outbreak of viruses such as measles
</span>"
)


plot <- dt %>%
  ggplot(aes(value, anwser, fill = anwser)) +
  geom_col(just = 0.5, size = 1) +
  facet_grid(label ~ ., scales = "free", space = "free") +
  geom_text(
    aes(label = paste0("  ", sprintf("%2.0f", 100 * value), "%  "),
        hjust = -.1),
    vjust = "center",
    color = color_text_1,
    size = rel(7),
    fontface = "bold",
    family = "inter_regular"
  ) +
  geom_text(
    aes(x = 0.005,
        label = anwser),
    color = color_background,
    hjust = "left",
    vjust = "center",
    size = rel(9),
    fontface = "bold",
    family = "inter_regular"
  ) +
  scale_x_continuous(
    name = NULL,
    expand = c(0, 0),
    limits = c(0, 0.5),
    guide = "none"
  ) +
  scale_y_discrete(guide = "none") +
  scale_color_manual(values = c(color_text_1, color_background),
                     guide = "none") +
  scale_fill_manual(
    values = c(
      "No" = "#537c78",
      "Yes" = "#cc222b",
      "Don't know enough to say" = "#f15b4c"
    ),
    guide = "none"
  ) +
  labs(
    title = title_text,
    subtitle = "To the best of your knowledge, will the overuse of antibiotics\nlead to the spread of measles or other contagious viruses?",
    x = NULL,
    y = NULL,
    caption = glue::glue(
      "<span style='color:{color_text_1}'>* Refused less than 0.5%</span><br>",
      caption_text(
        source_text = cfg_day_3$source,
        day_type =  cfg_day_3$category,
        day_hashtag = cfg_day_3$theme,
        day = cfg_day_3$day,
        color_text_2 = color_text_2
      )
    )
  ) +
  theme_my(color_text_2 = color_text_2) +
  theme(
    strip.background = element_rect(fill = color_text_1, color = color_background),
    strip.text = element_text(
      color = color_background,
      family = "inter_regular",
      hjust = 0.5,
      margin = margin(.25, .25, .25, .25, "line"),
      face = "bold",
      angle = -90,
    ),
    panel.grid.major = element_blank(),
    panel.spacing = unit(0.5, "lines"),
    plot.title = element_markdown(
      size = 26,
      family = "inter_bold",
      face = "bold",
      hjust = 0,
      vjust = 4.5,
      lineheight = .34,
      margin = margin(.2, .2, 1, .2, "lines")
    ),
    plot.margin = margin(.5, .5, .5, .5, "lines"),
  )


# Save --------------------------------------------------------------------
ggsave(
  filename = "day_3_makeover.png",
  path = normalizePath("R/30DayChartChallenge2024/day_03"),
  plot = plot,
  device = "png",
  units = "px",
  width = 1080,
  height = 1080,
  dpi = 320
)



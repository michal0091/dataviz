# 30DayChartChallenge2024 / day_5_diverging

# Libraries ---------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggstats)
library(ggtext)
library(sysfonts)
library(showtext)

# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_5 <- cfg$day_5


# Data --------------------------------------------------------------------
dt <- fread("R/30DayChartChallenge2024/day_05/ideo.csv")
dt[, position := fcase(
  position %in% c("1 Izquierda", "2"),
  "Left",
  position %in% c("3", "4"),
  "Center-Left",
  position %in% c("5", "6"),
  "Center",
  position %in% c("7", "8"),
  "Center-Right",
  position %in% c("9", "10 Derecha"),
  "Right"
)]
dt[, position := factor(position,
                        levels = c("Right", "Center-Right", "Center", "Center-Left", "Left"))]

dt[, age := factor(age,
                   levels = c("18-24", "25-34", "35-44", "45-54", "55-64", "65-74", "75+"))]

dt_mw <-
  melt(dt[, .(age, position, m, w)], id.vars = c("position", "age"))
dt_mw <-
  dt_mw[, .(value = sum(value)), by = .(position, age, variable)]
dt_mw[, pct := value / sum(value), by = .(age, variable)]
dt_mw[, variable := factor(variable,
                           labels = c("Men", "Women"),
                           levels = c("m", "w"))]


# Plot --------------------------------------------------------------------
custom_label <- function(x) {
  p <- scales::percent(x, accuracy = 1)
  p[x < .075] <- ""
  p
}

colors <- c("#d93328", "#f05d5d", "#999999", "#5da7f0", "#3366cc")

plot <-
  dt_mw[, ggplot(.SD, aes(variable, pct, fill = reorder(position,-as.numeric(position)))) +
          geom_bar(stat = "identity", position = "likert") +
          geom_text(
            aes(
              label = paste0("  ", sprintf("%2.0f", 100 * pct), "%  "),
              hjust = fifelse(position == "Right", .65, 0.5),
            ),
            color = color_background,
            size = rel(5),
            family = "inter_regular",
            position = position_likert(vjust = 0.5)
          ) +
          scale_fill_manual(values = colors) +
          facet_wrap( ~ age, scales = "fixed", ncol = 1) +
          coord_flip() +
          scale_y_continuous(limits = c(-.70, .70), guide = "none") +
          labs(
            title = "Ideological self-positioning in Spain (2023)",
            subtitle = "Percentage by age group and sex",
            x = NULL,
            y = NULL,
            fill = NULL,
            caption = caption_text(
              source_text = cfg_day_5$source,
              day_type =  cfg_day_5$category,
              day_hashtag = cfg_day_5$theme,
              day = cfg_day_5$day
            )
          ) +
          theme_my() +
          theme(
            legend.position = "bottom",
            legend.spacing = unit(5, "pt"),
            legend.key.spacing = unit(5, "pt"),
            legend.key.height = unit(6, 'pt'),
            legend.key.width = unit(9.708, 'pt'),
            panel.grid.major = element_blank(),
            plot.margin = margin(25, 15, 15, 20, "pt")
          )]


# Save --------------------------------------------------------------------
ggsave(
  filename = "day_5_diverging.png",
  path = normalizePath("R/30DayChartChallenge2024/day_05"),
  plot = plot,
  device = "png",
  units = "px",
  width = 1080,
  height = 1747.44,
  dpi = 320
)

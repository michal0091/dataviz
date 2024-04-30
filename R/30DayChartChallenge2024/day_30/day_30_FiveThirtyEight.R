# 30DayChartChallenge2024 / day_30_FiveThirtyEight

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
cfg_day_30 <- cfg$day_30


# Colors ------------------------------------------------------------------
base <- "#ed713a"
black <- "#000000"
prim_palette <- c("#3ca7db", "#ff2700", "#77ab43", "#f6b900")
color_text_1 <- "#3c3c3c"
color_text_2 <- "#000000"
background <- "#F0F0F0"
grid <- "#cdcdcd"
grid_darker <- "#a0a0a0"


# Fonts -------------------------------------------------------------------
font_add_google(
  name = "Lato",
  family = "lato_bold",
  regular.wt = 700,
  bold.wt = 900
)

font_add_google(
  name = "Lato",
  family = "lato_regular",
  regular.wt = 400,
  bold.wt = 700
)
font_add_google(
  name = "Lato",
  family = "lato_light",
  regular.wt = 300,
  bold.wt = 400
)

showtext_auto()


# Data --------------------------------------------------------------------
president_polls <-
  fread("https://projects.fivethirtyeight.com/polls/data/president_polls.csv")

president_polls <- president_polls[state == ""]
president_polls[, `:=`(
  start_date = as.Date(start_date, format = "%m/%d/%y"),
  end_date   = as.Date(end_date, format = "%m/%d/%y")
)]
president_polls[, ref_date := start_date + (end_date - start_date) / 2]


# Plot --------------------------------------------------------------------
best_4_2024 <-
  president_polls[ref_date >= "2024-01-01", .N, .(candidate_name)][order(N)] %>%
  tail(4) %>%
  pull(candidate_name)
president_polls_4 <-
  president_polls[candidate_name %in% best_4_2024 &
                    ref_date >= "2023-10-01" &
                    transparency_score >= 5]
president_polls_4[, candidate_name := factor(
  candidate_name,
  levels = c("Joe Biden", "Donald Trump", "Robert F. Kennedy", "Cornel West"),
  labels = c("Biden", "Trump", "Kennedy", "West")
)]
president_polls_4[, pct := pct / 100]

candidate_label <-
  president_polls_4[ref_date >= "2024-03-01", .(mean_pct = round(mean(pct), 2)), candidate_name]
candidate_label[, ref_date := president_polls_4[, max(ref_date) + 3]]



# Change local for english date
Sys.setlocale("LC_TIME", "English")


plot <-
  president_polls_4[, ggplot(.SD,
                             aes(ref_date, pct, color = candidate_name,
                                 fill = candidate_name)) +
                      geom_smooth(
                        alpha = 0.33,
                        formula = y ~ s(x, bs = "ds", fx = T, k = 7),
                        stat = "smooth",
                        method = "gam",
                        se = TRUE,
                        level = 0.99,
                        linewidth = .75
                      ) +
                      geom_point(size = 1.5, alpha = .33) +
                      # Add label at the end with candidate name
                      geom_text(
                        data = candidate_label,
                        aes(
                          x = ref_date,
                          y = mean_pct,
                          label = candidate_name,
                          color = candidate_name
                        ),
                        family = "lato_bold",
                        hjust = 0,
                        nudge_y = c(0, .02, 0, 0),
                        size = rel(14),
                        inherit.aes = FALSE
                      ) +
                      scale_y_continuous(
                        labels = scales::percent_format(),
                        limits = c(0, .6),
                        breaks = seq(0, .6, .15)
                      ) +
                      scale_x_date(date_labels = "%b %y",
                                   date_breaks = "1 month",
                                   expand = c(0, 2)) +
                      coord_cartesian(clip = "off") +
                      scale_color_manual(values = prim_palette) +
                      scale_fill_manual(values = prim_palette) +
                      labs(
                        title = "2024 Presidential Election Polls",
                        subtitle = "Top 4 candidates",
                        x = NULL,
                        y = NULL,
                        caption = caption_text(
                          source_text  = cfg_day_30$source,
                          day_type     = cfg_day_30$category,
                          day_hashtag  = cfg_day_30$theme,
                          day          = cfg_day_30$day,
                          color_text_1 = base,
                          color_text_2 = background
                        )
                      ) +
                      theme(
                        panel.background = element_rect(fill = background),
                        panel.grid.major = element_line(color = grid),
                        panel.grid.minor = element_blank(),
                        axis.ticks = element_blank(),
                        axis.line.x = element_line(color = grid_darker, linewidth = .8),
                        axis.text = element_text(
                          family = "lato_regular",
                          color = color_text_1,
                          size = 32
                        ),
                        plot.background = element_rect(fill = background),
                        plot.margin = margin(25, 55, 5, 15, "pt"),
                        plot.title = element_text(
                          family = "lato_bold",
                          color = color_text_1,
                          size = 52
                        ),
                        plot.subtitle = element_text(
                          family = "lato_bold",
                          color = color_text_1,
                          size = 42
                        ),
                        plot.caption = element_textbox_simple(
                          size = 32,
                          lineheight = .5,
                          fill = "#5b5e5f",
                          padding = margin(10, 10, 10, 15, "pt"),
                          margin = margin(10,-55,-6,-15, "pt")
                        ),
                        plot.caption.position = "plot"
                      ) +
                      theme(legend.position = "none")]

# Save --------------------------------------------------------------------
ggsave(
  "R/30DayChartChallenge2024/day_30/day_30_FiveThirtyEight.png",
  plot,
  width = 1920,
  height = 1920,
  units = "px",
  dpi = 320
)

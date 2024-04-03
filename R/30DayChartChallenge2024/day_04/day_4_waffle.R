# 30DayChartChallenge2024 / day_4_waffle

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
cfg_day_4 <- cfg$day_4


# Data --------------------------------------------------------------------
#' Portal de datos abiertos del Ayuntamiento de Madrid 2022
#' Dogs 283935
#' Cats 126522
#' Habs 3286662

dt <- data.table(animal = c("Dog", "Cat"),
                 animal_per_1000_habs = c(86.39, 38.49))


# Prepare data
dt[, value := "yes"]
dt <-
  rbind(dt, dt[, .(animal_per_1000_habs = 100 - animal_per_1000_habs), animal],
        fill = TRUE)
dt[is.na(value), value := "no"]
dt[, valor_round := as.integer(round(round(
  animal_per_1000_habs / sum(animal_per_1000_habs), 2
) * 100)), animal]

# Fix normalized values
dt[, fix_valor_round := {
  dif_sum <- 100 - sum(valor_round)
  wm <- which.max(abs(animal_per_1000_habs - valor_round))
  values <- valor_round
  values[wm] <- values[wm] + dif_sum
  values
}, animal]


# Prepare data for waffle chart
waffle <- dt[, data.table(
  xvals = 1 - (0:99 %% 10),
  yvals =  0:99 %/% 10,
  fill = factor(rep(value , times = fix_valor_round))
),
animal]

waffle[, fill := factor(fill, levels = c("no", "yes"))]

plot_dog <-
  ggplot(waffle[animal == "Dog"], aes(xvals, yvals, color = fill)) +
  geom_richtext(
    label = "<span style='font-family:fa-solid;'>&#xf6d3;</span>",
    size = 9,
    label.colour = NA,
    fill = NA
  ) +
  coord_equal(expand = TRUE) +
  lims(x  = c(min(waffle$xvals) - 1, max(waffle$xvals) + 1),
       y  = c(min(waffle$yvals) - 1, max(waffle$yvals) + 1)) +
  scale_color_manual(values = c(medium_gray, "#987f50")) +
  labs(
    title = "Dogs per 1000 inhabitants",
    subtitle = "In Madrid there are more than\n86 doggy friends per 1000 inhabitants",
    color = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_my() +
  theme(
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0, "pt"),
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      size = 20,
      margin = margin(.1, .1, .1, 10, "pt"),
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 18,
      color = "#987f50",
      family = "inter_regular",
      margin = margin(10, .1, .1, 10, "pt"),
      lineheight = .3,
      hjust = 0
    )
    
  )


plot_cat <-
  ggplot(waffle[animal == "Cat"], aes(xvals, yvals, color = fill)) +
  geom_richtext(
    label = "<span style='font-family:fa-solid;'>&#xf6be;</span>",
    size = 9.5,
    label.colour = NA,
    fill = NA
  ) +
  coord_equal(expand = TRUE) +
  lims(x  = c(min(waffle$xvals) - 1, max(waffle$xvals) + 1),
       y  = c(min(waffle$yvals) - 1, max(waffle$yvals) + 1)) +
  scale_color_manual(values = c(medium_gray, "#474645")) +
  labs(
    title = "Cats per 1000 inhabitants",
    subtitle = "In Madrid there are more than\n38 cat friends per 1000 inhabitants",
    color = NULL,
    x = NULL,
    y = NULL
  ) +
  theme_my() +
  theme(
    legend.position = "none",
    plot.margin = margin(0, 0, 0, 0, "pt"),
    panel.grid.major = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.title = element_text(
      size = 20,
      margin = margin(.1, .1, .1, 10, "pt"),
      hjust = 0
    ),
    plot.subtitle = element_text(
      size = 18,
      color = "#474645",
      family = "inter_regular",
      margin = margin(10, .1, .1, 10, "pt"),
      lineheight = 0.3,
      hjust = 0
    )
    
  )



combined_plot <- plot_dog + plot_cat +
  plot_annotation(
    title = "Dog Days or Nine Lives? The Purrfect Pet for Madrileños",
    subtitle = "A Canine vs. Feline Face-Off: See who has more\nfurry fans per 1,000 inhabitants in Madrid (2022 data)",
    caption = caption_text(
      source_text = cfg_day_4$source,
      day_type =  cfg_day_4$category,
      day_hashtag = cfg_day_4$theme,
      day = cfg_day_4$day
    ),
    theme = theme_my() +
      theme(
        plot.margin = margin(35, 0, 0, 5, "pt"),
        plot.title = element_text(
          size = 24,
          family = "inter_bold",
          color = color_text_1,
          face = "bold",
          hjust = 0,
          vjust = 18,
          lineheight = .34,
          margin = margin(0, 0, 0, 10, "pt"),
        ),
        plot.subtitle = element_text(
          size = 22,
          family = "inter_bold",
          color = color_text_2,
          hjust = 0,
          vjust = 13,
          lineheight = .34,
          margin = margin(0, 0, 0, 10, "pt"),
        ),
        plot.caption =  element_textbox_simple(
          size = 14,
          lineheight = .5,
          padding = margin(5, 5, 5, 5, "pt"),
          margin = margin(0, 0, 0, 5, "pt"),
        )
      )
  )


# Save --------------------------------------------------------------------
ggsave(
  filename = "day_4_waffle.png",
  path = normalizePath("R/30DayChartChallenge2024/day_04"),
  plot = combined_plot,
  device = "png",
  units = "px",
  width = 1080,
  height = 1080,
  dpi = 320
)


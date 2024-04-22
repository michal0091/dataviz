# 30DayChartChallenge2024 / day_22_mobility

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
library(zoo)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_22 <- cfg$day_22


# Data --------------------------------------------------------------------
dt <- fread("R/30DayChartChallenge2024/day_22/rail_passengers.csv")


# Rail transport of passengers
# Millions of passenger



# Plot --------------------------------------------------------------------

plot <- dt[, ggplot(.SD, aes(year, rail_passengers)) +
                 geom_line(color = c4_color_set_2) +
                 geom_point(color = c4_color_set_2) +
                 labs(title = NULL,
                      x = NULL,
                      y = "Millions of passenger",
                      color = NULL) +
     scale_x_continuous(breaks = 2015:2022) +
     
                 # scale_y_continuous(labels = scales::label_number_auto) +
                 theme_my(
                   font_regular     = "josefin_regular",
                   font_bold        = "josefin_bold",
                   font_light       = "josefin_light",
                   color_text_1     = c4_color_text_1,
                   color_text_2     = c4_color_text_2,
                   color_background = c4_color_background,
                   title_size       = 50
                 ) +
                 theme(
                   legend.position = "bottom",
                   plot.margin = margin(0, 0, 0, 0, "pt"),
                   axis.text = element_text(
                     size = rel(2),
                     family = "josefin_bold",
                     color = c4_color_text_2,
                     margin = margin(2, 2, 2, 0, "pt")
                   ),
                   axis.title.y = element_text(
                     size = 32,
                     family = "josefin_bold",
                     color = c4_color_text_2,
                     margin = margin(5, 5, 5, 5, "pt"), 
                     angle = 90
                   ), 
                   panel.grid.major = element_line(
                     color = c4_color_text_2, 
                     linetype = "dotted",
                     linewidth = .3, 
                   ),
                   axis.title.y.right = element_text(
                     size = 32,
                     family = "josefin_bold",
                     color = c4_color_text_2,
                     margin = margin(5, 5, 5, 5, "pt"), 
                     angle = -90
                   )
                 )
]



# Titles theme 
tit_theme <- 
  theme_void() +
  theme(plot.background = element_rect(color=c4_color_background, fill=c4_color_background))

subtitle_text <- data.table(
  x = 0,
  y = 0,
  label = paste("Ridership peaked in 2019 at 413.9 million passengers before",
                "dropping due to COVID-19. In 2022, ridership rebounded to 393.4",
                "million, but it hasn't quite reached pre-pandemic levels."))

subtitle <-
  ggplot(subtitle_text, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = c4_color_background,
    box.padding = unit(c(0, 45, 0, 0), "pt"),
    fill = c4_color_background,
    family = "josefin_regular",
    box.r = unit(0, "pt"),
    width = unit(9.5, "cm"),
    size = rel(16),
    lineheight = .5,
    color = c4_color_text_2
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  tit_theme

logo_text <-logo_text <-logo_text <- data.table(
  x = 0,
  y = 0,
  label = "<span style='font-family:fa-solid;'>&#xf238;</span>")

logo <-
  ggplot(logo_text, aes(x = x, y = y)) +
  geom_richtext(aes(label = label),
                size = 100,
                label.colour = NA,
                fill = NA,
                hjust = 1,
                label.padding = unit(c(0, 0, 0, 0), "pt"),
                label.margin = unit(c(0, 0, 0, 0), "pt"),
                label.r = unit(0, "pt"),
                col = c4_color_text_2) +
  coord_cartesian(expand = FALSE, clip = "off") +
  tit_theme


combined_plot <- ((logo | subtitle) + plot_layout(widths = c(0.3819821, 0.618047))) / plot +
  plot_layout(heights = c(1, 1), guides = "collect") +
  plot_annotation(
    title = "Rail transport of passengers\nin European Union (2015-2022)",
    theme = theme_my(
      font_regular     = "josefin_regular",
      font_bold        = "josefin_bold",
      font_light       = "josefin_light",
      color_text_1     = c4_color_text_1,
      color_text_2     = c4_color_text_2,
      color_background = c4_color_background,
      title_size       = 68
    ) + 
      theme(
        plot.margin = margin(25, 10, 10, 10, "pt"),
        plot.caption =  element_textbox_simple(
              size = 32,
              lineheight = .5,
              padding = margin(4, 0, 4, 0, "pt"),
              margin = margin(25, 25, 0, 35, "pt"),
            ),
        plot.title = element_text(
          margin = margin(25, 5, 5, 25, "pt")
        )),
    caption = caption_text(
      source_text  = cfg_day_22$source,
      day_type     = cfg_day_22$category,
      day_hashtag  = cfg_day_22$theme,
      day          = cfg_day_22$day, 
      color_text_1 = c4_color_text_1, 
      color_text_2 = c4_color_text_2
    )
  )

# Save --------------------------------------------------------------------
ggsave(
  filename = "R/30DayChartChallenge2024/day_22/day_22_mobility.png",
  plot = combined_plot,
  width = 1920,
  height = 2688,
  units = "px",
  dpi = 320
)











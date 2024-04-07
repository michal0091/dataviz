# 30DayChartChallenge2024 / day_7_hazards

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
cfg_day_7 <- cfg$day_7


#'Métrica":"Hectáreas quemadas
#'Fuente":"Ministerio de Agricultura y Pesca, Alimentación y Medio Ambiente
#'Clasificación":" "Unidad":"Hectáreas afectadas
#'Escala":"Unidades


# Data --------------------------------------------------------------------
hbs <- read.csv("R/30DayChartChallenge2024/day_07/hectares_burned_spain.csv",
             dec = ",") %>% 
  as.data.table()


# Plot --------------------------------------------------------------------
plot <- hbs[, ggplot(.SD, aes(hectares_burned)) +
              geom_histogram(bins = round(sqrt(.N)), fill = c2_color_accent_2,
                             color = c2_color_accent_2) +
              scale_x_continuous(labels = scales::comma) +
              scale_y_continuous(breaks = seq(0, 30, 5), limits = c(0, 30)) +
              labs(
                title = "Wildfires in SpainHectares burned in Spain",
                subtitle = "Number of hectares burned",
                x = "ha burned",
                y = "Frequency",
                caption = caption_text(
                  source_text = cfg_day_7$source,
                  day_type =  cfg_day_7$category,
                  day_hashtag = cfg_day_7$theme,
                  day = cfg_day_7$day, 
                  color_text_1 = c2_color_text_1, 
                  color_text_2 = c2_color_text_2
                )
              ) +
              theme_my(font_regular     = "roboto_regular",
                       font_bold        = "roboto_bold",
                       font_light       = "roboto_light",
                       color_text_1     = c2_color_text_1,
                       color_text_2     = c2_color_text_2,
                       color_background = c2_color_background,
                       title_size       = 28) +
              theme(plot.margin = margin(25, 5, 5, 5, "pt"))
    ]


# Save --------------------------------------------------------------------
ggsave(
  filename = "day_7_hazards.png",
  path = normalizePath("R/30DayChartChallenge2024/day_07"),
  plot = plot,
  device = "png",
  units = "px",
  width = 1080,
  height = 1080,
  dpi = 320
)

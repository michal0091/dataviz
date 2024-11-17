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
library(sysfonts)
library(showtext)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")

# Font Awesome
font_add(family = "fa-brands", regular = "fonts/fa-brands-400.ttf")
showtext_auto()

# Functions ---------------------------------------------------------------
new_caption_text <- function(viz_author = "Michal Kinel",
                         source_text,
                         color_text_1 = "#352725",
                         color_text_2 = "#3f68e3",
                         github_icon = '&#xf113',
                         github_username = "michal0091",
                         twitter_icon = "&#xf081",
                         twitter_username = "nico_kinel",
                         linkedin_icon = "&#xf08c",
                         linkedin_username = "michal-kinel",
                         bluesky_icon = "&#xe671",
                         bluesky_username = "mikipe",
                         bluesky_server = "bsky.sociall") {
  
  social_caption <- glue::glue(
    "
  <span style='color: {color_text_2}'><strong>Data Visualization</strong>:   {viz_author}  </span>
  <span style='color: {color_text_1}'><strong>Source</strong>:   {source_text}</span><br>
  <span style='font-family:\"fa-brands\"; color: {color_text_2};'>{github_icon};</span>
  <span style='color: {color_text_1}'>{github_username} </span>
  <span style='font-family:\"fa-brands\"; color: {color_text_2}'>{twitter_icon};</span>
  <span style='color: {color_text_1}'>{twitter_username} </span>
  <span style='font-family:\"fa-brands\"; color: {color_text_2}'>{linkedin_icon};</span>
  <span style='color: {color_text_1}'>{linkedin_username} </span>
  <span style='font-family:\"fa-brands\"; color: {color_text_2}'>{bluesky_icon};</span>
  <span style='color: {color_text_1}'>{bluesky_username}.<span><span style='color: {color_text_1}'>{bluesky_server}</span>
  "
  )
  
  social_caption
  
}


# Data --------------------------------------------------------------------
dt <-
  fread("R/2024/week_47/forecast_11_2024.csv", dec = ",")

dt_nov <-
  melt(dt,
       id.vars = "previsiones",
       variable.name = "year",
       value.name = "forecast")
dt_nov <- dt_nov[year %in% c("pib_2024_11_2024", "pib_2025_11_2024")]
dt_nov[, year := fifelse(year == "pib_2025_11_2024", "GDP 2025 forecast", "GDP 2024 forecast")]
dt_nov[, forecast := forecast / 100]
dt_nov[, x_jit := jitter(as.numeric(as.factor(year)), factor = .5)]


# Get min forecast of each year
min_forecast <-
  dt_nov[, .(x_jit = x_jit[forecast == min(forecast)], forecast = min(forecast)), year]
min_forecast <- min_forecast[, first(.SD), year]
max_forecast <-
  dt_nov[, .(x_jit = x_jit[forecast == max(forecast)], forecast = max(forecast)), year]
max_forecast <- max_forecast[, first(.SD), year]

plot <- dt_nov[, ggplot(.SD, aes(year, forecast)) +
             geom_boxplot(
               width = .5,
               outliers = FALSE,
               staplewidth = 0.5,
               fill = NA,
               color = "#1f2937"
             ) +
             
             geom_point(aes(x_jit, forecast), color = "#4b5563") +
             geom_text_repel(
               data = min_forecast,
               aes(x_jit, forecast, label = paste0(
                 "Worst: ", scales::percent(forecast, accuracy = .01)
               )),
               family = "inter_bold",
               size = rel(12),
               direction = "y",
               color = "#030712",
               segment.size = 0.5,
               nudge_x = 0.5,
               nudge_y = c(-0.001, -0.0015),
               segment.curvature = 0.2,
               segment.color = "#1f2937"
             ) +
             geom_text_repel(
               data = max_forecast,
               aes(x_jit, forecast, label = paste0(
                 "Best: ", scales::percent(forecast, accuracy = .01)
               )),
               family = "inter_bold",
               size = rel(12),
               direction = "y",
               color = "#030712",
               segment.size = 0.5,
               nudge_x = 0.5,
               nudge_y = c(0.0015, 0.001),
               segment.curvature = 0.2,
               segment.color = "#1f2937"
             ) +
             
             scale_y_continuous(labels = scales::percent) +
             labs(
               title = "GDP Forecast 2024 and 2025",
               subtitle = "for Spanish economy\n(november 2024 update)",
               x = NULL,
               y = NULL,
               caption = new_caption_text(
                 source_text = "Funcas",
                 color_text_1 = "#030712",
                 color_text_2 = "#1f2937"
               )
             ) +
             theme_my(
               font_regular     = "inter_regular",
               font_bold        = "inter_bold",
               font_light       = "inter_light",
               color_text_1     = "#030712",
               color_text_2     = "#1f2937",
               color_background = "#d1d5db",
               title_size       = 48
             ) +
             theme(
               legend.position = "bottom",
               axis.text = element_text(
                 family = "roboto_regular",
                 size = 26,
                 hjust = .5
               ),
               plot.margin = margin(25, 45, 25, 25, "pt"),
               plot.title.position = "plot",
               plot.caption.position = "plot",
               plot.caption =  element_textbox_simple(
                 size = 26,
                 lineheight = .5,
                 padding = margin(0, 0, 0, 0, "pt"),
                 margin = margin(25, 0, 0, 0, "pt"),
               )
             )]


# Forecast revision

dt_fc_ch <- dt[, .(
  previsiones,
  pib_2024_04_11 = pib_2024_11_2024 - pib_2024_04_2024,
  pib_2025_04_11 = pib_2025_11_2024 - pib_2025_04_2024
)]

dt_fc_ch <-
  melt(dt_fc_ch,
       id.vars = "previsiones",
       variable.name = "year",
       value.name = "forecast_rev")
dt_fc_ch[, year := fifelse(year == "pib_2025_04_11", "GDP 2025 forecast revision", "GDP 2024 forecast revision")]
dt_fc_ch[, forecast_rev := forecast_rev / 100]
dt_fc_ch[, x_jit := jitter(as.numeric(as.factor(year)), factor = .5)]


# Get min revision of each year
min_fc_ch <-
  dt_fc_ch[, .(x_jit = x_jit[forecast_rev == min(forecast_rev)], forecast_rev = min(forecast_rev)), year]
min_fc_ch <- min_fc_ch[, first(.SD), year]
max_fc_ch <-
  dt_fc_ch[, .(x_jit = x_jit[forecast_rev == max(forecast_rev)], forecast_rev = max(forecast_rev)), year]
max_fc_ch <- max_fc_ch[, first(.SD), year]

plot_2 <- dt_fc_ch[, ggplot(.SD, aes(year, forecast_rev)) +
                 geom_boxplot(
                   width = .5,
                   outliers = FALSE,
                   staplewidth = 0.5,
                   fill = NA,
                   color = "#1f2937"
                 ) +
                 
                 geom_point(aes(x_jit, forecast_rev), color = "#4b5563") +
                 geom_text_repel(
                   data = min_fc_ch,
                   aes(x_jit, forecast_rev, label = paste0(
                     "Worst: ", scales::percent(forecast_rev, accuracy = .01, 
                                                prefix = fifelse(forecast_rev >= 0, "+", ""))
                   )),
                   family = "inter_bold",
                   size = rel(12),
                   direction = "y",
                   color = "#030712",
                   segment.size = 0.5,
                   nudge_x = 0.5,
                   nudge_y = c(-0.001, -0.0015),
                   segment.curvature = 0.2,
                   segment.color = "#1f2937"
                 ) +
                 geom_text_repel(
                   data = max_fc_ch,
                   aes(x_jit, forecast_rev, label = paste0(
                     "Best: ", scales::percent(forecast_rev, accuracy = .01,
                                               prefix = fifelse(forecast_rev >= 0, "+", ""))
                   )),
                   family = "inter_bold",
                   size = rel(12),
                   direction = "y",
                   color = "#030712",
                   segment.size = 0.5,
                   nudge_x = 0.5,
                   nudge_y = c(0.0015, 0.001),
                   segment.curvature = 0.2,
                   segment.color = "#1f2937"
                 ) +
                 
                 scale_y_continuous(labels = scales::percent) +
                 labs(
                   title = "GDP Forecast revision 2024 and 2025",
                   subtitle = "for Spanish economy\n(march/november 2024)",
                   x = NULL,
                   y = NULL,
                   caption = new_caption_text(
                     source_text = "Funcas",
                     color_text_1 = "#030712",
                     color_text_2 = "#1f2937"
                   )
                 ) +
                 theme_my(
                   font_regular     = "inter_regular",
                   font_bold        = "inter_bold",
                   font_light       = "inter_light",
                   color_text_1     = "#030712",
                   color_text_2     = "#1f2937",
                   color_background = "#d1d5db",
                   title_size       = 48
                 ) +
                 theme(
                   legend.position = "bottom",
                   axis.text = element_text(
                     family = "roboto_regular",
                     size = 26,
                     hjust = .5
                   ),
                   plot.margin = margin(25, 45, 25, 25, "pt"),
                   plot.title.position = "plot",
                   plot.caption.position = "plot",
                   plot.caption =  element_textbox_simple(
                     size = 26,
                     lineheight = .5,
                     padding = margin(0, 0, 0, 0, "pt"),
                     margin = margin(25, 0, 0, 0, "pt"),
                   )
                 )]


# Save --------------------------------------------------------------------
ggsave(
  "R/2024/week_47/2024_w47_boxplot_1.png",
  plot,
  width = 1920,
  height = 1920,
  units = "px",
  dpi = 320
)

ggsave(
  "R/2024/week_47/2024_w47_boxplot_2.png",
  plot_2,
  width = 1920,
  height = 1920,
  units = "px",
  dpi = 320
)

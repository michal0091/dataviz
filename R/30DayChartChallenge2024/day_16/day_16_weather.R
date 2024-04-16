# 30DayChartChallenge2024 / day_16_weather

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
library(keyring)
library(climaemet)
library(qs)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_16 <- cfg$day_16


# Data --------------------------------------------------------------------
# aemet_api_key(key_get("API_AEMET"), install = TRUE)

if (!file.exists("R/30DayChartChallenge2024/day_16/temp_data.qs")) {
  temp_data <- climaemet::aemet_daily_clim(station = "3195",
                                           start = as.Date("1923-01-01"),
                                           end = Sys.Date())
  temp_data <- as.data.table(temp_data)
  qsave(temp_data,
        "R/30DayChartChallenge2024/day_16/temp_data.qs")
} else {
  temp_data <- qread("R/30DayChartChallenge2024/day_16/temp_data.qs")
}

# Get yearly average
temp_data_yearly <-
  temp_data[, .(temp = mean(tmed, na.rm = T)), by = .(year = year(fecha))]


# Get daily average 1960-2022
temp_data_y <-
  temp_data[fecha <= "2023-01-01", .(
    tmed = mean(tmed, na.rm = T),
    tmin = min(tmin, na.rm = T),
    tmax = max(tmax, na.rm = T),
    se = sd(tmed, na.rm = T) / sqrt(length(tmed))
  ),
  by = .(month = month(fecha), day = day(fecha))]
# drop feb 29
temp_data_y <- temp_data_y[!(month == 2 & day == 29)]
temp_data_y[, `:=`(
  ci_upper = tmed + 2.101 * se,
  ci_lower = tmed - 2.101 * se,
  yday = 1:.N,
  data = "Historical daily average 1960-2022"
)]

# Get daily average 2023
temp_data_y2023 <-
  temp_data[fecha >= "2023-01-01" &
              fecha <= "2023-12-31", .(
                yday = 1:.N,
                day = day(fecha),
                month = month(fecha),
                tmed,
                tmin,
                tmax
              )]

temp_data_y2023[, se := sd(tmed, na.rm = T) /
                  sqrt(length(tmed)),
                by = .(month)]
temp_data_y2023[, `:=`(
  ci_upper = tmed + 2.101 * se,
  ci_lower = tmed - 2.101 * se,
  data = "2023"
)]

# Combine data
plot_data <- rbind(temp_data_y, temp_data_y2023)
plot_data[, max_data := tmax[2] > tmax[1], .(yday)]
plot_data[, min_data := tmin[2] < tmin[1], .(yday)]


# Make bottom labels
Sys.setlocale("LC_TIME", "English")
month <- temp_data[, .(tmed = round(mean(tmed, na.rm = T), 2)),
                   by = .(
                     month = month(fecha, label = TRUE, abbr = FALSE),
                     data = fifelse(year(fecha) < 2023, "Historical daily average 1960-2022", "2023")
                   )]
month[, yday := rep(temp_data_y2023[, round(mean(yday)), month]$V1, 2)]


# Plot --------------------------------------------------------------------

pal_strip <- hcl.colors(13, "RdBu")
plot_1 <- temp_data_yearly[, ggplot(.SD, aes(x = year, y = 1,
                                             fill = temp)) +
                             geom_tile() +
                             scale_x_continuous(
                               breaks = seq(min(year), max(year), 5),
                               expand = c(0, 0),
                               limits = c(min(year),
                                          max(year)),
                             ) +
                             scale_y_continuous(expand = c(0, 0), guide = "none") +
                             scale_fill_gradientn(colors = rev(pal_strip)) +
                             labs(
                               fill = "Temperature (°C)",
                               x = NULL,
                               y = NULL,
                               subtitle = "Warming stripes"
                             ) +
                             theme_my(
                               font_regular     = "redhat_regular",
                               font_bold        = "redhat_bold",
                               font_light       = "redhat_light",
                               color_text_1     = c3_color_text_1,
                               color_text_2     = c3_color_text_2,
                               color_background = c3_color_background,
                               title_size       = 52
                             ) +
                             theme(
                               plot.margin = margin(25, 30, 0, 30, "pt"),
                               legend.position = "bottom",
                               legend.spacing = unit(10, "pt"),
                               legend.key.spacing = unit(10, "pt"),
                               legend.key.height = unit(20, 'pt'),
                               legend.key.width = unit(20 * 1.618, 'pt'),
                               legend.title = element_text(hjust = 0),
                               panel.grid.major = element_blank(),
                               axis.text = element_text(
                                 size = rel(1.8),
                                 family = "redhat_regular",
                                 color = c3_color_text_2,
                                 margin = margin(2, 2, 2, 0, "pt")
                               )
                             )]

p <-
  plot_data[, ggplot(.SD, aes(yday, tmed, color = data, fill = data)) +
              geom_ribbon(
                aes(
                  y = tmed,
                  ymin = tmin,
                  ymax = tmax,
                  fill = "Historical daily min/max 1960-2022"
                ),
                alpha = .2,
                color = NA
              ) +
              geom_line() +
              geom_line(
                data = .SD[data == "2023"],
                aes(yday, tmax),
                linetype = "dashed",
                color = c3_color_set_2,
                show.legend = FALSE
              ) +
              geom_point(data = .SD[data == "2023" &
                                      max_data == TRUE],
                         aes(yday, tmax, color = "Daily maximum since 1960")) +
              geom_line(
                data = .SD[data == "2023"],
                aes(yday, tmin),
                linetype = "dashed",
                color = c3_color_set_3,
                show.legend = FALSE,
                size = 0.8
              ) +
              scale_x_continuous(
                limits = c(0, 365),
                breaks = c(1, plot_data[data == "2023", last(.SD), month]$yday),
                expand = c(0, 0)
              ) +
              scale_color_manual(
                values = c(
                  "2023" = c3_color_accent_1,
                  "Historical daily average 1960-2022" = c3_color_accent_2,
                  "Daily maximum since 1960" = "#FF4E4F"
                )
              ) +
              scale_fill_manual(values = c("Historical daily min/max 1960-2022" = c3_color_text_2)) +
              labs(
                title = NULL,
                subtitle = "Average daily temperature between 1960-2022 (°C)",
                x = NULL,
                y = NULL,
                color = NULL,
                fill = NULL,
                caption = caption_text(
                  source_text = cfg_day_16$source,
                  day_type =  cfg_day_16$category,
                  day_hashtag = cfg_day_16$theme,
                  day = cfg_day_16$day,
                  color_text_1 = c3_color_text_2,
                  color_text_2 = c3_color_text_1
                )
              ) +
              scale_y_continuous(
                limits = c(-15, 40),
                breaks = seq(0, 40, 5),
                labels = paste(seq(0, 40, 5), "°C")
              ) +
              guides(colour = guide_legend(nrow = 3)) +
              theme_my(
                font_regular     = "redhat_regular",
                font_bold        = "redhat_bold",
                font_light       = "redhat_light",
                color_text_1     = c3_color_text_1,
                color_text_2     = c3_color_text_2,
                color_background = c3_color_background,
                title_size       = 52
              ) +
              theme(
                plot.margin = margin(20, 35, 10, 35, "pt"),
                plot.caption =  element_textbox_simple(
                  size = 24,
                  lineheight = .5,
                  padding = margin(5, 25, 5, 25, "pt"),
                  margin = margin(10, 0, 0, 0, "pt"),
                ),
                legend.position = "bottom",
                legend.spacing = unit(5, "pt"),
                legend.key.spacing = unit(5, "pt"),
                legend.key.height = unit(10, 'pt'),
                legend.key.width = unit(16.18, 'pt'),
                legend.title = element_text(hjust = 0),
                panel.grid.major = element_line(
                  colour = c3_color_text_2,
                  linetype = "dashed",
                  linewidth = .3
                ),
                axis.text = element_text(
                  size = rel(1.8),
                  family = "redhat_regular",
                  color = c3_color_text_2,
                  margin = margin(2, 2, 2, 0, "pt")
                )
              )]


plot_2 <-  p +
  geom_text(
    data = month[data == "2023"],
    aes(
      x = yday,
      y = -10,
      label = formatC(tmed, format = "f", digits = 2)
    ),
    inherit.aes = F,
    family = "redhat_regular",
    color = c3_color_accent_1,
    size = rel(7)
  ) +
  geom_text(
    data = month[data == "Historical daily average 1960-2022"],
    aes(
      x = yday,
      y = -13,
      label = formatC(tmed, format = "f", digits = 2)
    ),
    inherit.aes = F,
    family = "redhat_regular",
    color = c3_color_accent_2,
    size = rel(7)
  ) +
  geom_text(
    data = month[data == "2023"],
    aes(
      x = yday,
      y = -15,
      label = as.character(month)
    ),
    inherit.aes = F,
    family = "redhat_regular",
    color = c3_color_text_2,
    size = rel(8)
  )



# Combine plots -----------------------------------------------------------
combied <- plot_1 + plot_2 +
  plot_layout(ncol = 1, heights = c(0.381982, 0.6180469)) +
  plot_annotation(
    title = "Madrid Weather 2023\nvs Historical Daily Average 1960-2022",
    subtitle = paste0(
      "Historical maximum daily temperatures were exceeded ",
      plot_data[data == "2023" & max_data == TRUE, .N],
      " times"
    ),
    theme = theme_my(
      font_regular     = "redhat_regular",
      font_bold        = "redhat_bold",
      font_light       = "redhat_light",
      color_text_1     = c3_color_text_1,
      color_text_2     = c3_color_text_2,
      color_background = c3_color_background,
      title_size       = 56
    ) +
      theme(plot.margin = margin(25, 0, 0, 0, "pt"))
  )


# Save --------------------------------------------------------------------
ggsave(
  filename = "R/30DayChartChallenge2024/day_16/day_16_weather.png",
  plot = combied,
  width = 2560,
  height = 2560,
  units = "px",
  dpi = 320
)


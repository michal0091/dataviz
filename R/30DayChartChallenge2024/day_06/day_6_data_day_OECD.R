# 30DayChartChallenge2024 / day_6_data_day_OECD

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
cfg_day_6 <- cfg$day_6


# Data --------------------------------------------------------------------
dt <- fread("R/30DayChartChallenge2024/day_06/OECD_AV_AN_WAGE.csv")

dt_sub <-
  dt[TIME_PERIOD %in% c(2020, 2022), .(REF_AREA, `Reference area`, TIME_PERIOD, OBS_VALUE)]

# Set hjust type
dt_sub[, my_hjust := fifelse(OBS_VALUE[2] - OBS_VALUE[1] >= 0, 1, 0), REF_AREA]
dt_sub[TIME_PERIOD == 2020, my_hjust := fifelse(my_hjust == 1, 0, 1)]
dt_sub[, nchar := nchar(OBS_VALUE) + 1]
dt_sub[, TIME_PERIOD := factor(TIME_PERIOD, levels = c(2020, 2022))]
dt_sub <-
  dt_sub[REF_AREA %in% dt_sub[, .N, REF_AREA][N > 1, REF_AREA]]


# Plot --------------------------------------------------------------------
color_positive <- "#2d950c"
color_negative <- "#dc1243"

# Add text
dt_sub[, diff := round(OBS_VALUE[2] - OBS_VALUE[1]), `Reference area`]
dt_sub[, diff_label := fcase(
  diff > 0,
  paste0("+ $ ", formatC(
    abs(diff), big.mark = ",", width = nchar(max(abs(diff))) + 1
  )),
  diff < 0,
  paste0("- $ ", formatC(
    abs(diff), big.mark = ",", width = nchar(max(abs(diff))) + 1
  )),
  diff == 0,
  paste0("- $ ", formatC(
    0, big.mark = ",", width = nchar(max(abs(diff))) + 1
  ))
)]


# Add color
dt_sub[, color := fcase(diff > 0,
                        color_positive,
                        diff < 0,
                        color_negative,
                        diff == 0,
                        base_text)]


plot <- dt_sub[,
               # Plot
               ggplot(.SD, aes(
                 x = reorder(`Reference area`, OBS_VALUE),
                 y = OBS_VALUE,
                 group = TIME_PERIOD
               )) +
                 
                 # Geoms
                 geom_line(
                   aes(group = `Reference area`),
                   color = color_set_3,
                   linewidth =
                     1.35,
                   alpha = .4
                 ) +
                 geom_point(aes(color = TIME_PERIOD), size = .85) +
                 
                 geom_text(
                   aes(
                     y = Inf,
                     label = diff_label,
                     family = "inter_regular",
                     vjust = .5,
                     hjust = "left"
                   ),
                   size = 3.5,
                   color = color
                 ) +
                 
                 # Scales
                 scale_color_manual(values = c(color_set_1, color_set_2)) +
                 scale_y_continuous(
                   limits = c(15000, 80000),
                   breaks = seq(15000, 80000, 5000),
                   labels = scales::dollar_format(seq(15000, 80000, 5000))
                 ) +
                 
                 # Flip coordinates
                 coord_flip(clip = 'off') +
                 
                 # Labels
                 labs(
                   title = "Average annual wage in OECD countries 2020 vs 2022",
                   subtitle = "Purchasing power parities (PPP) US dollars",
                   x = NULL,
                   y = NULL,
                   color = NULL,
                   caption = caption_text(
                     source_text = cfg_day_6$source,
                     day_type =  cfg_day_6$category,
                     day_hashtag = cfg_day_6$theme,
                     day = cfg_day_6$day
                   )
                 ) +
                 
                 # Theme
                 theme_my() +
                 theme(
                   legend.position = "bottom",
                   legend.spacing = unit(5, "pt"),
                   legend.key.spacing = unit(5, "pt"),
                   legend.key.height = unit(6, 'pt'),
                   legend.key.width = unit(9.708, 'pt'),
                   panel.grid.major.x = element_blank(),
                   panel.grid.major.y = element_line(
                     color = color_text_1, 
                     linewidth = .08, 
                     linetype = 5),
                   plot.margin = margin(35, 25, 10, 10, "pt"),
                   plot.title = element_text(
                     size = 24,
                     family = "inter_bold",
                     color = color_text_1,
                     face = "bold",
                     hjust = -.3,
                     vjust = 18,
                     lineheight = .34,
                     margin = margin(10, 0, 0, 0, "pt"),
                   ),
                   plot.subtitle = element_text(
                     size = 22,
                     family = "inter_bold",
                     color = color_text_2,
                     hjust = -.17,
                     vjust = 13,
                     lineheight = .34,
                     margin = margin(0, 0, 0, 0, "pt"),
                   ),
                   plot.caption =  element_textbox_simple(
                     size = 14,
                     hjust = 0,
                     halign = 0,
                     lineheight = .5,
                     padding = margin(5, 0, 5, 0, "pt"),
                     margin = margin(0, 0, 0, 0, "pt")
                   ),
                   axis.text.y = element_text(
                     size = 12,
                     family = "inter_bold",
                     color = color_text_1
                   )
                 )]


# Save plot ---------------------------------------------------------------
ggsave(
  filename = "day_6_data_day_OECD.png",
  path = normalizePath("R/30DayChartChallenge2024/day_06"),
  plot = plot,
  device = "png",
  units = "px",
  width = 1440,
  height = 1080,
  dpi = 320
)


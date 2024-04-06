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
                 
                 
                 # Scales
                 scale_color_manual(values = c(color_set_1, color_set_2)) +
                 scale_y_continuous(
                   limits = c(15000, 90000),
                   expand = c(0, 10),
                   breaks = seq(15000, 80000, 5000),
                   labels = scales::dollar_format(seq(15000, 80000, 5000))
                 ) +
                 
                 # Flip coordinates
                 coord_flip() +
                 
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
                   plot.margin = margin(35, 0, 0, 5, "pt"),
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

# Add text
dt_diff <-
  dt_sub[, .(diff = round(OBS_VALUE[2] - OBS_VALUE[1])), `Reference area`]
dt_diff[, diff_label := fcase(
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



# Convert to factor
dt_diff[, `Reference area` := factor(`Reference area`,
                                     levels = dt_sub[TIME_PERIOD == 2020][order(-OBS_VALUE)][, unique(`Reference area`)])]

# Add color
dt_diff[, color := fcase(diff > 0,
                         color_positive,
                         diff < 0,
                         color_negative,
                         diff == 0,
                         base_text)]

# Make plot box information with difference between 2020 and 2022
plot_box <- dt_diff[, ggplot(.SD) +
                      # Geoms
                      geom_text(
                        aes(
                          x = reorder(`Reference area`, -as.numeric(`Reference area`)),
                          y = 0,
                          label = diff_label,
                          family = "inter_regular",
                          vjust = 1
                        ),
                        size = 3.5,
                        color = color
                      ) +
                      scale_y_continuous(expand = c(1,-1)) +
                      geom_text(
                        aes(x = 36, y = 0),
                        # 18 is the length + 1 of the x axis
                        label = "dif.",
                        nudge_y = .0,
                        nudge_x = .8,
                        color = color_text_1,
                        family = "inter_regular",
                        size = 3.5,
                        vjust = "inward",
                        hjust = "inward"
                      ) +
                      scale_x_discrete(guide = "none") +
                      scale_y_continuous(guide = "none") +
                      # Flip coordinates
                      coord_flip() +
                      labs(x = NULL, y = NULL) +
                      theme_void() +
                      # Theme
                      theme_my() +
                      theme(
                        legend.position = "bottom",
                        panel.grid.major = element_blank(),
                        plot.margin = margin(0, 0, 0, 0, "pt")
                        
                      )]

# Make combined plot
plot_combined <-
  plot + annotation_custom(
    ggplotGrob(plot_box),
    xmin = 1,
    xmax = 37,
    ymin = 85000,
    ymax = 90000
  ) +
  plot_annotation(theme = theme_my() + theme(plot.margin = margin(0, 10, 5, 12.5, "pt")))


# Save plot ---------------------------------------------------------------
ggsave(
  filename = "day_6_data_day_OECD.png",
  path = normalizePath("R/30DayChartChallenge2024/day_06"),
  plot = plot_combined,
  device = "png",
  units = "px",
  width = 1440,
  height = 1080,
  dpi = 320
)


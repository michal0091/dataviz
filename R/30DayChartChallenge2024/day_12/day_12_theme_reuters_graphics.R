# 30DayChartChallenge2024 / day_12_theme_reuters_graphics

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
cfg_day_12 <- cfg$day_12


# Colors ------------------------------------------------------------------
primary_colors_gray <- c(
  "#f9fafb",
  "#f3f4f6",
  "#e5e7eb",
  "#d1d5db",
  "#9ca3af",
  "#6b7280",
  "#4b5563",
  "#374151",
  "#1f2937",
  "#111827",
  "#030712"
)

primary_colors_orange <- c(
  "#fff7ed",
  "#ffedd5",
  "#fed7aa",
  "#fdba74",
  "#fb923c",
  "#f97316",
  "#ea580c",
  "#c2410c",
  "#9a3412",
  "#7c2d12",
  "#431407"
)
thomson_reuters_colors <- c(
  "#fa6400",
  "#dc4300",
  "#ffa100",
  "#404040",
  "#666666",
  "#afafaf",
  "#d0d0d0",
  "#949494",
  "#f8f8f8",
  "#f4f4f4",
  "#f4f4f4",
  "#005da2",
  "#0099c4",
  "#4386b9",
  "#7facce",
  "#e5eef5",
  "#621f95",
  "#6e3ab7",
  "#a00000",
  "#dc0a0a",
  "#387c2b",
  "#77a22d",
  "#000000",
  "#ffffff",
  "#ffb1b1"
)


# Fonts -------------------------------------------------------------------
font_add_google(
  name = "Source Sans Pro",
  family = "source_sans_h1_2",
  regular.wt = 700,
  bold.wt = 900
)
font_add_google(
  name = "Source Sans Pro",
  family = "source_sans_h3_4",
  regular.wt = 600,
  bold.wt = 800
)
font_add_google(
  name = "Source Sans Pro",
  family = "source_sans_text",
  regular.wt = 400,
  bold.wt = 600
)
font_add_google(
  name = "Source Sans Pro",
  family = "source_sans_sub_cap",
  regular.wt = 300,
  bold.wt = 400
)

showtext_auto()


# Data --------------------------------------------------------------------
dt <-
  fread("R/30DayChartChallenge2024/day_12/democracy-index-eiu.csv")

idx_2022 <- dt[Year == 2022 & !(Code %in% c("", "OWID_WRL"))]

rusukr <- dt[Entity %in% c("Russia", "Ukraine")]
idx_2022 <- idx_2022[order(-democracy_eiu)]
idx_2022[, y := {
  updw <- floor((.N - 1) / 2)
  
  if (updw == 0) {
    if (.N == 1) {
      1
    } else {
      c(1, 1.1)
    }
  } else {
    up <- .N - 1 - updw
    c(1 -  cumsum(rep(0.11, updw)), 1, 1 + cumsum(rep(0.11, up)))
  }
  
}, .(round(democracy_eiu * 4) / 4)]

# Plot --------------------------------------------------------------------

# idx_2022 jitter dot plot
plot_1 <- idx_2022[, ggplot(.SD,
                            aes(x = democracy_eiu, y = y)) +
                     geom_jitter(
                       size = 3,
                       color = fcase(
                         Code == "RUS",
                         thomson_reuters_colors[1],
                         Code == "UKR",
                         thomson_reuters_colors[21],
                         default = primary_colors_gray[4]
                       )
                     ) +
                     # add text for Russia and Ukraine
                     geom_text(
                       data = rusukr[Year == 2022],
                       aes(
                         x = democracy_eiu,
                         y = c(1.65, 1.55),
                         label = Entity
                       ),
                       hjust = 0.5,
                       family = "source_sans_h3_4",
                       size = 13,
                       color = primary_colors_gray[11]
                     ) +
                     # Russia and Ukraine lines
                     geom_curve(
                       aes(
                         x = .SD[Code == "RUS", democracy_eiu],
                         y = .SD[Code == "RUS", y + .05],
                         xend = .SD[Code == "RUS", democracy_eiu],
                         yend = 1.55
                       ),
                       curvature = -0.2,
                       linewidth = .75,
                       color = primary_colors_gray[11]
                     ) +
                     geom_curve(
                       aes(
                         x = .SD[Code == "UKR", democracy_eiu],
                         y = .SD[Code == "UKR", y + .05],
                         xend = .SD[Code == "UKR", democracy_eiu],
                         yend = 1.45
                       ),
                       curvature = 0.2,
                       linewidth = .75,
                       color = primary_colors_gray[11]
                     ) +
                     geom_text_repel(
                       data = .SD[Entity %in% c("Norway", "Spain", "Afghanistan", "North Korea")],
                       aes(x = democracy_eiu, y = y, label = Entity),
                       family = "source_sans_text",
                       size = 13,
                       color = primary_colors_gray[6],
                       direction = "y",
                       nudge_y = c(.35, -.2, .3, -.25),
                       nudge_x = c(0, .5, 0,-0.1),
                       segment.size = 0.5,
                       segment.color = primary_colors_gray[6]
                     ) +
                     
                     # add tex under 0 and 10 as (worst) and (best)
                     geom_text(
                       aes(
                         x = I(0.01),
                         y = I(-0.3),
                         label = "(worst)"
                       ),
                       hjust = 0.5,
                       family = "source_sans_sub_cap",
                       size = 13,
                       color = primary_colors_gray[8],
                       inherit.aes = FALSE
                     ) +
                     geom_text(
                       aes(
                         x = I(0.99),
                         y = I(-0.3),
                         label = "(best)"
                       ),
                       hjust = 0.5,
                       family = "source_sans_sub_cap",
                       size = 13,
                       color = primary_colors_gray[8]
                     ) +
                     coord_cartesian(clip = "off") +
                     scale_x_continuous(
                       limits = c(0, 10),
                       breaks = seq(0, 10, 1),
                       expand = c(0.01, 0.01)
                     ) +
                     scale_y_continuous(guide = "none") +
                     labs(title = "DEMOCRACY INDEX OF 165 INDEPENDENT STATES AND TWO TERRITORIES IN 2022") +
                     theme_void() +
                     theme(
                       plot.margin = margin(25, 20, 45, 20, "pt"),
                       axis.ticks.x = element_line(
                         color = primary_colors_gray[8],
                         linewidth = .7,
                         lineend = "square"
                       ),
                       axis.ticks.length.x = unit(6, "pt"),
                       axis.text.x = element_text(
                         color = primary_colors_gray[8],
                         size = 38,
                         vjust = 0,
                         family = "source_sans_text"
                       ),
                       plot.title = element_markdown(
                         hjust = -1.6,
                         vjust = -2,
                         color = "black",
                         family = "source_sans_h1_2",
                         margin = margin(5, 0, 30, 0, "pt"),
                         size = 48
                       )
                       
                     )]


# line plot
plot_2 <-
  rusukr[, ggplot(.SD, aes(Year, democracy_eiu, color = Entity)) +
           geom_line(linewidth = 1.05) +
           scale_color_manual(values = thomson_reuters_colors[c(1, 21)]) +
           scale_x_continuous(
             breaks = seq(2006, 2022, 2),
             limits = c(2006, 2022),
             expand = c(.01, .01)
             
           ) +
           scale_y_continuous(
             breaks = seq(0, 10, 2),
             labels = formatC(
               c(as.character(seq(0, 8, 2)), "10 (best)"),
               width = 9,
               flag = " "
             ),
             limits = c(0, 10),
             expand = c(0, 0)
           ) +
           
           # Add Countries labels
           geom_text(
             data = .SD[Year == 2019, .(Year,
                                        y = fifelse(Entity == "Russia", democracy_eiu - 0.5,
                                                    democracy_eiu + 0.5),
                                        Entity)],
             aes(x = Year, y = y, label = Entity),
             hjust = c(1, 0),
             family = "source_sans_h1_2",
             size = 13,
             color = primary_colors_gray[11],
             nudge_x = 0.1
           ) +
           labs(
             title = "DEMOCRACY INDEX OF RUSSIA AND UKRAINE OVER TIME",
             x = NULL,
             y = NULL,
             color = NULL
           ) +
           theme_void() +
           theme(
             plot.margin = margin(10, 10, 40, 0, "pt"),
             legend.position = "none",
             axis.ticks.x = element_line(
               color = primary_colors_gray[8],
               linewidth = .7,
               lineend = "square"
             ),
             axis.ticks.length.x = unit(6, "pt"),
             axis.text.x = element_text(
               color = primary_colors_gray[8],
               size = 38,
               vjust = 0,
               family = "source_sans_text"
             ),
             axis.text.y = element_text(
               color = primary_colors_gray[8],
               size = 38,
               hjust = .5,
               vjust = .5,
               family = "source_sans_text",
             ),
             axis.line.x = element_line(color = primary_colors_gray[8],
                                        linewidth = .7),
             panel.grid.major.y = element_line(
               color = primary_colors_gray[4],
               linetype = "solid",
               linewidth = .7
             ),
             plot.title = element_markdown(
               hjust = -.18,
               vjust = -2,
               color = "black",
               family = "source_sans_h1_2",
               margin = margin(5, 0, 30, 0, "pt"),
               size = 48
             )
           )]


# Combine plots -----------------------------------------------------------
comnbined <-
  plot_1 + plot_2 + plot_layout(ncol = 1, heights = c(1 / 3, 2 / 3)) +
  plot_annotation(
    title = "Democracy Index of Russia and Ukraine",
    subtitle = paste(
      "The Economist Intelligence Unit's (EIU) Democracy Index 2023 provides a comprehensive",
      "assessment of democratic health across the globe. This data visualization utilizes",
      "the EIU's expert estimations to capture the multifaceted nature of democracy.",
      "It incorporates factors such as electoral processes, civil liberties, political participation,",
      "and the effectiveness of government. Scores range from 0 (authoritarian) to 10 (full democracy),",
      "enabling cross-country comparisons.",
      sep = "\n"
    ),
    caption = caption_text(
      source_text = cfg_day_12$source,
      day_type =  cfg_day_12$category,
      day_hashtag = cfg_day_12$theme,
      day = cfg_day_12$day,
      color_text_1 = primary_colors_gray[11],
      color_text_2 = primary_colors_gray[8],
    ),
    theme = theme_void() %+replace%
      theme(
        plot.margin = margin(35, 10, 0, 0, "pt"),
        plot.title = element_text(
          size = 90,
          family = "source_sans_h1_2",
          color = "black",
          face = "bold",
          hjust = 0,
          vjust = 4.5,
          lineheight = .34,
          margin =  margin(5, 5, 5, 5, "pt")
        ),
        plot.subtitle = element_text(
          size = 48,
          family = "source_sans_text",
          color = "black",
          hjust = 0,
          vjust = 3,
          lineheight = .3,
          margin =  margin(5, 5, 5, 5, "pt")
        ),
        plot.caption =  element_textbox_simple(
          size = 38,
          lineheight = .5,
          padding = margin(5, 5, 5, 5, "pt"),
          margin = margin(5, 5, 5, 5, "pt"),
          family = "source_sans_text"
        )
      )
  )

# Save plot ---------------------------------------------------------------
ggsave(
  filename = "R/30DayChartChallenge2024/day_12/day_12_theme_reuters_graphics.png",
  plot = comnbined,
  width = 2784,
  height = 3244,
  units = "px",
  bg = "white",
  dpi = 320
)


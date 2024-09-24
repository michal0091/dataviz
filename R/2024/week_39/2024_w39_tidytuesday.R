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
library(tidytuesdayR)

# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


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
tuesdata <- tidytuesdayR::tt_load(2024, week = 39)
country <- tuesdata$country_results_df %>% as.data.table()
country[, mean_score := mean(c(p1, p2, p3, p4, p5, p6), na.rm = TRUE), by = .(year, country)]
country[, mean_score_scale := 10 * mean_score / 42]

# change some countries names
country[, country := fcase(
  country == "People's Republic of China", "PRC",
  country == "United States of America", "USA",
  country == "United Kingdom", "UK",
  default = country
)]

selected_countries <- c(
  "Poland",
  "Germany",
  "USA",
  "UK",
  "France",
  "Spain",
  "PRC",
  "Japan",
  "United Arab Emirates"
)

score_2024 <- country[year == 2024]

ts_selected <- country[country %in% selected_countries & year > 1990]
score_2024 <- score_2024[order(-mean_score)]
score_2024[, y := {
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
  
}, .(round(mean_score_scale * 4) / 4)]



# Plot --------------------------------------------------------------------

plot_1 <- score_2024[, ggplot(.SD,
                            aes(x = mean_score_scale, y = y)) +
                     geom_jitter(
                       size = 3,
                       color = fcase(
                         country == "Poland",
                         thomson_reuters_colors[20],
                         country == "Spain",
                         thomson_reuters_colors[3],
                         default = primary_colors_gray[4]
                       )
                     ) +
                     # add text for Poland and Spain
                     geom_text(
                       data = ts_selected[year == 2024 & country %in% c("Poland", "Spain")],
                       aes(
                         x = mean_score_scale,
                         y = c(1.65, 1.55),
                         label = country
                       ),
                       hjust = 0.6,
                       family = "source_sans_h3_4",
                       size = 13,
                       color = primary_colors_gray[11]
                     ) +
                     # Poland and Spain lines
                     geom_curve(
                       aes(
                         x =    .SD[country == "Poland", mean_score_scale],
                         y =    .SD[country == "Poland", y + .05],
                         xend = .SD[country == "Poland", mean_score_scale],
                         yend = 1.55
                       ),
                       curvature = -0.2,
                       linewidth = .75,
                       color = primary_colors_gray[11]
                     ) +
                     geom_curve(
                       aes(
                         x =    .SD[country == "Spain", mean_score_scale],
                         y =    .SD[country == "Spain", y + .05],
                         xend = .SD[country == "Spain", mean_score_scale],
                         yend = 1.45
                       ),
                       curvature = 0.2,
                       linewidth = .75,
                       color = primary_colors_gray[11]
                     ) +
                     geom_text_repel(
                       data = .SD[country %in% selected_countries[selected_countries %notin% c("Poland", "Spain")],
                                  .(mean_score_scale, y, country)],
                       aes(x = mean_score_scale, y = y, label = country),
                       family = "source_sans_text",
                       size = 13,
                       color = primary_colors_gray[6],
                       direction = "y",
                       nudge_y = c(0.20, -0.15, 0.15,  0.20, -0.32, 0.35, 0.32),
                       nudge_x = c(0.10, -0.10, 0.00, -0.08,  0.10, 0.18, 0.1),
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
                     labs(title = "MEAN SCALED SCORE ON IMO PROBLEMS FOR 108 COUNTRIES IN 2024") +
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
                         hjust = -0.18,
                         vjust = -2,
                         color = "black",
                         family = "source_sans_h1_2",
                         margin = margin(5, 0, 30, 0, "pt"),
                         size = 48
                       )
                       
                     )]

# line plot
plot_2 <-
  ts_selected[country %in% c("Poland", "Spain"), 
              ggplot(.SD, aes(year, mean_score_scale, color = country)) +
           geom_line(linewidth = 1.05) +
           scale_color_manual(values = thomson_reuters_colors[c(20, 3)]) +
           scale_x_continuous(
             breaks = seq(1991, 2024, 3),
             limits = c(1991, 2024),
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
             data = .SD[year == 2021, .(year,
                                        y = fifelse(country == "Poland", mean_score_scale + 2.5,
                                                    mean_score_scale - 0.5),
                                        country)],
             aes(x = year, y = y, label = country),
             hjust = c(1, 0),
             family = "source_sans_h1_2",
             size = 13,
             color = primary_colors_gray[11],
             nudge_x = 0.1
           ) +
           labs(
             title = "MEAN SCALED SCORE ON IMO PROBLEMS FOR POLAND AND SPAIN OVER TIME",
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
caption_text <- function(viz_author = "Michal Kinel",
                         source_text,
                         color_text_1 = "#352725",
                         color_text_2 = "#3f68e3",
                         github_icon = '&#xf113',
                         github_username = "michal0091",
                         twitter_icon = "&#xf081",
                         twitter_username = "nico_kinel",
                         linkedin_icon = "&#xf08c",
                         linkedin_username = "michal-kinel",
                         mastodon_icon = "&#xf4f6",
                         mastodon_username = "miki_peltzer",
                         mastodon_server = "techhub.social") {
  
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
  <span style='font-family:\"fa-brands\"; color: {color_text_2}'>{mastodon_icon};</span>
  <span style='color: {color_text_1}'>{mastodon_username}@<span><span style='color: {color_text_1}'>{mastodon_server}</span>
  "
  )
  
  social_caption
  
}

comnbined <-
  plot_1 + plot_2 + plot_layout(ncol = 1, heights = c(1 / 3, 2 / 3)) +
  plot_annotation(
    title = "IMO scaled mean scores for Poland and Spain",
    subtitle = paste(
      "The International Mathematical Olympiad (IMO) is the World Championship Mathematics Competition",
      "for High School students and is held annually in a different country.",
      "The first IMO was held in 1959 in Romania, with 7 countries participating.",
      "It has gradually expanded to over 100 countries from 5 continents.",
      "The competition consists of 6 problems and is held over two consecutive days with 3 problems each.",
      sep = "\n"
    ),
    caption = caption_text(
      color_text_1 = primary_colors_gray[11],
      color_text_2 = primary_colors_gray[8],
      source_text = "Tidy Tuesday 2024 Week 39 | IMO Data",
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
          size = 42,
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
  filename = "R/2024/week_39/2024_w39_tidytuesday.png",
  plot = comnbined,
  width = 2784,
  height = 3244,
  units = "px",
  bg = "white",
  dpi = 320
)



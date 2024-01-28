# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-01-28
#
# Script Name: 2024_w4_scatter_smooth.R
#
# Script Description: The ideological gap between men and women in Spain
#
#
# Notes: 
#
#

# Set options -------------------------------------------------------------
cat("Setting options... \n\n", sep = "")
options(scipen = 999) # turns off scientific notation
options(encoding = "UTF-8") # sets string encoding to UTF-8 instead of ANSI


# Install packages & load libraries ---------------------------------------
cat("Install packages & load libraries... \n\n", sep = "")
packages <- c("tidyverse", "data.table", "zoo", "extrafont", "patchwork") # list of packages to load
n_packages <- length(packages) # count how many packages are required

new_pkg <- packages[!(packages %in% installed.packages())] # determine which packages aren't installed

# Install missing packages
if(length(new_pkg)){
  install.packages(new_pkg)
}

# Load all requried libraries
for(n in 1:n_packages){
  cat("Loading Library #", n, " of ", n_packages, "... Currently Loading: ", packages[n], "\n", sep = "")
  lib_load <- paste("library(\"",packages[n],"\")", sep = "") # create string of text for loading each library
  eval(parse(text = lib_load)) # evaluate the string to load the library
}


# Load data ---------------------------------------------------------------
cat("Load data... \n\n", sep = "")

ideology <- fread("R/2024/week_04/ideological_perception_by_sex_es.csv", dec = ",")
ideology_18_34 <- fread("R/2024/week_04/ideological_perception_by_sex_18_34y_es.csv")


# Manage data -------------------------------------------------------------
cat("Manage data... \n\n", sep = "")

### All age groups

# Rescale 1-10 to 1-5
ideology[, scale_aux := fcase(
  scale %in% c("1", "2"), "1-2",
  scale %in% c("3", "4"), "3-4",
  scale %in% c("5", "6"), "5-6",
  scale %in% c("7", "8"), "7-8",
  scale %in% c("9", "10"), "9-10"
)]
ideology[, scale_aux := fifelse(is.na(scale_aux), scale, scale_aux)]
ideology_rs <- ideology[, .(m = sum(m),
                            w = sum(w),
                            all = sum(all)), by = .(year, scale = scale_aux)]

# Drop NR & DK
ideology_rs <- ideology_rs[!scale %in% c("NR", "DK")]

# Factorize scale
ideology_rs[, scale := factor(scale, levels = c("1-2", "3-4", "5-6", "7-8", "9-10"))]

# Get left vs right
summary_id <- ideology_rs[, .(m_position = sum(m[scale %in% c("1-2", "3-4")]) - sum(m[scale %in% c("7-8", "9-10")]),
                              w_position = sum(w[scale %in% c("1-2", "3-4")]) - sum(w[scale %in% c("7-8", "9-10")])),
                          .(year)]

# Add loess
summary_id[, `:=`(
  m_position_sm = loess(m_position ~ year, span = 0.5)$fitted,
  w_position_sm = loess(w_position ~ year, span = 0.5)$fitted)]


### 18-34 age group
ideology_18_34[, scale_aux := fcase(
  scale %in% c("1", "2"), "1-2",
  scale %in% c("3", "4"), "3-4",
  scale %in% c("5", "6"), "5-6",
  scale %in% c("7", "8"), "7-8",
  scale %in% c("9", "10"), "9-10"
)]
ideology_18_34[, scale_aux := fifelse(is.na(scale_aux), scale, scale_aux)]
ideology_18_34_rs <- ideology_18_34[, .(m   = as.numeric(sum(m)),
                                        w   = as.numeric(sum(w)),
                                        all = as.numeric(sum(all))), by = .(year, scale = scale_aux)]
vars <- c("m", "w", "all")
ideology_18_34_rs[, (vars) := lapply(.SD, function(x) round(100 * x / sum(x), 1)), .SDcols = vars, year]

# Drop NR & DK
ideology_18_34_rs <- ideology_18_34_rs[!scale %in% c("NR", "DK")]

# Factorize scale
ideology_18_34_rs[, scale := factor(scale, levels = c("1-2", "3-4", "5-6", "7-8", "9-10"))]

# Get left vs right
summary_18_34_id <- ideology_18_34_rs[, .(m_position = sum(m[scale %in% c("1-2", "3-4")]) - sum(m[scale %in% c("7-8", "9-10")]),
                                          w_position = sum(w[scale %in% c("1-2", "3-4")]) - sum(w[scale %in% c("7-8", "9-10")])),
                                      .(year )]
# Add loess
summary_18_34_id[, `:=`(
  m_position_sm = loess(m_position ~ year, span = 0.75)$fitted,
  w_position_sm = loess(w_position ~ year, span = 0.75)$fitted)]


# Plot data ---------------------------------------------------------------
cat("Plot data... \n\n", sep = "")

# Style
# Color palette
background = "#F2E4DC"
color_m <- "#41BDD9"
color_w <- "#BF3641"
color_gap <- "#F2BBC9"
color_text_base <- "#37302A"
color_text_medium <- "#625D59"

# Load fonts
loadfonts(device = "win")

# Fonts
font_base <- "Noto Sans"

### All age groups plot
p_1 <- summary_id[, ggplot(.SD) +
                    # Man
                    geom_point(
                      aes(x = year, y = m_position),
                      size = 2,
                      alpha = 0.5,
                      color = color_m
                    ) +
                    geom_path(aes(x = year, y = m_position_sm),
                              size = 1.2,
                              color = color_m) +
                    # Woman
                    geom_point(
                      aes(x = year, y = w_position),
                      size = 2,
                      alpha = 0.5,
                      color = color_w
                    ) +
                    geom_path(aes(x = year, y = w_position_sm),
                              size = 1.2,
                              color = color_w) +
                    # Gap
                    geom_ribbon(
                      aes(x = year, ymin = m_position_sm, ymax = w_position_sm),
                      alpha = 0.5,
                      fill = color_gap
                    ) +
                    # 0 hline
                    geom_hline(yintercept = 0,
                               color = color_text_medium,
                               size = 1) +
                    # Scales
                    scale_y_continuous(
                      breaks = seq(-10, 50, 10),
                      limits = c(-10, 50),
                      labels = fifelse(seq(-20, 40, 10) > 0, paste0("+", seq(-10, 50, 10)), as.character(seq(-10, 50, 10)))
                    ) +
                    scale_x_continuous(
                      breaks = seq(1990, 2020, 10),
                      limits = c(1990, 2023),
                      labels = c("1990", "'00", "'10", "'20")
                    ) +
                    # Labels
                    labs(title = NULL,
                         subtitle = "All adult age groups") +
                    # Theme
                    theme_void() +
                    theme(
                      plot.margin = margin(1, 0.5, 1, 1, "cm"),
                      plot.background = element_rect(fill = background, color = NA),
                      plot.subtitle = element_text(
                        hjust = 0,
                        vjust = 2,
                        color = color_text_medium,
                        family = font_base,
                        size = 12
                      ),
                      panel.grid.major.y = element_line(colour = "#B0A7A2"),
                      axis.text = element_text(colour = color_text_medium),
                      legend.position = "none"
                    )]

### 18-34 age group plot
p_2 <- summary_18_34_id[, ggplot(.SD) +
                          # Man
                          geom_point(
                            aes(x = year, y = m_position),
                            size = 2,
                            alpha = 0.5,
                            color = color_m
                          ) +
                          geom_path(aes(x = year, y = m_position_sm),
                                    size = 1.2,
                                    color = color_m) +
                          # Woman
                          geom_point(
                            aes(x = year, y = w_position),
                            size = 2,
                            alpha = 0.5,
                            color = color_w
                          ) +
                          geom_path(aes(x = year, y = w_position_sm),
                                    size = 1.2,
                                    color = color_w) +
                          # Gap
                          geom_ribbon(
                            aes(x = year, ymin = m_position_sm, ymax = w_position_sm),
                            alpha = 0.5,
                            fill = color_gap
                          ) +
                          # 0 hline
                          geom_hline(yintercept = 0,
                                     color = color_text_medium,
                                     size = 1) +
                          # Scales
                          scale_y_continuous(
                            breaks = seq(-10, 50, 10),
                            limits = c(-10, 50),
                            labels = fifelse(seq(-10, 50, 10) > 0,
                                             paste0("+", seq(-10, 50, 10)),
                                             as.character(seq(-10, 50, 10))),
                            position = "right"
                          ) +
                          scale_x_continuous(breaks = seq(2013, 2023, 5),
                                             limits = c(2013, 2023)) +
                          # Labels
                          labs(title = NULL,
                               subtitle = "Young adult population ages 18 to 34") +
                          # Theme
                          theme_void() +
                          theme(
                            plot.margin = margin(1, 0.5, 1, 1, "cm"),
                            plot.background = element_rect(fill = background, color = NA),
                            plot.subtitle = element_text(
                              hjust = 0,
                              vjust = 2,
                              color = color_text_medium,
                              family = font_base,
                              size = 12
                            ),
                            panel.grid.major.y = element_line(colour = "#B0A7A2"),
                            axis.text = element_text(
                              colour = color_text_medium,
                              vjust = 1,
                              hjust = 1
                            ),
                            legend.position = "none"
                          )]

### Combine plots
combined_plot <- p_1 + p_2 + plot_layout(widths = c(2, 1)) +
  plot_annotation(
    title = "Ideology gap is closing up between man and woman in Spain",
    subtitle = "Ideological self-positioning (% left minus % right), by sex",
    "Source: Spanish Sociological Research Center (CIS)\nInspired by FT graphic: John Born-Murdoch @jburnmurdoch\nmichal0091",
    theme = theme(
      plot.background = element_rect(fill = background, color = NA),
      plot.title = element_text(
        hjust = 0,
        vjust = 1,
        color = color_text_base,
        family = font_base,
        size = 18,
        face = "bold"
      ),
      plot.subtitle = element_text(
        hjust = 0,
        vjust = 1,
        color = color_text_medium,
        family = font_base,
        size = 16
      ),
      plot.caption = element_text(
        hjust = 0,
        vjust = 0,
        color = color_text_medium,
        family = font_base,
        size = 10
      ),
    )
  )


# Save plot ---------------------------------------------------------------
cat("Saving plot... \n\n", sep = "")

ggsave(
  filename = "ideology_gap_spain.png",
  path = normalizePath("R/2024/week_04/"),
  plot = combined_plot,
  device = "png",
  units = "cm",
  width = 30,
  height = 15,
  dpi = 320
)

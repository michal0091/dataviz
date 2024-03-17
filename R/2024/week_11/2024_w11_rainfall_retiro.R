# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-03-16
#
# Script Name: 2024_w11_rainfall_retiro.R
#
# Script Description:
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
packages <- c("tidyverse", "data.table", "showtext", "sysfonts") # list of packages to load
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
rainfall <- fread("R/2024/week_11/rainfall_retiro.csv", dec = ",")

rainfall[, year := nafill(year, "locf")]

# Subset 1923-2023
rainfall <- rainfall[year >= 1923]

# Styles ------------------------------------------------------------------
cat("Styles... \n\n", sep = "")

# Color palette
color_background <- "#fcfbf9"
color_font <- "#252b35"
color_accent_1 <- "#79253B"
color_accent_2 <- "#3B7925"
color_gradient_low <- "#253b79"
color_gradient_high <- "#94cfcb"

# Fonts
font_add_google(name = "Lato")
font_base <- "Lato"
showtext_auto()


# Plot --------------------------------------------------------------------
cat("Plot... \n\n", sep = "")


plot_1 <- rainfall[, ggplot(.SD, aes(
  x = reorder(factor(month), -month),
  y = mm,
  color = year
)) +
  geom_jitter(
    position = position_jitter(seed = 1991, width = 0.2),
    size = 3,
    alpha = .5
  ) +
  stat_summary(
    data = .SD[year <= 1955],
    fun = mean,
    geom = "point",
    aes(shape = "≤ 1955"),
    size = 5,
    color = color_accent_2
  ) +
  stat_summary(
    data = .SD[year > 1990],
    fun = mean,
    geom = "point",
    aes(shape = "> 1990"),
    size = 5,
    color = color_accent_1
  ) +
  scale_shape_manual("Mean", values =  c("≤ 1955" = 15, "> 1990" = 17)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 200), expand = c(0.08, 0)) +
  scale_colour_steps(low = color_gradient_low, high = color_gradient_high) +
  labs(
    title = "Rainfall in Retiro, Madrid",
    subtitle = "Monthly rainfall in Retiro, Madrid, 1923-2023",
    caption = "Source: AEMET",
    x = "Month",
    y = "Rainfall (mm)",
    color = NULL
  ) +
  theme_void() +
  theme(
    text = element_text(
      family = font_base,
      color = color_font,
      size = 16
    ),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = color_background, color  = NA),
    legend.position = "bottom",
    legend.key.width = unit(1.4, "cm"),
    legend.key.height = unit(0.35, "cm"),
    axis.text = element_text(
      color = color_font,
      family = font_base,
      size = 16
    ),
    axis.title = element_text(
      color = color_font,
      family = font_base,
      size = 16
    ),
    axis.title.y = element_text(
      color = color_font,
      family = font_base,
      size = 16,
      angle = 90
    ),
    plot.title = element_text(
      hjust = 0,
      vjust = 2,
      color = color_font,
      family = font_base,
      size = 26,
      face = "bold"
    )
  )]

sum_year <- rainfall[, .(sum_year = sum(mm, na.rm = TRUE)), year]
sum_year[, rolling_mean_10y := frollmean(sum_year, 10, align = "right", fill = NA)]
sum_year[, rolling_sd_10y := frollapply(sum_year, 10, sd, align = "right", fill = NA)]

plot_2 <- sum_year[, ggplot(.SD, aes(year, sum_year, color = "Yearly rainfall")) +
                     geom_line() +
                     geom_line(aes(year, rolling_mean_10y, color = "10y roll mean")) +
                     geom_ribbon(aes(y = sum_year, 
                                     ymin = rolling_mean_10y - rolling_sd_10y, 
                                     ymax = rolling_mean_10y + rolling_sd_10y,
                                     fill = "± sd"),
                                 alpha = .2, 
                                 color = NA) +
                     scale_x_continuous(breaks = seq(1923, 2023, 20), 
                                        limits = c(1923, 2023), 
                                        expand = c(.02, .02)) +
                     scale_y_continuous(limits = c(0, 800),
                                        breaks = seq(0, 800, 100)) +
                     scale_color_manual(values = c("Yearly rainfall" = color_gradient_low,
                                                   "10y roll mean" = color_gradient_high)) +
                     scale_fill_manual(values = c("± sd" = color_gradient_high)) +
                     labs(
                       title = "Rainfall in Retiro, Madrid",
                       subtitle = "Yearly rainfall in Retiro, Madrid, 1923-2023",
                       caption = "Source: AEMET",
                       x = "Year",
                       y = "Rainfall (mm)",
                       color = NULL,
                       fill = NULL
                     ) +
                     theme_void() +
                     theme(
                       text = element_text(
                         family = font_base,
                         color = color_font,
                         size = 16
                       ),
                       plot.margin = margin(1, 1, 1, 1, "cm"),
                       plot.background = element_rect(fill = color_background, color  = NA),
                       legend.position = "bottom",
                       axis.text = element_text(
                         color = color_font,
                         family = font_base,
                         size = 16
                       ),
                       axis.title = element_text(
                         color = color_font,
                         family = font_base,
                         size = 16
                       ),
                       axis.title.y = element_text(
                         color = color_font,
                         family = font_base,
                         size = 16,
                         angle = 90
                       ),
                       plot.title = element_text(
                         hjust = 0,
                         vjust = 2,
                         color = color_font,
                         family = font_base,
                         size = 26,
                         face = "bold"
                       ),
                       panel.grid.major = element_line(
                         colour = color_font,
                         linetype = "dotted",
                         linewidth = .3
                       )
                     )]
plot_2



# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-02-11
#
# Script Name: 2024_w06_area_graph.R
#
# Script Description: net wealth by age of head of family
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
packages <- c("tidyverse", "data.table", "zoo", "extrafont") # list of packages to load
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

net_wealth <- fread("R/2024/week_06/median_net_wealth_by_age.csv")
net_wealth[, categoria := factor(
  categoria,
  levels = c(
    "Menor de 35",
    "Entre 35 y 44",
    "Entre 45 y 54",
    "Entre 55 y 64",
    "Entre 65 y 74",
    "Mayor de 75"
  ),
  labels = c("<35",
             "35-44",
             "45-54",
             "55-64",
             "65-74",
             ">75")
)]

# Summary by year
summary_ola <- net_wealth[, .(line_length = sum(valor) + 50, valor = round(sum(valor))), .(ola)]
summary_ola <- summary_ola[order(ola)]


# Styles ------------------------------------------------------------------
cat("Setting style... \n\n", sep = "")

# Color palette
background <- "#f4f7f2"
col_tex <- "#251c7a"
col_accent_1 <- "#e8a9ab"
col_accent_2 <- "#ef563a"
col_accent_3 <- "#d93917"

palette <- c(
  "#2e2c3f",
  "#0b3250",
  "#026876",
  "#00839a",
  "#009cad",
  "#00ad93")

# Load fonts
loadfonts(device = "win")

# Fonts
font_base <- "Lato"
font_title <- "Lato Black"


# Plot --------------------------------------------------------------------
cat("Plotting... \n\n", sep = "")

plot <- net_wealth[,
                   ggplot(.SD, aes(
                     x = ola,
                     y = valor,
                     fill = forcats::fct_rev(categoria)
                   )) +
                     geom_area() +
                     scale_fill_manual(values = palette) +
                     scale_x_continuous(breaks = summary_ola$ola) +
                     scale_y_continuous(breaks = seq(0, 1500, 250), limits = c(0, 1500)) +
                     # Add vertical line for ola
                     geom_segment(
                       data = summary_ola,
                       aes(
                         x = ola,
                         xend = ola,
                         y = 0,
                         yend = line_length,
                         fill = NULL
                       ),
                       linetype = "solid",
                       linewidth = 1.2,
                       color = col_accent_1
                     ) +
                     # Add points for vline
                     geom_point(
                       data = summary_ola,
                       aes(x = ola, y = line_length, fill = NULL),
                       color = col_accent_2,
                       size = 3,
                       show.legend = FALSE
                     ) +
                     # Add text label for segments
                     geom_text(
                       data = summary_ola,
                       aes(
                         x = ola,
                         y = line_length,
                         label = valor,
                         fill = NULL
                       ),
                       hjust = 1,
                       vjust = 1,
                       nudge_y = 100,
                       color = col_accent_3,
                       family = font_base,
                       size = 3
                     ) +
                     labs(
                       title = "Riqueza neta mediana de las familias españolas",
                       subtitle = "por edad del cabeza de familia",
                       x = "Año",
                       y = "Miles de Euros",
                       caption = "Soure: Encuesta Financiera de las Familias\nBanco de España\nmichal0091",
                       fill = NULL
                     ) +
                     theme_void() +
                     theme(
                       plot.margin = margin(1, 1, 1, 1, "cm"),
                       plot.background = element_rect(fill = background, color = NA),
                       plot.title = element_text(
                         hjust = 0,
                         vjust = 6,
                         color = col_tex,
                         family = font_title,
                         size = 28
                       ),
                       plot.subtitle = element_text(
                         hjust = 0,
                         vjust = 6,
                         color = col_tex,
                         family = font_title,
                         size = 18
                       ),
                       plot.caption = element_text(
                         hjust = 1,
                         vjust = -18,
                         color = col_tex,
                         family = font_base,
                         size = 8
                       ),
                       axis.text = element_text(
                         color = col_tex,
                         family = font_base,
                         size = 10
                       ),
                       axis.title.y = element_text(
                         color = col_tex,
                         family = font_base,
                         size = 12,
                         angle = 90
                       ),
                       legend.position = "bottom",
                       legend.title = element_text(
                         hjust = 0.5,
                         color = col_tex,
                         family = font_title
                       ),
                       legend.text = element_text(color = col_tex, family = font_base)
                     )]

# Save plot ---------------------------------------------------------------
cat("Saving plot... \n\n", sep = "")
ggsave(
  filename = "wealth_age_head_family.png",
  path = normalizePath("R/2024/week_06/"),
  plot = plot,
  device = "png",
  units = "cm",
  width = 30 ,
  height = 18.54102
)

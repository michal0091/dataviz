# DataViz week 1 ----------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-01-13
#
# Script Name: 2024_w01_lollipop_eolica.R
#
# Script Description: 
# This script creates a lollipop chart of the top 10 wind turbine manufacturers 
# in Sapin
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
packages <- c("tidyverse", "data.table", "extrafont") # list of packages to load
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

# Load fonts
loadfonts(device = "win")


# Style -------------------------------------------------------------------
cat("Setting style... \n\n", sep = "")

# Color palette
base_clear <- "#efeef2"
base_clear_sh <- "#e1e2e7"
base_text <- "#2e2445"
base_accent_light <- "#f2e1d5"
color_1 <- "#5fb0dd"
color_2 <- "#f04b73"
color_positive <- "#284D33"
color_negative <- "#6B3842"

# Fonts
font_base <- "Lato"
font_title <- "Lato Black"


# Load data ---------------------------------------------------------------
cat("Loading data... \n\n", sep = "")
dt <- fread("R/2024/week_01/eolica.csv", header = T)


# Transform data ----------------------------------------------------------
cat("Transforming data... \n\n", sep = "")

# Long format
dt <- dt %>%
  melt(id.vars = "CCAA", measure.vars = c("2012", "2022"), value.name = "GWh", variable.name = "Año") 

# Set hjust type
dt[, my_hjust := fifelse(GWh[2] - GWh[1] >= 0, 1, 0), CCAA]
dt[Año == 2012, my_hjust := fifelse(my_hjust == 1, 0, 1)]
dt[, nchar := nchar(GWh) + 1]


# Plot --------------------------------------------------------------------
cat("Plotting... \n\n", sep = "")

plot <- dt[,
           # Plot
           ggplot(.SD, aes(
             x = reorder(CCAA, GWh),
             y = GWh,
             fill = Año,
             group = Año)
             ) +
             
             # Geoms
             geom_line(aes(group = CCAA), color = base_accent_light, linewidth =
                         3.5) +
             geom_point(aes(color = Año), size = 3) +
             geom_text(
               aes(label = GWh, color = Año),
               nudge_y = fifelse(my_hjust == 1, nchar * 180, -nchar * 180),
               hjust = my_hjust,
               show.legend = FALSE
             ) +
             
             # Scales
             scale_color_manual(values = c(color_1, color_2)) +
             scale_y_continuous(limits = c(-500, 16000),
                                breaks = seq(0, 16000, 2000)) +
             
             # Flip coordinates
             coord_flip() +
             
             # Labels
             labs(
               title = "Energía eólica en España",
               subtitle = "GWh producidos en 2012 y 2022",
               caption = "Fuente: Elaboración propia a partir de datos oficiales del Gobierno de España\nmichal0091",
               x = NULL,
               y = NULL
             ) +
             
             # Theme
             theme_minimal() +
             theme(
               legend.position = "top",
               text = element_text(color = base_text, family = font_base),
               plot.title = element_text(
                 color = base_text,
                 family = font_title,
                 size = 18
               ),
               axis.text.x = element_text(color = base_text, family = font_title),
               axis.text.y = element_text(
                 color = base_text,
                 family = font_base,
                 size = 12
               ),
               plot.caption =  element_text(
                 color = base_text,
                 family = font_base,
                 size = 8
               ),
               plot.margin = margin(1.5, 0.5, 1, 1, "cm"),
               panel.grid = element_blank(),
               plot.background = element_rect(fill = base_clear)
             ) 
             ]

# Add text 
dt_diff <- dt[, .(diff = GWh[2] - GWh[1]), CCAA]
dt_diff[, diff_label := fcase(
  diff > 0, paste0("+", abs(diff)),
  diff < 0, paste0("-", abs(diff)),
  diff == 0, "0"
)]

# Convert to factor
dt_diff[, CCAA := factor(CCAA, levels = CCAA)]

# Add color
dt_diff[, color := fcase(
  diff > 0, color_positive,
  diff < 0, color_negative,
  diff == 0, base_text
)]

# Make plot box information with difference between 2012 and 2022
plot_box <- dt_diff[, ggplot(.SD) +
                      # Geoms
                      geom_text(
                        aes(
                          x = reorder(CCAA,-as.numeric(CCAA)),
                          y = 0,
                          label = diff_label,
                          family = font_base,
                          vjust = 1
                        ),
                        fontface = "bold",
                        size = 3.5,
                        color = color
                      ) +
                      geom_text(
                        aes(x = 18, y = 0), # 18 is the length + 1 of the x axis
                        label = "dif.",
                        nudge_y = .0,
                        nudge_x = .8, 
                        color = base_text,
                        family = font_title,
                        size = 3.5,
                        vjust = "inward",
                        hjust = "inward"
                      ) +
                      # Flip coordinates
                      coord_flip() +
                      theme_void() +
                      # Theme
                      theme(
                        text = element_text(color = base_text, family = font_base),
                        plot.margin = margin(
                          l = 0,
                          r = 0,
                          b = 0,
                          t = 0
                        ),
                        panel.background = element_rect(fill = base_clear_sh, color = base_clear_sh),
                        legend.position = "none"
                      )]

# Make combined plot
plot_combined <- plot + annotation_custom(ggplotGrob(plot_box), xmin = 0.25, xmax = 19, 
                       ymin = 15500, ymax = 16500)

# Save plot ---------------------------------------------------------------
cat("Saving plot... \n\n", sep = "")

ggsave(
  filename = "eolica.png",
  path = normalizePath("R/2024/week_01/"),
  plot = plot_combined,
  device = "png",
  units = "cm",
  width = 30 ,
  height = 18.54102
)

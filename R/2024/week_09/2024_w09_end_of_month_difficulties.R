# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-02-27
#
# Script Name: 2024_w09_end_of_month_difficulties.R
#
# Script Description:  End of month difficulties of spaniards in 2023
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
packages <- c("ggplot2", "data.table", "zoo", "extrafont", "fontawesome") # list of packages to load
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
dt <- fread("R/2024/week_09/dificultades.csv", dec = ",", encoding = "Latin-1")


# Prepare data ------------------------------------------------------------
cat("Prepare data... \n\n", sep = "")

# Drop "No consta"
dt <- dt[dificultad != "No consta"]

# Set dificultad as factor
dt[, dificultad := factor(
  dificultad,
  levels = c(
    "Con mucha dificultad",
    "Con dificultad",
    "Con cierta dificultad",
    "Con cierta facilidad",
    "Con facilidad",
    "Con mucha facilidad"
  ), labels = c(
    "Con mucha dificultad",
    "Con dificultad",
    "Con cierta dificultad",
    "Con cierta facilidad",
    "Con facilidad",
    "Con mucha facilidad"
  )
)]

# Split data  
spain <- dt[region == "España"]
ccaa  <- dt[region != "España"]

# Prepare ccaa data
# region as factor (map order)
ccaa[, region := factor(
  region,
  levels = c(
    "Galicia",
    "Asturias",
    "Cantabria",
    "País Vasco",
    "Navarra",
    "Castilla y León",
    "La Rioja",
    "Aragón",
    "Cataluña",
    "Baleares",
    "Extremadura",
    "Castilla - La Mancha",
    "Madrid",
    "Valencia",
    "Murcia",
    "Andalucía",
    "Ceuta",
    "Melilla",
    "Canarias"
  )
)]

# Normalize valor column
ccaa[, valor_round := as.integer(round(round(valor / sum(valor), 2) * 100)), by = region]

# Fix normalized values
ccaa[, fix_valor_round := {
  dif_sum <- 100 - sum(valor_round)
  wm <- which.max(abs(valor - valor_round))
  values <- valor_round
  values[wm] <- values[wm] + dif_sum
  values
}, region]

ccaa <- ccaa[order(region, dificultad)]

# Prepare data for waffle chart
waffle_ccaa <- ccaa[, data.table(xvals = 0:99 %/% 10,
                        yvals = 1 - (0:99 %% 10),
                        fill = factor(rep(dificultad , times = fix_valor_round))),
           region]

# Prepare spain data
spain[, valor_round := as.integer(round(round(valor / sum(valor), 2) * 100))]

# Fix normalized values
spain[, fix_valor_round := {
  dif_sum <- 100 - sum(valor_round)
  wm <- which.max(abs(valor - valor_round))
  values <- valor_round
  values[wm] <- values[wm] + dif_sum
  values
}]

spain <- spain[order(dificultad)]

# Prepare data for waffle chart
waffle_spain <- spain[, data.table(xvals = 0:99 %/% 10,
                        yvals = 1 - (0:99 %% 10),
                        fill = factor(rep(dificultad , times = fix_valor_round))),
           region]


# Styles ------------------------------------------------------------------
cat("Setting style... \n\n", sep = "")

# Color palette
background <- "#f8fefd"
title_font_color <- "#9c807d"
font_color <- "#3c3944"

scale_palette <-
  c("#8a1538",
    "#da291c",
    "#ed8b00",
    "#00ed8b",
    "#1cda29",
    "#388a15")

# Load fonts
loadfonts(device = "win")

# Fonts
font_base <- "Lato"
font_title <- "Lato Black"

# Plots -------------------------------------------------------------------
cat("Plotting... \n\n", sep = "")

# Waffle chart Spain
plot_spain <-
  ggplot(waffle_spain, aes(xvals, yvals, color = fill)) +
  geom_text(label = fontawesome('fa-male'),
            family = 'fontawesome-webfont',
            size = 6) +
  coord_equal(expand = TRUE) +
  lims(x  = c(min(df$xvals) - 1, max(df$xvals) + 1),
       y  = c(min(df$yvals) - 1, max(df$yvals) + 1)) +
  scale_color_manual(values = scale_palette) +
  labs(title = "Total Nacional", color = NULL) +
  theme_void() +
  theme(
    axis.title = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = background, color  = NA),
    plot.title = element_text(
      color = font_color,
      family = font_title,
      size = 16,
      hjust = 0.5,
      vjust = -1,
      face = "bold"
    ),
    panel.background = element_rect(color = background, fill = background),
    legend.position = "none"
  )








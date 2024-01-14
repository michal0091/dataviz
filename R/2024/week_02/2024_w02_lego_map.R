# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-01-14
#
# Script Name: 2024_w02_lego_map.R
#
# Script Description: Make lego style map of Poland of the % eco farms
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
packages <- c("tidyverse", "data.table", "sf", "raster", "extrafont") # list of packages to load
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
poland <- sf::st_as_sf(raster::getData('GADM', country = 'POL', level = 1))
eco_farms <- fread("R/2024/week_02/eco_farms_poland.csv")
eco_farms[, perc_eco := eco_farms / farms]

# Join data ---------------------------------------------------------------
cat("Join data... \n\n", sep = "")
poland <- poland %>%
  left_join(eco_farms, by = "NAME_1")


# Prepare data ------------------------------------------------------------
# Make grid
grid <- st_make_grid(poland, n = c(45,45)) %>% st_sf()
grid <- st_join(grid, poland)
grid <- grid %>% filter(!is.na(perc_eco))
centroids <- st_centroid(grid)


# Style -------------------------------------------------------------------
# Color palette
background <- "#0D1F2D"
text <- "#FFFFFF"
palette <-  c("#FF3D3D", "#ffe0c8","#afffff", "#4a9d9c", "#0D6E6E")

# Load fonts
loadfonts(device = "win")

# Fonts
font_base <- "Lato"
font_title <- "Lato Black"

# Plot data ---------------------------------------------------------------
cat("Plot data... \n\n", sep = "")

plot <- 
  ggplot() +
  geom_sf(data = grid, aes(fill = perc_eco), color = background) +
  geom_sf(data = centroids,
          color = alpha("black", 0.75),
          size = 1) +
  labs(fill = "% of eco-farms", title = "Ecological farms in Poland \U1F33D") +
  guides(fill = guide_legend(
    nrow = 1,
    title.position = "top",
    label.position = "bottom"
  )) +
  scale_fill_stepsn(labels = scales::percent, colors = palette,
                    breaks = c(0, .01, .02, .04, .06, .08)) +
  theme_void() +
  theme(
    plot.margin = margin(1, 0.5, 1, 1, "cm"),
    plot.background = element_rect(fill = background, color = NA),
    plot.title = element_text(
      hjust = 0.5,
      vjust = 6,
      color = text,
      family = font_title,
      size = 28 
    ),
    legend.position = "bottom",
    legend.title = element_text(
      hjust = 0.5,
      color = text,
      family = font_title
    ),
    legend.text = element_text(color = text, family = font_base)
  ) 
plot

# Save plot ---------------------------------------------------------------
cat("Saving plot... \n\n", sep = "")

ggsave(
  filename = "eco_farms_poland.png",
  path = normalizePath("R/2024/week_02/"),
  plot = plot,
  device = "png",
  units = "cm",
  width = 15.75,
  height = 21
)

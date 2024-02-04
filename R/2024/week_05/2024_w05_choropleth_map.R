# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-02-03
#
# Script Name: 2024_w05_choropleth_map.R
#
# Script Description: choropleth map of median salary to rental ratio
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
packages <- c("tidyverse", "data.table", "eurostat", "sf", "giscoR",
              "extrafont", "patchwork", "osmdata") # list of packages to load
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
salaries <- fread("R/2024/week_05/salarios.csv", dec = ",")
# Fix the coin column
salaries[, coin := as.numeric(gsub(",", ".", coin))]

# Add city coords
salaries[, c("lon", "lat") := {
  coor <- osmdata::getbb(.BY$city) %>%
    t() %>%
    data.frame() %>%
    sf::st_as_sf(coords = c("x", "y"), crs = 4326) %>%
    sf::st_bbox() %>%
    sf::st_as_sfc() %>%
    sf::st_centroid() %>% unlist() %>% as.numeric()
  .(coor[1], coor[2])
}, by = city]


# Get spatial data
eu_sf <- eurostat::get_eurostat_geospatial(resolution = 10, 
                                           nuts_level = 0, 
                                           year = 2016)

# Merge data 
eu_sf <- eu_sf %>% inner_join(salaries, by = "id")





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
packages <- c("tidyverse", "data.table", "zoo") # list of packages to load
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
rainfall[, month := nafill(month, fill = 0)]


# Plot --------------------------------------------------------------------
cat("Plot... \n\n", sep = "")

rainfall[, ggplot(.SD, aes(x = reorder(factor(month), -month), y = mm, color = year)) +
           geom_jitter(position = position_jitter(seed = 1991, width = 0.2), size = 2, alpha = 0.25) +
           stat_summary(data = .SD[year <= 2000], fun = mean, geom = "point", size = 5) +
           stat_summary(data = .SD[year > 2000], fun = mean, geom = "point", size = 5) +
           coord_flip() +
           scale_y_continuous(limits = c(0, 220), expand = c(0.08, 0))]




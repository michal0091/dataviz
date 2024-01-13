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
base_text <- "#2e2445"
base_accent <- "#b9c602"
base_accent_light <- "#f2e1d5"
color_1 <- "#5fb0dd"
color_2 <- "#f04b73"

# Fonts
font_base <- "Lato-Regular"
font_title <- "Lato-Black"

# Load data ---------------------------------------------------------------
cat("Loading data... \n\n", sep = "")
dt <- fread("R/2024/week_01/eolica.csv", header = T)


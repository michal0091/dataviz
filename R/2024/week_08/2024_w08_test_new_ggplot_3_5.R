# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-02-25
#
# Script Name: 2024_w08_test_new_ggplot_3_5.R
#
# Script Description: Test new ggplot 3.5.0 version. Gradientes,
# Ignorign scales, etc.
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
packages <- c("ggplot2", "data.table", "patchwork", "grid", "extrafont") # list of packages to load
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
data(iris) 
iris <- as.data.table(iris)


# Styles -------------------------------------------------------------------
cat("Setting style... \n\n", sep = "")

# Color palette
iris_colors <- c("#221B4B",
                 "#361D8C",
                 "#504396",
                 "#5352B6",
                 "#BFBEF3")

# Load fonts
loadfonts(device = "win")

# Fonts
font_black <- "Lato Black"
font_normal <- "Lato"


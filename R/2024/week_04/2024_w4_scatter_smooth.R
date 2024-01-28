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
packages <- c("tidyverse", "data.table", "zoo", "extrafont", "ggpubr") # list of packages to load
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

# All age groups

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


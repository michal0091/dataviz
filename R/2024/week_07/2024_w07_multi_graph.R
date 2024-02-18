# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-02-18
#
# Script Name: 2024_w07_multi_graph.R
#
# Script Description: Multi line graph of consumer confidence index of EU countries
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
packages <- c("tidyverse", "data.table", "zoo", "eurostat", "extrafont",
              "gghighlight", "ggtext", "patchwork") # list of packages to load
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

toc <- get_eurostat_toc()

# Get confidence indicators by sector
eurostat_data_code <- "teibs020" 
ci <- get_eurostat(id = eurostat_data_code,
                    stringsAsFactors = TRUE)
ci_labeled <- label_eurostat(cci)

# Set to data table
ci_labeled <- as.data.table(ci_labeled)
cci <- ci_labeled[indic == "Consumer confidence indicator"]

cci_since_2023 <- cci[between(TIME_PERIOD, "2023-01-01", "2023-12-31"),
                      .(median_cci = median(values)), 
                      geo]
cci_since_2023 <- cci_since_2023[order(-median_cci)]

# Select 3 countries with highest median CCI, 3 with lowest and 3 in the middle 
# (bigger economies) 
countries <- c("Lithuania", "Poland", "Denmark",
               "Germany", "Netherlands", "Spain",
               "Portugal", "Greece", "Hungary")

# Selected countries
cci_sel_countries <- cci[geo %in% countries]


# Styles -------------------------------------------------------------------
cat("Setting style... \n\n", sep = "")

# Color palette
background <- "#f8fefd"
title_font_color <- "#bdaaa8"
font_color <- "#605c6e"

palette <- c("#d64408", "#121214", "#96cccd", "#a3ac57", "#3a464f", "#efcf9c",
             "#fac712", "#337553", "#4d3ad8")

# Load fonts
loadfonts(device = "win")

# Fonts
font_base <- "Lato"
font_title <- "Lato Black"


# Plot --------------------------------------------------------------------
cat("Plotting... \n\n", sep = "")

# Base plot
base_plot <-
  cci_sel_countries[,
                    {
                      aux_data <- .SD[, .(TIME_PERIOD = max(TIME_PERIOD),
                                          values = values[which.max(TIME_PERIOD)]),
                                      geo]
                      
                      
                      ggplot(.SD) +
                        geom_hline(yintercept = 0,
                                   linetype = "solid",
                                   size = .25) +
                        geom_point(
                          data = aux_data,
                          aes(x = TIME_PERIOD, y = values, color = geo),
                          shape = 16,
                          size = 2
                        ) +
                        geom_line(aes(x = TIME_PERIOD, y = values, color = geo)) +
                        gghighlight(use_direct_label = F,
                                    unhighlighted_params = list(colour = alpha("gray", 1))) +
                        geom_text(
                          data = aux_data,
                          aes(
                            x = TIME_PERIOD,
                            y = values,
                            color = geo,
                            label = round(values)
                          ),
                          hjust = 1,
                          vjust = -1,
                          size = 3,
                          fontface = "bold"
                        ) +
                        scale_x_date(date_labels = "%m-%y") +
                        scale_y_continuous(limits = c(-50, 25)) +
                        facet_wrap( ~ factor(geo, levels = countries))
                    }]

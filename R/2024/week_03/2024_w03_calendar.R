# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-01-21
#
# Script Name:2024_w03_calendar
#
# Script Description: Make a calendar with ggplot2
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


# Data --------------------------------------------------------------------
cat("Data... \n\n", sep = "")

# Create data frame
calendar <- data.table(
  date = seq.Date(as.Date("2024-01-01"), as.Date("2024-12-31"), by = "day")
)

# Add month
calendar[, month := str_to_title(lubridate::month(date,
                                                  label = TRUE,
                                                  abbr = FALSE))]
calendar[, month := factor(month, levels = unique(month))]

# Add week
calendar[, week := lubridate::week(date)]

# Add day
calendar[, day := day(date)]

# Add weekday
calendar[, weekday := {
  weekdays(date, abbreviate = TRUE) %>% 
    fct_inorder()
}]

# Sunday as first day of week
calendar[, weekend := ifelse(wday(date) %in% c(7, 1), TRUE, FALSE)]

# Add week of month
calendar[, week_month := frank(week, ties.method = "dense"), by = month]


# Style -------------------------------------------------------------------
# Color palette
background <- "#fffefa"
weekday_color <- "#313743"
weekend_color <- "#67b7b5"
holiday_color <- "#fa334c"

# Load fonts
loadfonts(device = "win")

# Fonts
font_base <- "Lato"
font_title <- "Lato Black"


# Plot --------------------------------------------------------------------
cat("Plot... \n\n", sep = "")

calendar %>%
  ggplot(aes(x = weekday, y = week_month)) +
  geom_tile(alpha = 0.8, aes(fill = weekend)) +
  geom_text(aes(label = day), family = "Roboto Condensed") +
  facet_wrap( ~ month, scales = "free_x", ncol = 3) +  ## So that Each Line is Quarter!
  scale_x_discrete() +
  scale_y_reverse(breaks = NULL) +
  labs(title = "2018", x = "Start of week is Monday", y = "") +
  expand_limits(y = c(0.5, 6.5)) +
  theme_void()


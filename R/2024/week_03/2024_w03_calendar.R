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
packages <- c("tidyverse", "data.table", "zoo", "extrafont") # list of packages to load
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
calendar[, weekend_holydays := fifelse(wday(date) %in% c(7, 1), "weekend", "workdays")]

# Holydays Spain
holdays <-
  c(
    "2024-01-01",
    "2024-03-29",
    "2024-05-01",
    "2024-08-15",
    "2024-10-12",
    "2024-11-01",
    "2024-12-06",
    "2024-12-25"
  ) %>% as.Date()

# Add holydays
calendar[, weekend_holydays := fifelse(date %in% holdays, "holydays", weekend_holydays)]

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

plot <- calendar %>%
  ggplot(aes(x = weekday, y = week_month)) +
  geom_tile(alpha = 0.8, aes(fill = weekend_holydays)) +
  geom_text(
    aes(label = day),
    family = font_base,
    color = fifelse(calendar$weekend_holydays != "weekend", "white", "black"),
    size = 3
  ) +
  facet_wrap( ~ month, scales = "free_x", ncol = 3) +
  scale_x_discrete() +
  scale_y_reverse(breaks = NULL) +
  labs(title = "Calendario 2024",
       fill = NULL,
       caption = "Festivos de España en rojo\nmichal0091") +
  theme_void() +
  scale_fill_manual(values = c(holiday_color, weekend_color, weekday_color)) +
  # Add theme elements
  theme(
    plot.margin = margin(1, 0.5, 1, 1, "cm"),
    axis.text.x = element_text(
      color = weekday_color,
      family = font_base,
      size = 10,
      vjust = 2
    ),
    # add plot title
    plot.title = element_text(
      hjust = 0.5,
      vjust = 3,
      color = weekday_color,
      family = font_title,
      size = 28
    ),
    # add plot caption
    plot.caption = element_text(
      hjust = 1,
      color = weekday_color,
      family = font_base,
      size = 8
    ),
    # add legend
    legend.position = "none",
    # add panel spacing
    panel.spacing = unit(0.5, "lines"),
    # add panel background
    panel.background = element_rect(fill = background, color = background),
    # add plot background
    plot.background = element_rect(fill = background, color = background)
  )


# Save plot ---------------------------------------------------------------
cat("Saving plot... \n\n", sep = "")

ggsave(
  filename = "calendar_2024.png",
  path = normalizePath("R/2024/week_03/"),
  plot = plot,
  device = "png",
  units = "cm",
  width = 21,
  height = 15.75
)

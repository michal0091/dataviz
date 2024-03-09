# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-03-09
#
# Script Name: 2024_w10_gender_pay_gap.R
#
# Script Description: Gender pay gap evolution in EU countries between 2012 and 
# 2022
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
packages <- c("tidyverse", "data.table", "showtext") # list of packages to load
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

gender <- fread("R/2024/week_10/gender_pay_gap_2022.csv", dec = ",") 
gender[country == "European Union - 27 countries (from 2020)", country := "EU27"]

# Styles ------------------------------------------------------------------
cat("Setting style... \n\n", sep = "")

# Color palette
color_background <- "#2f2a3a"
color_font <- "#f6f5f1"
color_positive <- "#498867"
color_negative <- "#ea3b1e"

# Fonts
font_add_google(name = "Yeseva One")
font_base <- "Yeseva One"
showtext_auto()

# Plot --------------------------------------------------------------------
cat("Plot... \n\n", sep = "")

colors <- c("2012" = color_font, "2022" = color_font)


plot <- gender[, ggplot(.SD) +
         # Add arrows from 2012 to 2022
         geom_segment(
           aes(
             x = reorder(country, gender_pay_gap_2022),
             xend = reorder(country, gender_pay_gap_2022),
             y = fifelse(
               gender_pay_gap_2022 < gender_pay_gap_2012,
               gender_pay_gap_2012 - 1 ,
               gender_pay_gap_2012 + 1
             ),
             yend = fifelse(
               gender_pay_gap_2022 < gender_pay_gap_2012,
               gender_pay_gap_2022 + 1 ,
               gender_pay_gap_2022 - 1
             )
           ),
           size = 1.1,
           color = fifelse(gender_pay_gap_2022 < gender_pay_gap_2012,
                           color_positive, 
                           color_negative),
           arrow = arrow(
             length = unit(0.1, "cm"),
             type = "closed")) +

         geom_point(
           aes(country, gender_pay_gap_2022, color = "2022"),
           shape = 17,
           size = 1.2) +
         geom_point(
           aes(country, gender_pay_gap_2012, color = "2012"),
           shape = 16,
           size = 1.2
         ) +
         geom_hline(
           yintercept = 0,
           color = color_font,
           linetype = "solid",
           size = 1.2
         ) +
           scale_y_continuous(limits = c(-5, 30), breaks = seq(-5, 30, 5)) +
           scale_color_manual(values = colors) +
         labs(
           title = "Unadjusted gender pay gap* in 2022 and change since 2012, EU-27",
           x = NULL,
           y = "Unadjusted gender pay gap (%)",
           caption = paste(
             "*The unadjusted Gender Pay Gap that represents the difference",
             "between average gross hourly earnings of male paid employees and",
             "of female paid employees as a percentage of average gross hourly",
             "earnings of male paid employees.\nThe population consists of all",
             "paid employees in enterprises with 10 employees or more in NACE",
             "Rev. 2 aggregate B to S (excluding O) - before reference year 2008:\n",
             "NACE Rev. 1.1 aggregate C to O (excluding L)\n",
             "Source: Eurostat\n",
             "@michal0091"
           ),
           color = NULL
         ) +
         theme_void() +
         theme(
           plot.margin = margin(1, 1, 1, 1, "cm"),
           plot.background = element_rect(fill = color_background, color  = NA),
           plot.title = element_text(
             color = color_font,
             family = font_base,
             size = 46,
             hjust = 0.5,
             vjust = 6,
             face = "bold"
           ),
           plot.caption = element_text(
             color = color_font,
             family = font_base,
             size = 16,
             hjust = 0,
             vjust = -12
           ),
           panel.grid.major.y = element_line(
             colour = color_font,
             linetype = "dotted",
             linewidth = .3
           ),
           legend.position = "bottom",
           legend.text = element_text(
             color = color_font,
             family = font_base,
             size = 18,
             face = "bold"),
           axis.text = element_text(
             color = color_font,
             family = font_base,
             size = 16,
             face = "bold"),
           axis.text.x = element_text(
             color = color_font,
             family = font_base,
             angle = 45
           ),
           axis.title.y = element_text(
             color = color_font,
             family = font_base,
             size = 20,
             angle = 90,
             vjust = 4,
             face = "bold"
           )
         )]

# Save plot ---------------------------------------------------------------
cat("Saving plot... \n\n", sep = "")

ggsave(
  filename = "gender_pay_gap.png",
  path = normalizePath("R/2024/week_10/"), 
  plot = plot,
  device = "png",
  units = "cm",
  width = 21,
  height = 13
)


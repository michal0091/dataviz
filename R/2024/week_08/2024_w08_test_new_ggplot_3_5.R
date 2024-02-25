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

# Plots -------------------------------------------------------------------
iris_gradient <- linearGradient(iris_colors)
iris_gradient_light <- linearGradient(iris_colors[2:4])

plot <-
  iris[,
       ggplot(.SD, aes(Species, Sepal.Length)) +
         # Apply new ggplot2 v3.5.0 gradient in fill and staplewidth in boxplot
         geom_boxplot(outliers = FALSE,
                      staplewidth = 0.5,
                      fill = iris_gradient,
                      color = "white",
                      fatten = 2) +
         # Scales
         scale_y_continuous(limits = c(4, 8),
                            breaks = seq(0, 8, 1)) +
         # Labels
         labs(
           title = "Iris",
           subtitle = "Sepal Length by Species",
           x = NULL,
           y = "length (cm)",
           caption = "Source: Fisher's Iris Data\nmichal0091"
         ) +
         # Annotations v3.5.0. I() ‘AsIs’ variables automatically added an identity 
         # scale to the plot
         annotate(
           "rect",
           xmin   = I(0.55),
           xmax   = I(1.68),
           ymin   = I(0.68),
           ymax   = I(0.95),
           fill   = iris_gradient_light,
           colour = NA,
           size   = 1.5
         ) +
         annotate(
           "text",
           label = paste(
             "This famous (Fisher's or Anderson's) iris data set",
             "gives the measurements in centimeters of the variables",
             "sepal length and width and petal length and width,",
             "respectively, for 50 flowers from each of 3 species of iris.",
             "The species are Iris setosa, versicolor, and virginica.",
             sep = "\n"
           ),
           x = I(0.58),
           y = I(0.82),
           size = 2.5,
           hjust = 0, 
           color = iris_colors[5],
           family = font_normal
         ) +
         
         # Theme
         theme_void() +
         theme(
           plot.margin = margin(1, 1, 1, 1, "cm"),
           plot.background = element_rect(fill = iris_colors[5], color  = NA),
           plot.title = element_text(
             color = iris_colors[1],
             family = font_black,
             size = 18,
             hjust = 0,
             vjust = 6,
             face = "bold"
           ),
           plot.subtitle = element_text(
             color = iris_colors[1],
             family = font_normal,
             size = 16,
             hjust = 0,
             vjust = 6, 
             face = "plain"
           ),
           plot.caption = element_text(
             color = iris_colors[1],
             family = font_normal,
             size = 10,
             hjust = 1,
             vjust = -6
           ),
           axis.text.x = element_text(
             color = iris_colors[2],
             family = font_black,
             size = 14
           ),
           axis.title.y = element_text(
             color = iris_colors[2],
             family = font_black,
             size = 10,
             angle = 90,
             vjust = 4
           ),
           axis.text.y = element_text(
             color = iris_colors[2],
             family = font_black,
             size = 10
           ),
           panel.background = element_rect(color = iris_colors[5],
                                           fill  = iris_colors[5])
         )]


# Save plot ---------------------------------------------------------------
cat("Saving plot... \n\n", sep = "")

ggsave(
  filename = "iris.png",
  path = normalizePath("R/2024/week_08/"),
  plot = plot,
  device = "png",
  units = "cm",
  width = 21,
  height = 12.97899)


# pdf format
ggsave(
  filename = "iris.pdf",
  path = normalizePath("R/2024/week_08/"),
  plot = plot,
  device = cairo_pdf,
  units = "cm",
  width = 21,
  height = 12.97899)

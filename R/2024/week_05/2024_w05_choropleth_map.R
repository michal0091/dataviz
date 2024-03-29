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


# Styles ------------------------------------------------------------------
cat("Setting style... \n\n", sep = "")
# Color palette
background <- "#fefdfc"
text <- "#503a5a"
text2 <- "#21be80"
col_1 <- "#636891"
col_2 <- "#f15f79"
col_3 <- "#ebc24c"
sh_1 <- "#ddbfb3"
sh_2 <- "#f5c0ae"

palette <-  c("#167d54", "#21be80", "#aef5c0", "#f5c0ae", "#ebc24c", "#f15f79", "#eb193e")

# Load fonts
loadfonts(device = "win")

# Fonts
font_base <- "Lato"
font_title <- "Lato Black"

# Common theme
my_theme <- theme_void() +
  theme( 
    plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = background, color = NA),
    plot.subtitle = element_text(
      hjust = 0,
      vjust = 1,
      color = text2,
      family = font_base,
      size = 14 
    ),
    plot.caption =  element_text(
      color = text,
      family = font_base,
      size = 10
    ),
    axis.text = element_text(
      color = text,
      family = font_base,
      size = 10
    ),
    legend.position = "bottom",
    legend.title = element_text(
      hjust = 0.5,
      color = text,
      family = font_base
    ),
    legend.key.width = unit(1, "cm"),
    legend.key.height = unit(0.6, "cm"),
    legend.text = element_text(color = text, family = font_base)
  )

# Make plots --------------------------------------------------------------
cat("Make plots... \n\n", sep = "")

# Choropleth map
map_plot <- eu_sf %>%
  ggplot() +
  geom_sf(aes(fill = pe), color = background) +
  geom_point(aes(x = lon, y = lat),
             shape = 15,
             color = text,
             size = 2) +
  geom_text(aes(x = lon, y = lat, label = city),
            color = text,
            size = 3,
            family = font_title, nudge_y = -0.5) +
  labs(fill = "% of net salary destinated to rental",
       subtitle = "Average monthly rental cost of a furnished one-bedroom\n apartmentas as % of the net salary",) +
  guides(fill = guide_legend(
    nrow = 1,
    title.position = "top",
    label.position = "bottom"
  )) +
  scale_fill_stepsn(labels = scales::percent, colors = palette, breaks = c(0.45, 0.6, 0.8, 1.0, 1.2, 1.4, 1.6, 2), limits = c(0.4,1.6)) +
  scale_x_continuous(limits = c(-10, 35)) +
  scale_y_continuous(limits = c(35, 70)) +
  my_theme +
  theme(axis.text = element_blank())

# Bar plot of net salary
bar_plot_salary <- salaries %>%
  ggplot(aes(x = reorder(id, -net))) +
  geom_bar(aes(y = net / 12, fill = "Net salary"),
           stat = "identity") +
  geom_point(aes(y = gross / 12, color = "Gross salary"), size = 1.5) +
  geom_line(aes(
    y = gross / 12,
    group = 1,
    color = "Gross salary"
  ), size = 1.05) +
  labs(x = NULL, y = "EUR",
       subtitle = "Full-time median monthly salary") +
  scale_fill_manual(name = "", values = c("Net salary" = col_1)) +
  scale_color_manual(name = "", values = c("Gross salary" = col_2)) +
  my_theme

# Horizontal bar plot of avarage rental cost
hbar_plot_rental <- salaries %>%
  ggplot() +
  geom_bar(aes(x = reorder(paste0(city, " (", id, ")"), rental_one_bedroom), y = rental_one_bedroom),
           stat = "identity",
           fill = col_3) +
  labs(x = NULL, y = "EUR",
       subtitle = "Average monthly rental cost of\na furnished one-bedroom apartment") +
  scale_y_continuous(limits = c(0, 2500), breaks = seq(0, 3000, 500)) +
  coord_flip() +
  my_theme


# Combine plots -----------------------------------------------------------
cat("Combine plots... \n\n", sep = "")

combined_plot <-
  map_plot +
  (bar_plot_salary + hbar_plot_rental + plot_layout(ncol = 1)) +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = "% of net median salary destinated to rental by Capital",
    caption = "michal0091",
    theme = theme(
      plot.background = element_rect(fill = background, color = NA),
      plot.title = element_text(
        hjust = 0,
        vjust = 1,
        color = text,
        family = font_title,
        size = 20
      ),
      plot.caption =  element_text(
        color = text,
        family = font_base,
        size = 10
      )
    )
  )


# Save --------------------------------------------------------------------
cat("Save plot... \n\n", sep = "")

ggsave(
  filename = "choropleth_map.png",
  path = normalizePath("R/2024/week_05/"),
  plot = combined_plot,
  device = "png",
  units = "cm",
  width =  29.7,
  height = 21,
  dpi = 320
)


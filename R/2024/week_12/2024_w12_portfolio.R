# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-03-24
#
# Script Name: 2024_w12_portfolio.R
#
# Script Description: Protfolio allocation plot
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
packages <- c("tidyverse", "data.table", "zoo", "showtext", "sysfonts", "treemapify",
              "patchwork") # list of packages to load
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
portfolio <- fread("R/2024/week_12/portfolio.csv")
portfolio[, prp := as.numeric(gsub(",", ".", prp))]

# Styles ------------------------------------------------------------------
cat("Setting style... \n\n", sep = "")
source("R/2024/theme.R")


# Plot --------------------------------------------------------------------
cat("Plot... \n\n", sep = "")

pais <- portfolio[type == "pais"][order(-prp)][, .(prp = sum(prp)), by = .(name = fifelse(
  prp > 1, name, paste0("Otros ", length(name), " países")
))]
pais[, name := factor(name, levels = pais$name)]

# Pais  plot
pais_plot <- pais %>%
  ggplot(aes(area = prp, label = paste0(name, " ", sprintf("%2.0f", prp), "%  "))) +
  geom_treemap(fill = color_set_1, color = color_background, layout = "scol") +
  geom_treemap_text(family = "inter_regular",
                    colour = color_background,
                    place = "centre",
                    grow = TRUE, 
                    layout = "scol",
                    min.size = 7) +
  labs(subtitle = "Porcentaje de la cartera en cada país") +
  theme_my()


# Sector
sector <-
  portfolio[type == "sector"][order(-prp)][, .(prp = sum(prp)), by = .(name = fifelse(prp > 2.5, name, paste0("Otros sectores")))]
sector[, name := factor(name, levels = sector$name)]

# Sector base plot
sector_base_plot <-
  sector %>%
  ggplot(aes(prp, name)) +
  geom_col() +
  facet_wrap(~ name, ncol = 1, scales = "free_y") +
  scale_x_continuous(name = "Porcentaje",
                     expand = c(0, 0),
                     limits = c(0, 35)) +
  scale_y_discrete(guide = "none") +
  theme_my() +
  theme(
    strip.background = element_rect(fill = color_background, color = color_background),
    strip.text = element_text(
      color = color_text_2,
      family = "inter_regular",
      hjust = 0,
      margin = margin(1, 1, 1, 1),
      size = rel(1.1),
      face = "bold"
    ),
    panel.grid.major = element_blank()
  )

# Sector plot add text labels
sector_plot <- sector_base_plot +
  geom_text(
    aes(
      label = paste0("  ", sprintf("%2.1f", prp), "%  "),
      color = prp > 5,
      hjust = prp > 5
    ),
    size = 4,
    fontface = "bold",
    family = "inter_regular"
  ) +
  scale_color_manual(values = c(color_text_1, color_background),
                     guide = "none") +
  labs(subtitle = "Porcentaje de la cartera en cada sector", y = NULL)


# Combine plots -----------------------------------------------------------
cat("Combine plots... \n\n", sep = "")
combined_plot <-
  pais_plot + sector_plot +
  plot_layout(widths = c(1, 1)) +
  plot_annotation(
    title = "Distribución de la cartera de ETFs",
    caption = caption_text(source_text = "Own elaboration"),
    theme = theme_my()
  )


# Save plot ---------------------------------------------------------------
cat("Save plot... \n\n", sep = "")

ggsave(
  filename = "portfolio_distr_plot.png",
  path = normalizePath("R/2024/week_12"),
  plot = combined_plot,
  device = "png",
  units = "mm",
  width = 105*2,
  height = 105,
  dpi = 320
)

# Save separately
pais_plot_solo <- pais_plot +
  labs(
    title = "Distribución de la cartera de ETFs",
    caption = caption_text(source_text = "Elaboración propia")) 
ggsave(
    filename = "portfolio_distr_pais_plot.png",
    path = normalizePath("R/2024/week_12"), 
    plot = pais_plot_solo,
    device = "png",
    units = "mm",
    width = 105,
    height = 105,
    dpi = 320
  )
    
    
pais_sector_solo <- sector_plot +
  labs(
    title = "Distribución de la cartera de ETFs",
    caption = caption_text(source_text = "Elaboración propia")) 
ggsave(
  filename = "portfolio_distr_sector_plot.png",
  path = normalizePath("R/2024/week_12"), 
  plot = pais_sector_solo,
  device = "png",
  units = "mm",
  width = 105,
  height = 105,
  dpi = 320
)
    


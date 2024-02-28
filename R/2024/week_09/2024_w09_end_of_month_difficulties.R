# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-02-27
#
# Script Name: 2024_w09_end_of_month_difficulties.R
#
# Script Description:  End of month difficulties of spaniards in 2023
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
packages <- c("ggplot2", "data.table", "zoo", "showtext", "fontawesome",
              "emojifont", "sf", "raster", "dplyr", "patchwork", "stringr",
              "ggtext") # list of packages to load
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
dt <- fread("R/2024/week_09/dificultades.csv", dec = ",", encoding = "Latin-1")
sf_ccaa <- sf::st_as_sf(raster::getData('GADM', country = 'ESP', level = 1))

# Prepare data ------------------------------------------------------------
cat("Prepare data... \n\n", sep = "")

# Drop "No consta"
dt <- dt[dificultad != "No consta"]

# Set dificultad as factor
dt[, dificultad := factor(
  dificultad,
  levels = c(
    "Con mucha dificultad",
    "Con dificultad",
    "Con cierta dificultad",
    "Con cierta facilidad",
    "Con facilidad",
    "Con mucha facilidad"
  ), labels = c(
    "Con mucha dificultad",
    "Con dificultad",
    "Con cierta dificultad",
    "Con cierta facilidad",
    "Con facilidad",
    "Con mucha facilidad"
  )
)]

# Split data  
spain <- dt[region == "España"]
ccaa  <- dt[region != "España"]

# Prepare ccaa data
# region as factor (map order)
ccaa[, region := factor(
  region,
  levels = c(
    "Galicia",
    "Asturias",
    "Cantabria",
    "País Vasco",
    "Navarra",
    "Castilla y León",
    "La Rioja",
    "Aragón",
    "Cataluña",
    "Baleares",
    "Extremadura",
    "Castilla - La Mancha",
    "Madrid",
    "Valencia",
    "Murcia",
    "Andalucía",
    "Ceuta",
    "Melilla",
    "Canarias"
  )
)]

# Normalize valor column
ccaa[, valor_round := as.integer(round(round(valor / sum(valor), 2) * 100)), by = region]

# Fix normalized values
ccaa[, fix_valor_round := {
  dif_sum <- 100 - sum(valor_round)
  wm <- which.max(abs(valor - valor_round))
  values <- valor_round
  values[wm] <- values[wm] + dif_sum
  values
}, region]

ccaa <- ccaa[order(region, dificultad)]

# Prepare data for waffle chart
waffle_ccaa <- ccaa[, data.table(xvals = 0:99 %/% 10,
                        yvals = 1 - (0:99 %% 10),
                        fill = factor(rep(dificultad , times = fix_valor_round))),
           region]

# Prepare spain data
spain[, valor_round := as.integer(round(round(valor / sum(valor), 2) * 100))]

# Fix normalized values
spain[, fix_valor_round := {
  dif_sum <- 100 - sum(valor_round)
  wm <- which.max(abs(valor - valor_round))
  values <- valor_round
  values[wm] <- values[wm] + dif_sum
  values
}]

spain <- spain[order(dificultad)]

# Prepare data for waffle chart
waffle_spain <- spain[, data.table(xvals = 0:99 %/% 10,
                        yvals = 1 - (0:99 %% 10),
                        fill = factor(rep(dificultad , times = fix_valor_round))),
           region]

# Prepare data for choropleth map
ccaa_dificulad <- ccaa[dificultad %in% c("Con mucha dificultad",
                                         "Con dificultad", 
                                         "Con cierta dificultad"), .(valor = sum(valor)), .(region)] 
# Ceuta and Melilla fusion
ccaa_dificulad <- rbind(
  ccaa_dificulad[!(region %in% c("Ceuta", "Melilla"))],
  ccaa_dificulad[region %in% c("Ceuta", "Melilla"), .(region = "Ceuta y Melilla", valor = mean(valor))]
  )

sf_ccaa_names <- sf_ccaa$NAME_1
ccaa_dificulad[, NAME_1 := {
  sf_ccaa_names[trimws(gsub("-", " ", sf_ccaa_names)) %like% trimws(gsub(" -", "", .BY$region))]
  },
  region]
  
# Merge data
sf_ccaa <- merge(sf_ccaa, ccaa_dificulad, by = "NAME_1")

grid <- st_make_grid(sf_ccaa, n = c(45,45)) %>% st_sf()
grid <- st_join(grid, sf_ccaa)
grid <- grid %>% filter(!is.na(valor))
grid$valor <- grid$valor / 100 


# Styles ------------------------------------------------------------------
cat("Setting style... \n\n", sep = "")

# Color palette
background <- "#f8fefd"
title_font_color <- "#9c807d"
font_color <- "#3c3944"

scale_palette <-
  c("#8a1538",
    "#da291c",
    "#ed8b00",
    "#00ed8b",
    "#1cda29",
    "#388a15")

scale_palette_bad <- 
  c("#fae100",
    "#ffb81c",
    "#ed8b00",
    "#da291c",
    "#8a1538")

# Load fonts
font_add_google("Lato")

# Fonts
font_base <- "Lato"

# Plots -------------------------------------------------------------------
cat("Plotting... \n\n", sep = "")

# Waffle chart Spain
plot_spain <-
  ggplot(waffle_spain, aes(xvals, yvals, color = fill)) +
  geom_text(label = fontawesome('fa-male'),
            family = 'fontawesome-webfont',
            size = 6) +
  coord_equal(expand = TRUE) +
  lims(x  = c(min(waffle_spain$xvals) - 1, max(waffle_spain$xvals) + 1),
       y  = c(min(waffle_spain$yvals) - 1, max(waffle_spain$yvals) + 1)) +
  scale_color_manual(values = scale_palette) +
  labs(title = "Total Nacional", color = NULL) +
  theme_void() +
  theme(
    axis.title = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = background, color  = NA),
    plot.title = element_text(
      color = font_color,
      family = font_base,
      size = 32,
      hjust = 0.5,
      vjust = -1,
      face = "bold"
    ),
    panel.background = element_rect(color = background, fill = background),
    legend.position = "none"
  )

# Waffle chart Spain
plot_spain_leg <-
  ggplot(waffle_spain, aes(xvals, yvals, color = fill)) +
  geom_text(label = fontawesome('fa-male'),
            family = 'fontawesome-webfont',
            size = 6) +
  coord_equal(expand = TRUE) +
  lims(x  = c(min(waffle_spain$xvals) - 1, max(waffle_spain$xvals) + 1),
       y  = c(min(waffle_spain$yvals) - 1, max(waffle_spain$yvals) + 1)) +
  scale_color_manual(values = scale_palette) +
  labs(title = "Total Nacional", color = NULL) +
  theme_void() +
  theme(
    axis.title = element_blank(),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = background, color  = NA),
    plot.title = element_text(
      color = font_color,
      family = font_base,
      size = 32,
      hjust = 0.5,
      vjust = -1,
      face = "bold"
    ),
    panel.background = element_rect(color = background, fill = background),
    legend.position = "bottom",
    legend.text = element_text(
      color = font_color,
      family = font_base,
      size = 22),
    strip.background = element_rect(colour = background,
                                    fill = background)
  )


# Waffle chart CCAA
plot_ccaa <- 
  ggplot(waffle_ccaa, aes(xvals, yvals, color = fill)) +
  geom_text(label = fontawesome('fa-male'), 
            family = 'fontawesome-webfont', size = 5) +
  coord_equal(expand = TRUE) +
  lims(x  = c(min(waffle_ccaa$xvals) - 1, max(waffle_ccaa$xvals) + 1),
       y  = c(min(waffle_ccaa$yvals) - 1, max(waffle_ccaa$yvals) + 1)) + 
  scale_color_manual(values = scale_palette) +
  labs(title = "Comunidades Autónomas", color = NULL) +
  theme_void() +
  facet_wrap( ~ region) +
  theme(
    plot.title = element_text(
      color = font_color,
      family = font_base,
      size = 32,
      hjust = 0.5,
      vjust = 6,
      face = "bold"
    ),
    plot.margin = margin(1, 1, 1, 1, "cm"),
    plot.background = element_rect(fill = background, color  = NA),
    panel.spacing.x = unit(1, "lines"),
    panel.spacing.y = unit(1, "lines"),
    panel.background = element_rect(color = background, fill = background),
    strip.text = element_text(
      color = font_color,
      family = font_base,
      size = 26
    ),
    legend.position = "bottom",
    legend.text = element_text(
      color = font_color,
      family = font_base,
      size = 22),
    strip.background = element_rect(colour = background,
                                    fill = background)
  )

# Choropleth map
plot_grid_map <- ggplot() +
  geom_sf(data = grid, aes(fill = valor), color = background) +
  guides(fill = guide_legend(
    nrow = 1,
    title.position = "top",
    label.position = "bottom"
  )) +
  labs(
    fill = "% de personas con dificultades",
    title = "Personas con algún grado de dificultad para llegar a fin",
    subtitle = "Comunidades Autónomas",
    fill = NULL
  ) +
  scale_fill_stepsn(
    labels = scales::percent,
    colors = scale_palette_bad,
    breaks = seq(0.35, 0.7, 0.07)
  ) +
  theme_void() +
  theme(
    plot.margin = margin(1, 0.5, 1, 1, "cm"),
    plot.background = element_rect(fill = background, color = NA),
    plot.title = element_text(
      hjust = 0.5,
      vjust = 2,
      color = font_color,
      family = font_base,
      size = 32,
      face = "bold"
    ),
    plot.subtitle = element_text(
      hjust = 0.5,
      vjust = 2,
      color = font_color,
      family = font_base,
      size = 22
    ),
    legend.position = "bottom",
    legend.title = element_text(
      hjust = 0.5,
      color = font_color,
      family = font_base,
      size = 26
    ),
    legend.text = element_text(
      color = font_color,
      family = font_base,
      size = 22)
  )

# Text --------------------------------------------------------------------
cat("Adding text... \n\n", sep = "")

# Caption
caption_text <- str_glue("**Source:** INE<br>",
                         "**@michal0091**")

# Titles theme 
tit_theme <-
  theme_void() +
  theme(plot.background = element_rect(color = background, fill = background))


# Subtitle
subtitle_text <- data.table(
  x = 0,
  y = 0,
  label = "En España, la sombra de la dificultad para llegar a fin de mes se extiende sobre casi la mitad de la población (48,5%), según datos de 2023. La realidad económica actual, marcada por la inflación, el aumento del coste de la vida y la incertidumbre, golpea con fuerza a los hogares españoles.<br>")

subtitle <-
  ggplot(subtitle_text, aes(x = x, y = y)) +
  geom_textbox(
    aes(label = label),
    box.color = background,
    fill = background,
    family = font_base,
    size = 7,
    lineheight = 0.5,
    color = font_color
  ) +
  coord_cartesian(expand = FALSE, clip = "off") +
  tit_theme


# Combine plots -----------------------------------------------------------
cat("Combine plots... \n\n", sep = "")

# Gathering all parts
design <- 
"AAAB#
 CCCCC
 CCCCC"

comb_right <- plot_spain + wrap_elements(subtitle) + plot_grid_map +
  plot_layout(design = design)

combined_plot  <- plot_ccaa | comb_right

combined_plot <- combined_plot +
  plot_annotation(
    title = "Personas según dificultades para llegar a fin de mes en 2023",
    caption = caption_text,
    theme = theme(
      plot.title = element_text(
        hjust = 0.5,
        vjust = 2,
        color = title_font_color,
        family = font_base,
        size = 42,
        face = "bold"
      ),
      plot.caption = element_markdown(
        hjust = 1,
        size = 22,
        color = font_color,
        family = font_base,
        lineheight = 1.2
      ),
      plot.margin = margin(1, 1, 1, 1, "cm"),
      plot.background = element_rect(color = background, fill = background)
    )
  )


# Save --------------------------------------------------------------------
cat("Save... \n\n", sep = "")

ggsave(
  filename = "end_of_month_difficulties.png",
  path = normalizePath("R/2024/week_09/"),
  plot = combined_plot,
  units = "mm",
  width = 297,
  height = 210,
  dpi = 320
)

# Save individual plots

# CCAA waffle chart
ggsave(
  filename = "end_of_month_difficulties_ccaa.png",
  path = normalizePath("R/2024/week_09/"),
  plot = plot_ccaa,
  units = "mm",
  width = 185,
  height = 185,
  dpi = 320
)

# Spain waffle chart
ggsave(
  filename = "end_of_month_difficulties_spain.png",
  path = normalizePath("R/2024/week_09/"),
  plot = plot_spain_leg,
  units = "mm",
  width = 100,
  height = 100,
  dpi = 320
)

# Choropleth map
ggsave(
  filename = "end_of_month_difficulties_map.png",
  path = normalizePath("R/2024/week_09/"),
  plot = plot_grid_map,
  units = "mm",
  width = 114,
  height = 185,
  dpi = 320
)

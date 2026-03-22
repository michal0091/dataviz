# --- Day 29: Uncertainties - Monochrome ---
# #30DayChartChallenge 2026
# Autor: Michal Kinel

# --- 1. Cargar Librerías ---
library(ggplot2)
library(dplyr)
library(data.table)
library(yaml)
library(showtext)
library(ggtext)
library(scales)

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2026/themes_30DCC2026.R"
utils_file  <- "R/30DayChartChallenge2026/utils.R"
config_file <- "R/30DayChartChallenge2026/30DayChartChallenge2026.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path  <- "R/30DayChartChallenge2026/plots/"

source(themes_file)
source(utils_file)
config <- yaml::read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Cargar y Preparar Datos ---
# TODO: cargar datos

# --- 5. Crear Gráfico (paleta monocromática) ---
source_text <- "TODO: fuente de datos"
plot_title    <- "TODO"
plot_subtitle <- "TODO"

caption <- generate_caption(day = 29, source_text = source_text, config = config)

# Paleta monocromática de ejemplo (adaptar al tema elegido):
# mono_palette <- colorRampPalette(c("#f7f7f7", "#252525"))(7)

# gg <- ggplot(...) +
#   scale_fill_gradientn(colours = mono_palette) +
#   theme_week5(base_size = 14) +
#   labs(title = plot_title, subtitle = plot_subtitle, caption = caption)

# --- 6. Guardar Gráfico ---
# output_file <- file.path(output_path, "day_29_monochrome.png")
# ggsave(filename = output_file, plot = gg,
#        width = 1200, height = 1200, units = "px", dpi = 150, bg = "white")
# message("Día 29 guardado en: ", normalizePath(output_file, mustWork = FALSE))

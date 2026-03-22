# --- Day 17: Relationships - Remake ---
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
# TODO: elegir un gráfico propio o ajeno para rehacer

# --- 5. Crear Gráfico ---
source_text <- "TODO: fuente de datos"
plot_title    <- "TODO (Remake)"
plot_subtitle <- "TODO"

caption <- generate_caption(day = 17, source_text = source_text, config = config)

# gg <- ggplot(...) +
#   theme_week3(base_size = 14) +
#   labs(title = plot_title, subtitle = plot_subtitle, caption = caption)

# --- 6. Guardar Gráfico ---
# output_file <- file.path(output_path, "day_17_remake.png")
# ggsave(filename = output_file, plot = gg,
#        width = 1200, height = 1200, units = "px", dpi = 150, bg = "white")
# message("Día 17 guardado en: ", normalizePath(output_file, mustWork = FALSE))

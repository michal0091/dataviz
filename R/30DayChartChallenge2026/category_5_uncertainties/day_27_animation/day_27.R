# --- Day 27: Uncertainties - Animation ---
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
library(gganimate)  # Para animaciones
# library(gifski)   # Renderer GIF (install if needed)
# library(av)       # Renderer video MP4 (install if needed)

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

# --- 5. Crear Animación ---
source_text <- "TODO: fuente de datos"
plot_title    <- "TODO"
plot_subtitle <- "TODO"

caption <- generate_caption(day = 27, source_text = source_text, config = config)

# gg_anim <- ggplot(...) +
#   theme_week5(base_size = 14) +
#   labs(title = plot_title, subtitle = plot_subtitle, caption = caption) +
#   transition_time(year) +
#   ease_aes("linear")

# --- 6. Guardar Animación ---
# output_file <- file.path(output_path, "day_27_animation.gif")
# anim_save(output_file, animation = animate(gg_anim, width = 800, height = 800, fps = 10, duration = 10))
# message("Día 27 guardado en: ", normalizePath(output_file, mustWork = FALSE))

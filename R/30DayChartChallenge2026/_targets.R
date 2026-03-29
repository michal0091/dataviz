# =============================================================================
# _targets.R — Pipeline targets para el #30DayChartChallenge 2026
# Autor: Michal Kinel
# Ejecutar desde la raíz del proyecto: targets::tar_make()
# =============================================================================
library(targets)
library(tarchetypes)

# -----------------------------------------------------------------------------
# Opciones globales del pipeline
# -----------------------------------------------------------------------------
tar_option_set(
  packages = c(
    "data.table",
    "dplyr",
    "ggplot2",
    "logger",
    "stringr",
    "lubridate",
    "zoo",
    "sf",
    "patchwork",
    "ggtext",
    "scales",
    "glue"
  ),
  format = "rds"
)

# Carga automática de todas las funciones definidas en R/
tar_source("R/30DayChartChallenge2026/R")

# -----------------------------------------------------------------------------
# Configuración de rutas
# -----------------------------------------------------------------------------
CONFIG_FILE  <- "R/30DayChartChallenge2026/30DayChartChallenge2026.yml"
DATA_DIR     <- "R/30DayChartChallenge2026/data"
OUTPUTS_DIR  <- "R/30DayChartChallenge2026/outputs"

# -----------------------------------------------------------------------------
# Pipeline
# -----------------------------------------------------------------------------
list(

  # --- Metadatos compartidos --------------------------------------------------
  tar_target(config,  yaml::read_yaml(CONFIG_FILE), format = "rds"),

  # Day 01 --- Part-to-Whole (Comparisons)
  tar_target(
    url_gcat_activos,
    "https://planet4589.org/space/gcat/tsv/derived/active.tsv"
  ),
  tar_target(
    clean_dia01, 
    prep_dia01_satelites(url_gcat_activos)
  ),
  tar_target(
    plot_dia01, 
    plot_dia01_satelites(clean_dia01, paleta = paleta_funk_2026)
  ),
  tar_target(
    save_dia01,
    ggsave(paste0(OUTPUTS_DIR, "/dia01_part_to_whole.png"), plot_dia01, 
           width = 8, height = 10, bg = paleta_funk_2026["azul_claro"]), 
    format = "file"
  ),

  # Day 02 --- Pictogram (Comparisons)
  tar_target(
    clean_dia02,
    prep_dia02_gaming()
  ),
  tar_target(
    plot_dia02,
    plot_dia02_pictogram(clean_dia02, paleta = paleta_funk_2026)
  ),
  tar_target(
    save_dia02,
    ggsave(paste0(OUTPUTS_DIR,"/dia02_pictogram.png"), plot_dia02, 
           width = 8, height = 10, dpi = 300, bg = "#14141c"), 
    format = "file"
  ),

  # Day 02 --- Mosaic (Comparisons)
  tar_target(
    clean_dia03,
    prep_dia03_energia() 
  ),
  tar_target(
    plot_dia03,
    plot_dia03_mosaico(clean_dia03, paleta = paleta_funk_2026)
  ),
  tar_target(
    save_dia03,
    ggsave(paste0(OUTPUTS_DIR,"/dia03_mosaic.png"), plot_dia03, 
           width = 8, height = 10, dpi = 300, bg = "#14141c"), 
    format = "file"
  ),

  # Day 04 --- Slope (Comparisons)
  tar_target(
    clean_dia04,
    prep_dia04_slope()
  ),
  tar_target(
    plot_dia04,
    plot_dia04_slope(clean_dia04, paleta = paleta_funk_2026)
  ),
  tar_target(
    save_dia04,
    ggsave(paste0(OUTPUTS_DIR, "/dia04_slope.png"), plot_dia04, 
           width = 8, height = 10, dpi = 300, bg = "#14141c"), 
    format = "file"
  ),

  # Day 05 --- Experimental (Comparisons)
  tar_target(
    clean_dia05,
    prep_dia05_experimental()
  ),
  tar_target(
    plot_dia05,
    plot_dia05_experimental(clean_dia05, paleta = paleta_funk_2026)
  ),
  tar_target(
    save_dia05,
    ggsave(paste0(OUTPUTS_DIR, "/dia05_experimental.png"), plot_dia05, 
           width = 8, height = 10, dpi = 300, bg = "#14141c"), 
    format = "file"
  ),

  # Day 06 --- RSF (Editorial)
  tar_target(
    clean_dia06,
    prep_dia06_rsf() 
  ),  
  tar_target(
    plot_dia06,
    plot_dia06_rsf_editorial(clean_dia06)
  ),  
  tar_target(
    save_dia06,
    ggsave(paste0(OUTPUTS_DIR, "/dia06_rsf.png"), plot_dia06, 
           width = 8, height = 10, dpi = 300, bg = "#FFFFFF"), 
    format = "file"
  ),
  
  # Marcador de fin de pipeline (eliminar cuando haya targets reales)
  tar_target(pipeline_listo, TRUE)

)

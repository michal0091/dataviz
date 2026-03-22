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

  # ===========================================================================
  # DÍA 01 — Part-to-Whole (Comparisons)
  # ===========================================================================

  # Nodo 1: Lectura de datos crudos
  # tar_target(
  #   dia01_raw,
  #   prep_raw_dia01(
  #     path = file.path(DATA_DIR, "day_01.csv")
  #   )
  # ),

  # Nodo 2: Limpieza y transformación con data.table
  # tar_target(
  #   dia01_clean,
  #   prep_datos_dia01(dt_raw = dia01_raw)
  # ),

  # Nodo 3: Generación del gráfico con ggplot2
  # tar_target(
  #   dia01_plot,
  #   plot_dia01(
  #     dt    = dia01_clean,
  #     tema  = theme_30dcc(),
  #     config = config,
  #     output = file.path(OUTPUTS_DIR, "day_01_part_to_whole.png")
  #   )
  # ),

  # Marcador de fin de pipeline (eliminar cuando haya targets reales)
  tar_target(pipeline_listo, TRUE)

)

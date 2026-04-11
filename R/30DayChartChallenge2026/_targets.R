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

  # Day 07 --- Multiscale (Distributions)
  tar_target(
    clean_dia07,
    prep_dia07_renta_aeat() 
  ),  
  tar_target(
    plot_dia07,
    plot_dia07_multiscale(clean_dia07, paleta = paleta_sobria_2026)
  ),  
  tar_target(
    save_dia07,
    ggsave(paste0(OUTPUTS_DIR, "/dia07_multiscale.png"), plot_dia07, 
           width = 8, height = 10, dpi = 300, bg = "#faf5ed"), 
    format = "file"
  ),

  # Day 08 --- Circular (Distributions)
  tar_target(
    clean_dia08,
    prep_dia08_circular()
  ),
  
  tar_target(
    plot_dia08,
    plot_dia08_circular(clean_dia08, paleta = paleta_sobria_2026)
  ),
  
  tar_target(
    save_dia08,
    ggsave(paste0(OUTPUTS_DIR, "/dia08_circular.png"), plot_dia08, 
           width = 8, height = 10, dpi = 300, bg = "#faf5ed"), 
    format = "file"
  ),

  # Day 09 --- Wealth (Distributions)
  tar_target(
    clean_dia09,
    prep_dia09_wealth()
  ),
  tar_target(
    plot_dia09,
    plot_dia09_wealth(clean_dia09, paleta = paleta_sobria_2026)
  ),
  tar_target(
    save_dia09,
    ggsave(paste0(OUTPUTS_DIR, "/dia09_wealth.png"), plot_dia09, 
           width = 8, height = 10, dpi = 300, bg = "#faf5ed"), 
    format = "file"
  ),

  # Day 10 --- Pop Culture (Distributions)
  tar_target(
    clean_dia10,
    prep_dia10_popculture()
  ),
  tar_target(
    plot_dia10,
    plot_dia10_popculture(clean_dia10, paleta = paleta_sobria_2026)
  ),
  tar_target(
    save_dia10,
    ggsave(paste0(OUTPUTS_DIR, "/dia10_popculture.png"), plot_dia10, 
           width = 8, height = 11, dpi = 300, bg = "#faf5ed"), 
    format = "file"
  ),

  # Day 11 --- Physical (Distributions)
  tar_target(
    clean_dia11,
    prep_dia11_physical()
  ),
  tar_target(
    plot_dia11,
    plot_dia11_physical(clean_dia11, paleta = paleta_sobria_2026)
  ),
  tar_target(
    save_dia11,
    ggsave(paste0(OUTPUTS_DIR, "/dia11_physical.png"), plot_dia11, 
           width = 8, height = 10, dpi = 300, bg = "#faf5ed"), 
    format = "file"
  ),

  # Day 12 --- FlowingData (Distributions)
  tar_target(
    clean_dia12,
    prep_dia12_flowingdata()
  ),  
  tar_target(
    plot_dia12,
    plot_dia12_flowingdata(clean_dia12)
  ),
  tar_target(
    save_dia12,
    ggsave(paste0(OUTPUTS_DIR, "/dia12_flowingdata.png"), plot_dia12, 
           width = 8, height = 10, dpi = 300, bg = "#ffffff"), 
    format = "file"
  ),

  # Day 13 --- Ecosystems (Relationships)
  tar_target(
    clean_dia13,
    prep_dia13_ecosystems()
  ),  
  tar_target(
    plot_dia13,
    plot_dia13_ecosystems(clean_dia13, paleta = paleta_relaciones)
  ),  
  tar_target(
    save_dia13,
    ggsave(paste0(OUTPUTS_DIR, "/dia13_ecosystems.png"), plot_dia13, 
           width = 8, height = 10, dpi = 300, bg = "#e6e9f0"), 
    format = "file"
  ),
  # Day 14 --- Trade (Relationships)
  tar_target(
    clean_dia14,
    prep_dia14_trade()
  ),  
  tar_target(
    plot_dia14,
    plot_dia14_trade(clean_dia14, paleta = paleta_relaciones)
  ),  
  tar_target(
    save_dia14,
    ggsave(paste0(OUTPUTS_DIR, "/dia14_trade.png"), plot_dia14, 
           width = 8, height = 10, dpi = 300, bg = "#e6e9f0"), 
    format = "file"
  ),

  # Day 15 --- Correlation (Relationships)
  tar_target(
    clean_dia15,
    prep_dia15_correlation()
  ),  
  tar_target(
    plot_dia15,
    plot_dia15_correlation(clean_dia15, paleta = paleta_relaciones)
  ),
  tar_target(
    save_dia15,
    ggsave(paste0(OUTPUTS_DIR, "/dia15_correlation.png"), plot_dia15, 
           width = 8, height = 10, dpi = 300, bg = "#e6e9f0"), 
    format = "file"
  ),

  # Day 16 --- Causation (Relationships)
  tar_target(
    clean_dia16,
    prep_dia16_causation()
  ),  
  tar_target(
    plot_dia16,
    plot_dia16_causation(clean_dia16, paleta = paleta_relaciones)
  ),  
  tar_target(
    save_dia16,
    ggsave(paste0(OUTPUTS_DIR, "/dia16_causation.png"), plot_dia16, 
           width = 8, height = 10, dpi = 300, bg = "#e6e9f0"), 
    format = "file"
  ),

  # Day 17 --- Remake (Relationships)
  tar_target(
    clean_dia17,
    prep_dia17_remake()
  ),
  tar_target(
    plot_dia17,
    plot_dia17_remake(clean_dia17, paleta = paleta_relaciones)
  ),
  tar_target(
    save_dia17,
    ggsave(paste0(OUTPUTS_DIR, "/dia17_remake.png"), plot_dia17, 
           width = 8, height = 10, dpi = 300, bg = "#e6e9f0"), 
    format = "file"
  ),

  # Day 18 --- UNICEF (Relationships)
  tar_target(
    clean_dia18,
    prep_dia18_unicef("R/30DayChartChallenge2026/data/UNICEF_Expanded_Global_Databases_child_food_poverty_2024_2.xlsx")
  ),  
  tar_target(
    plot_dia18,
    plot_dia18_unicef(clean_dia18, paleta = paleta_relaciones)
  ),  
  tar_target(
    save_dia18,
    ggsave(paste0(OUTPUTS_DIR, "/dia18_unicef.png"), plot_dia18, 
           width = 8, height = 10, dpi = 300, bg = "#e6e9f0"), 
    format = "file"
  ),

  # Marcador de fin de pipeline (eliminar cuando haya targets reales)
  tar_target(pipeline_listo, TRUE)

)

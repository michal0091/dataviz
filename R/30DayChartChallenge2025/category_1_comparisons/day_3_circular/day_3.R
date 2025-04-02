# --- Day 3: Circular (Radar Chart - Real Estate Imbalances) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Eurostat)

# --- 1. Cargar Librerías ---
library(data.table)
library(stringr)
library(yaml)
library(showtext)
library(ggtext) # Aunque no usemos element_markdown, generate_caption lo necesita
library(fmsb)     # Para el gráfico de radar
library(scales)   # Para reescalar si fuera necesario (usaremos Z-score)
library(ggradar)  # Paquete para radar charts con ggplot

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"

# Rutas a los archivos de datos
pi_ratio_file <- "R/30DayChartChallenge2025/data/price_to_income.csv"       # TIPSHO60
hpi_file <- "R/30DayChartChallenge2025/data/hpi.csv"                       # TIPSHO40
rent_idx_file <- "R/30DayChartChallenge2025/data/hicp.csv"                   # PRC_HICP_MIDX (Actual rentals)
real_growth_file <- "R/30DayChartChallenge2025/data/estat_tipsho10_filtered_en.csv" # tipsho10
debt_ratio_file <- "R/30DayChartChallenge2025/data/estat_teina510_filtered_en.csv" # teina510
invest_gdp_file <- "R/30DayChartChallenge2025/data/const_gdp.csv"            # TIPSNA50

# Verificar archivos... 
if (!file.exists(utils_file)) stop("Archivo de temas no encontrado.")
if (!file.exists(utils_file)) stop("Archivo de utilidades no encontrado.")
if (!file.exists(config_file)) stop("Archivo de configuración no encontrado.")
if (!file.exists(pi_ratio_file)) stop("Archivo price_to_income.csv no encontrado.")
if (!file.exists(hpi_file)) stop("Archivo hpi.csv no encontrado.")
if (!file.exists(rent_idx_file)) stop("Archivo hicp.csv no encontrado.")
if (!file.exists(real_growth_file)) stop("Archivo estat_tipsho10_filtered_en.csv no encontrado.")
if (!file.exists(debt_ratio_file)) stop("Archivo estat_teina510_filtered_en.csv no encontrado.")
if (!file.exists(invest_gdp_file)) stop("Archivo const_gdp.csv no encontrado.")

source(themes_file) # Carga paletas y tema (aunque el tema no se aplique directamente)
source(utils_file)  # Carga caption y setup_fonts
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Cargar y Procesar Datos ---
target_geo <- c("Austria", "Belgium", "Bulgaria", "Cyprus", "Czechia", "Germany", 
"Denmark", "Estonia", "Spain", "Finland", "France", "Croatia", 
"Hungary", "Ireland", "Italy", "Lithuania", "Luxembourg", "Latvia", 
"Malta", "Netherlands", "Poland", "Portugal", "Romania", "Sweden", 
"Slovenia", "Slovakia")

# Función para leer y extraer valor 2023 (o 2023-Q4 para deuda)
read_and_extract <- function(filepath, code_id, target_year = 2023, is_quarterly = FALSE) {
  dt <- fread(filepath, sep = ",", dec = ".", na.strings = c(":", ""), encoding = "UTF-8", header = TRUE)
  # Limpieza básica de nombres 
  names(dt) <- make.names(names(dt))

  # Renombrar columna geo si es necesario 
  if (!"geo" %in% names(dt)) {
    geo_col <- names(dt)[grepl("^geo", names(dt))]
    if(length(geo_col) == 1) setnames(dt, geo_col, "geo") else stop("Columna Geo no encontrada")
  }

  # Filtrar
  dt_filt <- dt[geo %in% target_geo & TIME_PERIOD == ifelse(is_quarterly, paste0(target_year, "-Q4"), as.character(target_year))]
  dt_filt <- dt_filt[, .(geo, Value = OBS_VALUE)]
  setnames(dt_filt, "Value", code_id) # Renombrar valor con nombre del indicador
  return(unique(dt_filt)) # Devolver filas únicas por país
}

# Cargar datos para 2023 (o 2023-Q4)
dt_pi_ratio <- read_and_extract(pi_ratio_file, "PI_Ratio_Idx") # TIPSHO60 (Anual)
dt_real_growth <- read_and_extract(real_growth_file, "Real_Growth_Pct") # tipsho10 (Anual)
dt_debt_ratio <- read_and_extract(debt_ratio_file, "Debt_Income_Pct", is_quarterly = TRUE) # teina510 (Trimestral -> Q4)
dt_invest_gdp <- read_and_extract(invest_gdp_file, "Invest_GDP_Pct") # TIPSNA50 (Anual)

# Calcular Ratio P/R para 2023
dt_hpi <- fread(hpi_file, sep = ",", dec = ".", na.strings = ":", encoding = "UTF-8", header = TRUE)
setnames(dt_hpi, make.names(names(dt_hpi)))
if (!"geo" %in% names(dt_hpi)) setnames(dt_hpi, names(dt_hpi)[grepl("^geo", names(dt_hpi))], "geo")
dt_hpi[, Year := as.integer(str_extract(TIME_PERIOD, "[0-9]{4}"))]
dt_hpi_2023_avg <- dt_hpi[geo %in% target_geo & Year == 2023, .(HPI_2023 = mean(OBS_VALUE, na.rm=TRUE)), by=geo]

dt_rent <- fread(rent_idx_file, sep = ",", dec = ".", na.strings = ":", encoding = "UTF-8", header = TRUE)
setnames(dt_rent, make.names(names(dt_rent)))
if (!"geo" %in% names(dt_rent)) setnames(dt_rent, names(dt_rent)[grepl("^geo", names(dt_rent))], "geo")
dt_rent[, Year := as.integer(str_extract(TIME_PERIOD, "[0-9]{4}"))]
dt_rent <- dt_rent[coicop == "Actual rentals for housing"]
dt_rent_2023_avg <- dt_rent[geo %in% target_geo & Year == 2023, .(Rent_2023 = mean(OBS_VALUE, na.rm=TRUE)), by=geo]

dt_pr_ratio <- merge(dt_hpi_2023_avg, dt_rent_2023_avg, by="geo")
dt_pr_ratio[, PR_Ratio_Level := HPI_2023 / Rent_2023 * 100]
dt_pr_ratio <- dt_pr_ratio[, .(geo, PR_Ratio_Level)]

# --- Unir todos los indicadores ---
dt_list <- list(dt_pi_ratio, dt_pr_ratio, dt_real_growth, dt_debt_ratio, dt_invest_gdp)
dt_radar <- Reduce(function(x, y) merge(x, y, by = "geo", all = TRUE), dt_list)

# Verificar datos unidos
print("Datos 2023 para Radar (Antes de Normalizar):")
print(dt_radar)

# --- 7. Normalizar Datos (Z-Scores) ---
indicator_cols <- setdiff(names(dt_radar), "geo")
dt_radar_z_values <- dt_radar[, lapply(.SD, scale), .SDcols = indicator_cols]
dt_radar_z <- cbind(dt_radar[, .(geo)], dt_radar_z_values) # Mantener columna geo

# Renombrar columnas para las etiquetas del radar
setnames(dt_radar_z, old = paste0(indicator_cols, ".V1"), new = c(
  "P/I Ratio (Idx Z)",
  "P/R Ratio (Level Z)",
  "Real Growth (% Z)",
  "Debt/Income (% Z)",
  "Invest/GDP (% Z)"
))

# Verificar datos Z
print("Datos Z-Score para Radar:")
print(dt_radar_z)


# --- 8. Crear Gráfico Radar con ggradar ---

# Seleccionar países de interés
selected_geo <- c("Spain", "Germany", "France", "Italy")
dt_radar_z <- dt_radar_z[geo %in% selected_geo]
# Traducir nombres
dt_radar_z[, geo := fcase(
  geo  == "Spain", "España", 
  geo == "Germany", "Alemania",
  geo == "France", "Francia", 
  geo == "Italy", "Italia"
)]

# Definir colores
radar_colors <- paleta_week1[1:nrow(dt_radar_z)] # Asigna un color por país

# Calcular límites para la rejilla basados en Z-scores
min_z <- floor(min(dt_radar_z[, -1], na.rm = TRUE)) 
max_z <- ceiling(max(dt_radar_z[, -1], na.rm = TRUE))

# Crear el gráfico con ggradar
gg_radar <- ggradar(
  plot.data = dt_radar_z,
  base.size = 14, # Tamaño base para texto del radar
  font.radar = "Roboto", # Fuente para etiquetas del radar
  values.radar = c(paste(min_z, "SD"), "0 SD", paste(max_z, "SD")), # Etiquetas para círculos grid
  grid.min = min_z,
  grid.mid = 0,
  grid.max = max_z,
  axis.label.size = 5, # Tamaño etiquetas indicadores (ejes)
  group.line.width = 1,
  group.point.size = 1.1,
  group.colours = radar_colors, # Asignar colores
  legend.position = "bottom",
  legend.title = "",
  legend.text.size = 9
)

# --- 9. Añadir Título, Caption y Tema ggplot ---

# Definir textos
source_text_day3 <- "Eurostat (2023)"
plot_title <- "Perfil de Desequilibrio Inmobiliario (2023)"
plot_subtitle <- "Indicadores Normalizados (Z-score) respecto a la media de la UE\nCentro=Media."

# Generar caption
caption_day3 <- generate_caption(day = 3, source_text = source_text_day3, config = config)

# Añadir labs y tema
gg_day3 <- gg_radar +
  labs(title = plot_title, subtitle = plot_subtitle, caption = caption_day3) +
  theme_week1(base_size = 14) +
  scale_x_continuous(limits = c(-12, 12)) +
  scale_y_continuous(limits = c(-6, 6)) +
  theme(
    legend.position = "bottom",
    aspect.ratio = .478
  )


# --- 10. Guardar Gráfico ---
output_file <- file.path(output_path, "day_3_circular_radar_ggradar.png")
ggsave(
  filename = output_file,
  plot = gg_day3,
  width = 1200, height = 1200, units = "px", dpi = 300, bg = "white"
)

message("Gráfico del Día 3 (Radar con ggradar) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_3.R ---
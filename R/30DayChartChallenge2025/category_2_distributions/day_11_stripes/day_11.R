# --- Day 11: Stripes (High Volatility VIX Days Barcode) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Yahoo Finance)
# Visualización de días con alta volatilidad (VIX > umbral) como "rayas"
# en una línea de tiempo, creando un efecto de código de barras.

# --- 1. Cargar Librerías ---
library(quantmod)
library(ggplot2)
library(data.table)
library(lubridate)
library(yaml)
library(showtext)
library(ggtext)
library(scales)

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"

# Verificar existencia de archivos
if (!file.exists(themes_file)) stop("Archivo de temas no encontrado: ", themes_file)
if (!file.exists(utils_file)) stop("Archivo de utilidades no encontrado: ", utils_file)
if (!file.exists(config_file)) stop("Archivo de configuración no encontrado: ", config_file)

source(themes_file) # Carga paletas y temas
source(utils_file)  # Carga caption y setup_fonts
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Obtener y Preparar Datos VIX ---

# Definir fechas y ticker
end_date <- Sys.Date()
start_date <- as.Date("1993-01-20") # Ampliar para cubrir más presidentes (desde Clinton)
ticker_symbol <- "^VIX"
ticker_name <- "VIX Index"

message("Descargando datos para ", ticker_name, " (", ticker_symbol, ")")
data_xts <- tryCatch({
  getSymbols(
    ticker_symbol,
    src = "yahoo",
    from = start_date,
    to = end_date,
    auto.assign = FALSE
  )
}, error = function(e) {
  warning("Error al descargar ", ticker_symbol, ": ", e$message)
  return(NULL)
})


if (!is.null(data_xts)) {
  price_col_name <- paste0(gsub("\\^", "", ticker_symbol), ".Close")
   if (!price_col_name %in% names(data_xts)) {
      price_col_name <- grep("\\.Close", names(data_xts), value = TRUE)[1]
      if(is.na(price_col_name)) stop("No se encontró la columna de cierre para VIX.")
      warning("Usando columna de cierre detectada: ", price_col_name)
  }
  vix_levels_xts <- data_xts[, price_col_name]
  names(vix_levels_xts) <- "VIX_Level"

  vix_dt <- data.table(
    Date = index(vix_levels_xts),
    VIX_Level = as.numeric(coredata(vix_levels_xts))
  )
  vix_dt <- na.omit(vix_dt)
  message("Procesado ", ticker_name, ". ", nrow(vix_dt), " niveles diarios obtenidos.")

  # --- Filtrar días de alta volatilidad ---
  high_vol_thr <- 30 # Umbral para considerar alta volatilidad (puedes usar 35 si prefieres)
  vix_high_days_dt <- vix_dt[VIX_Level >= high_vol_thr]
  message(nrow(vix_high_days_dt), " días encontrados con VIX >= ", high_vol_thr)

} else {
  stop("No se pudieron obtener datos para ", ticker_name, " (", ticker_symbol, ").")
}

# Verificar datos filtrados
print(head(vix_high_days_dt))


# --- 5. Crear Gráfico de Código de Barras ("Stripes") ---

# Definir textos
source_text_day11 <- paste0("Yahoo Finance (^VIX). Periodo: ",
                           format(start_date, "%Y-%m-%d"), " a ",
                           format(end_date, "%Y-%m-%d"))
plot_title <- paste0("Días de Alta Volatilidad del Mercado (VIX >= ", high_vol_thr, ")")
plot_subtitle <- "Cada 'raya' vertical representa un día donde el Índice VIX cerró por encima del umbral,\nmostrando clústeres de estrés en el mercado a lo largo del tiempo."

# Generar caption
caption_day11 <- generate_caption(
  day = 11,
  source_text = source_text_day11,
  config = config,
  color_text_source = "#E8EAED",
  color_text_author = "#4FC3F7"
)

# Colores y tema 
stripe_color <- paleta_week2_tech[5] 
text_col <- "#E8EAED"
bg_col <- "#202124"

# Crear el gráfico
gg_day11 <- ggplot(vix_high_days_dt, aes(x = Date)) +
  # Usar geom_segment para crear las rayas verticales
  geom_segment(aes(y = 0, yend = 1), # Fija la altura de las rayas
               color = stripe_color,
               linewidth = 0.4) +    # Ajusta el grosor de las rayas

  # Escala de fecha
  scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult = 0.01)) +

  # Aplicar tema personalizado
  theme_week2_tech(base_size = 11) +

  # Ocultar el eje Y ya que no tiene significado aquí
  theme(
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank()
  ) +

  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Año",
    y = NULL, # Eje Y sin título
    caption = caption_day11
  ) +
  # Fijar límites Y (opcional pero ayuda a asegurar que los segmentos llenen el espacio)
  coord_cartesian(ylim = c(0, 1))


# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_11_stripes_vix_barcode.png")
ggsave(
  filename = output_file,
  plot = gg_day11,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col # Más ancho y menos alto
)

message("Gráfico del Día 11 (VIX Barcode Stripes) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_11_stripes_vix_barcode.R ---
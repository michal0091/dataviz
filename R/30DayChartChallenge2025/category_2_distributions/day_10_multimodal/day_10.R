# --- Day 10: Multi-modal (VIX Distribution by US Presidency) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Yahoo Finance, Presidencias: Manual)
# Análisis de la distribución del índice VIX 
# para comparar patrones de volatilidad entre presidencias de EE.UU.

# --- 1. Cargar Librerías ---
library(quantmod)
library(ggplot2)
library(data.table)
library(lubridate)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(ggridges)

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"

# Verificar existencia de archivos
if (!file.exists(themes_file))
  stop("Archivo de temas no encontrado: ", themes_file)
if (!file.exists(utils_file))
  stop("Archivo de utilidades no encontrado: ", utils_file)
if (!file.exists(config_file))
  stop("Archivo de configuración no encontrado: ", config_file)

source(themes_file)
source(utils_file)
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
    if (is.na(price_col_name))
      stop("No se encontró la columna de cierre para VIX.")
    warning("Usando columna de cierre detectada: ", price_col_name)
  }
  vix_levels_xts <- data_xts[, price_col_name]
  names(vix_levels_xts) <- "VIX_Level"
  
  vix_dt <- data.table(Date = index(vix_levels_xts),
                       VIX_Level = as.numeric(coredata(vix_levels_xts)))
  vix_dt <- na.omit(vix_dt)
  message("Procesado ",
          ticker_name,
          ". ",
          nrow(vix_dt),
          " niveles diarios obtenidos.")
} else {
  stop("No se pudieron obtener datos para ",
       ticker_name,
       " (",
       ticker_symbol,
       ").")
}

# --- 4.5 Crear Datos de Presidencias y Unir con VIX ---
presidents_dt <- data.table(
  President = c(
    "Bill Clinton",
    "George W. Bush",
    "Barack Obama",
    "Donald Trump (1st)",
    "Joe Biden",
    "Donald Trump (2nd)"
  ),
  Party = c(
    "Demócrata",
    "Republicano",
    "Demócrata",
    "Republicano",
    "Demócrata",
    "Republicano"
  ),
  StartDate = as.Date(
    c(
      "1993-01-20",
      "2001-01-20",
      "2009-01-20",
      "2017-01-20",
      "2021-01-20",
      "2025-01-20"
    )
  ),
  EndDate = as.Date(
    c(
      "2001-01-20",
      "2009-01-20",
      "2017-01-20",
      "2021-01-20",
      "2025-01-20",
      "2029-01-20"
    )
  ) # Fin mandato
)
presidents_dt[President == "Donald Trump" &
                EndDate > end_date, EndDate := end_date]

# Asignar Presidente a cada día de VIXo
vix_pres_dt <- presidents_dt[vix_dt, on = .(StartDate <= Date, EndDate > Date), nomatch = 0]
# Renombrar para claridad 
setnames(
  vix_pres_dt,
  c("StartDate", "EndDate", "VIX_Level"),
  c("Pres_Start", "Pres_End", "VIX_Level")
)

# Ordenar Presidentes para el gráfico
president_order <- presidents_dt[order(-StartDate), President]
vix_pres_dt[, President := factor(President, levels = president_order)]

# Party como factor
vix_pres_dt[, Party := factor(Party, levels = c("Demócrata", "Republicano"))]

# Verificar merge
print(head(vix_pres_dt))
print(table(vix_pres_dt$President))


# --- 5. Crear Gráfico Ridgeline ---

# Definir umbrales y textos
calma_thr <- 20
estres_thr <- 35
source_text_day10 <- paste0(
  "Yahoo Finance (^VIX), Presidencias USA (Manual). Periodo VIX: ",
  format(min(vix_pres_dt$Pres_Start), "%Y"),
  " a ",
  format(max(vix_pres_dt$Pres_End), "%Y")
)
plot_title <- "Distribución del Índice VIX por Presidencia de EE.UU."
plot_subtitle <- paste0(
  "Comparativa de patrones de volatilidad. Líneas indican umbrales:\n",
  "Calma (<",
  calma_thr,
  "), Estrés (",
  calma_thr,
  "-",
  estres_thr,
  "), ",
  "Pánico (≥",
  estres_thr,
  ")."
)


# Generar caption
caption_day10 <- generate_caption(
  day = 10,
  source_text = source_text_day10,
  config = config,
  color_text_source = "#E8EAED",
  color_text_author = "#4FC3F7"
)

# Paleta de colores
colores_partidos <- setNames(c(paleta_week2_tech[1], paleta_week2_tech[3]),
                             levels(vix_pres_dt$Party))

text_col <- "#E8EAED"
bg_col <- "#202124"
vline_col <- "grey70"
vline_col_stress <- alpha(vline_col, 0.8)

# Posición Y para anotaciones
annotation_y_pos <-  vix_pres_dt[, max(density(VIX_Level)$y)] * 1.2

# Crear el gráfico
gg_day10 <- ggplot(vix_pres_dt, aes(x = VIX_Level, fill = Party)) +
  # Geometría principal
  geom_density(alpha = .7) +  # Transparencia
  # Líneas verticales de umbral
  geom_vline(
    xintercept = calma_thr,
    linetype = "dashed",
    color = vline_col,
    linewidth = 0.8
  ) +
  geom_vline(
    xintercept = estres_thr,
    linetype = "dotted",
    color = vline_col_stress,
    linewidth = 0.7
  ) + # <-- Línea añadida
  
  # Anotaciones para las líneas
  annotate(
    geom = "text",
    x = calma_thr + 5,
    y = annotation_y_pos,
    label = paste0("VIX=", calma_thr),
    color = vline_col,
    hjust = 0,
    size = 4.5,
    family = "Roboto",
    angle = -90
  ) +
  annotate(
    geom = "text",
    x = estres_thr + 5,
    y = annotation_y_pos,
    label = paste0("VIX=", estres_thr),
    # <-- Anotación añadida
    color = vline_col_stress,
    hjust = 0,
    size = 4.5,
    family = "Roboto",
    angle = -90
  ) +
  
  # Escala de colores manual
  scale_fill_manual(values = colores_partidos) +
  # Aplicar tema y ajustar para ridges
  theme_week2_tech(base_size = 11) + # O seguir con tu tema personalizado
  theme(
    panel.grid.major.y = element_blank(),
    axis.text.y = element_text(vjust = 0.5),
    # Personalizar facet_wrap
    strip.background = element_rect(fill = bg_col, color = NA),
    strip.text = element_text(
      color = text_col,
      size = rel(1.4),
      face = "bold"
    )
  ) +
  # Etiquetas
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Nivel Índice VIX",
    y = "Densidad",
    fill = NULL,
    # Se entiende
    caption = caption_day10
  ) +
  # Ajustar límites X si es necesario
  coord_cartesian(xlim = c(0, 85),
                  ylim = c(0, annotation_y_pos * 1.1)) +
  facet_wrap(President ~ .)
               
               
# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_10_multimodal_vix_presidents_ridges.png")
ggsave(
  filename = output_file,
  plot = gg_day10,
  width = 1200,
  height = 1200,
  units = "px",
  dpi = 150,
  bg = bg_col
)

message(
  "Gráfico del Día 10 (VIX Ridgeline) guardado en: ",
  normalizePath(output_file, mustWork = FALSE)
)
# --- Fin day_10_multimodal_vix_presidents_ridges.R ---
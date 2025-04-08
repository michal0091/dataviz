# --- Day 9: Diverging (Tesla vs BYD Relative Monthly Performance) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Yahoo Finance)

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

source(themes_file) # Carga paletas y temas
source(utils_file)  # Carga caption y setup_fonts
config <- read_yaml(config_file)

if (!file.exists(utils_file)) stop("Archivo de utilidades no encontrado.")
if (!file.exists(config_file)) stop("Archivo de configuración no encontrado.")


# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Obtener y Preparar Datos ---

# Definir fechas y tickers
end_date <- Sys.Date()
start_date <- end_date - years(5) # Últimos 5 años
tickers <- c("TSLA", "BYDDY")
names(tickers) <- c("Tesla", "BYD") # Nombres para el gráfico

# Lista para guardar las rentabilidades mensuales
list_monthly_returns <- list()

# Bucle para descargar datos diarios y calcular rentabilidades mensuales
for (i in seq_along(tickers)) {
  ticker_symbol <- tickers[i]
  ticker_name <- names(tickers)[i]

  message("Descargando datos para ", ticker_name, " (", ticker_symbol, ")")
  data_xts <- tryCatch({
    getSymbols(ticker_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  }, error = function(e) {
    warning("Error al descargar ", ticker_symbol, ": ", e$message)
    return(NULL)
  })

  if (!is.null(data_xts)) {
    # Usar precios ajustados si están disponibles
    price_col <- if (paste0(ticker_symbol, ".Adjusted") %in% names(data_xts)) Ad(data_xts) else Cl(data_xts)
    # Calcular rentabilidad logarítmica MENSUAL
    returns_xts <- monthlyReturn(price_col, type = "log")

    # Convertir a data.table
    dt_temp <- data.table(
      Date = index(returns_xts), # Fecha de fin de mes
      Log_Return = as.numeric(coredata(returns_xts))
    )
    dt_temp[, Stock := ticker_name] # Añadir columna con el nombre
    list_monthly_returns[[ticker_name]] <- dt_temp
    message("Procesado ", ticker_name)
  }
}

# Combinar y pivotar a formato ancho
all_returns_dt <- rbindlist(list_monthly_returns)
dt_wide <- dcast(all_returns_dt, Date ~ Stock, value.var = "Log_Return")

# Calcular la diferencia (TSLA - BYD) y el tipo de outperform
# Asegurarse que las columnas existen antes de calcular
if ("Tesla" %in% names(dt_wide) && "BYD" %in% names(dt_wide)) {
  dt_wide[, Diff_TSLA_vs_BYD := Tesla - BYD]
  dt_wide[, Outperform_Type := fifelse(Diff_TSLA_vs_BYD >= 0, "Tesla Outperforms", "BYD Outperforms")]
  dt_wide[, Outperform_Type := factor(Outperform_Type)]
} else {
  stop("No se pudieron obtener datos para ambas acciones (Tesla y BYD).")
}

# Quitar filas con NA en la diferencia (primer mes incompleto)
dt_plot <- na.omit(dt_wide, cols = "Diff_TSLA_vs_BYD")

# Verificar
print(head(dt_plot))

# --- 4.5 Calcular y Formatear Diferencia Acumulada ---

cumulative_diff_log <- dt_plot[, sum(Diff_TSLA_vs_BYD, na.rm = TRUE)]

# Convertir a factor de rendimiento relativo: exp(log_diff)
# Si > 1, TSLA outperform; Si < 1, BYD outperform
relative_factor <- exp(cumulative_diff_log)

# Crear texto para la anotación
if (relative_factor >= 1) {
  annotation_text <- glue::glue(
    "Resultado Acumulado ({nrow(dt_plot)} meses):\n",
    "TSLA ha rendido {sprintf('%.1f', relative_factor)} veces más que BYD"
  )
  annotation_color <- colores_divergentes["Tesla Outperforms"] # Color Verde
} else {
  annotation_text <- glue::glue(
    "Resultado Acumulado ({nrow(dt_plot)} meses):\n",
    "BYD ha rendido {sprintf('%.1f', 1/relative_factor)} veces más que TSLA" # Invertir factor
  )
  annotation_color <- colores_divergentes["BYD Outperforms"] # Color Rosa
}

print(annotation_text) # Para verificar

# --- 5. Crear Gráfico de Barras Divergente ---

# Definir textos
source_text_day9 <- paste0("Yahoo Finance (TSLA, BYDDY). Periodo: ",
                           format(min(dt_plot$Date), "%Y-%m"), " a ",
                           format(max(dt_plot$Date), "%Y-%m"))
plot_title <- "Rendimiento Mensual Relativo: Tesla vs BYD"
plot_subtitle <- "Diferencia en rentabilidad logarítmica mensual (TSLA - BYDDY)\nBarras > 0 indican Tesla superó a BYD."

# Generar caption
caption_day9 <- generate_caption(
  day = 9,
  source_text = source_text_day9,
  config = config,
  color_text_source = "#E8EAED",
  color_text_author = "#4FC3F7"
)

# Colores (ej. de paleta tech: Verde para TSLA, Rosa para BYD)
colores_divergentes <- c(
  "Tesla Outperforms" = paleta_week2_tech[2], # Verde
  "BYD Outperforms"   = paleta_week2_tech[3]  # Rosa
)

text_col <- "#E8EAED"
bg_col <- "#202124"  

annot_y <- max(abs(dt_plot$Diff_TSLA_vs_BYD)) * 0.9

# Crear el gráfico
gg_day9 <- ggplot(dt_plot, aes(x = Date, y = Diff_TSLA_vs_BYD, fill = Outperform_Type)) +
  geom_col(show.legend = FALSE, width = 20) + # Ancho de barras (en días)
  geom_hline(yintercept = 0, color = text_col, linewidth=0.5) + # Línea cero
  annotate(
    "label", # Usar label para un pequeño fondo
    x = min(dt_plot$Date) + days(floor(as.numeric(max(dt_plot$Date) - min(dt_plot$Date)) / 3)), # Posición X 
    y = fifelse(relative_factor >= 1, annot_y, -annot_y), 
    label = annotation_text,
    hjust = 0, 
    vjust = 1,
    family = "Roboto Mono", 
    size = 4.5, # Tamaño texto
    color = annotation_color, 
    label.padding = unit(0.2, "lines"),
    label.r = unit(0.1, "lines"),
    label.size = 0.1, 
    fill = alpha(bg_col, 0.7) 
   ) +
  # Asignar colores manualmente
  scale_fill_manual(values = colores_divergentes) +
  # Formatear eje Y como porcentaje (aproximado para log diff)
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 1)) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y") + # Marcas anuales en eje X
  # Aplicar tema tech
  theme_week2_tech(base_size = 11) +
  # Ajustes adicionales
  theme(panel.grid.major.x = element_blank()) + # Sin grid vertical
  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Mes",
    y = "Diferencia Rentabilidad Log. Mensual (TSLA - BYD)",
    caption = caption_day9
  )

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_9_diverging_tsla_byd.png")
ggsave(
  filename = output_file,
  plot = gg_day9,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col
)

message("Gráfico del Día 9 (Diverging TSLA vs BYD) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_9.R ---
lubridate::make_difftime(max(dt_plot$Date) - min(dt_plot$Date), units = "days")

# --- Day 7: Outliers (Comparison IBEX vs S&P500 vs Nasdaq Daily Returns) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Yahoo Finance)

# --- 1. Cargar Librerías ---
# install.packages("quantmod")
library(quantmod)
library(ggplot2)
library(data.table)
library(lubridate)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(ggrepel)

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"

# Verificar archivos...
if (!file.exists(utils_file)) stop("Archivo de utilidades no encontrado.")
if (!file.exists(config_file)) stop("Archivo de configuración no encontrado.")

source(themes_file) # Carga paletas y temas
source(utils_file)  # Carga caption y setup_fonts
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Obtener y Preparar Datos ---

# Definir fechas y tickers
end_date <- Sys.Date()
start_date <- end_date - years(3) # Últimos 3 años
tickers <- c("^IBEX", "^GSPC", "^IXIC")
names(tickers) <- c("IBEX 35", "S&P 500", "Nasdaq Comp.") # Nombres para el gráfico

# Lista para guardar los data.tables de rentabilidades
list_dt_returns <- list()

# Bucle para descargar, calcular rentabilidades y convertir a data.table
for (i in seq_along(tickers)) {
  ticker_symbol <- tickers[i]
  ticker_name <- names(tickers)[i]

  message("Descargando datos para ", ticker_name, " (", ticker_symbol, ")")
  data_xts <- tryCatch({
    getSymbols(ticker_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
  }, error = function(e) {
    warning("Error al descargar ", ticker_symbol, ": ", e$message)
    return(NULL) # Devuelve NULL si hay error
  })

  if (!is.null(data_xts)) {
    # Calcular rentabilidades diarias simples
    price_col <- if (paste0(gsub("\\^", "", ticker_symbol), ".Adjusted") %in% names(data_xts)) Ad(data_xts) else Cl(data_xts)
    returns_xts <- periodReturn(price_col, period = "daily", type = "log")
    returns_xts <- PerformanceAnalytics::Return.clean(returns_xts) * 100

    # Convertir a data.table
    dt_temp <- data.table(
      Date = index(returns_xts),
      Return_Pct = as.numeric(coredata(returns_xts))
    )
    dt_temp[, Index := ticker_name] # Añadir columna con el nombre del índice

    list_dt_returns[[ticker_name]] <- dt_temp
    message("Procesado ", ticker_name)
  }
}

# Combinar todos los data.tables en uno
all_returns_dt <- rbindlist(list_dt_returns)

# Asegurar que Index es un factor con el orden deseado
all_returns_dt[, Index := factor(Index, levels = names(tickers))]

# Verificar datos combinados
print("Resumen rentabilidades diarias por índice:")
print(all_returns_dt[, .(Min=min(Return_Pct, na.rm=T), P25=quantile(Return_Pct, 0.25, na.rm=T), Med=median(Return_Pct, na.rm=T), P75=quantile(Return_Pct, 0.75, na.rm=T), Max=max(Return_Pct, na.rm=T), N=.N), by=Index])
print(head(all_returns_dt))

# --- Identificar el outlier más extremo (mayor valor absoluto) ---
outlier_extremo <- all_returns_dt[which.max(abs(Return_Pct))]

print("Outlier más extremo:")
print(outlier_extremo)


# --- 5. Crear Gráfico Box Plot Comparativo ---

# Definir textos
source_text_day7 <- paste0("Yahoo Finance (^IBEX, ^GSPC, ^IXIC). Periodo: ",
                           format(min(all_returns_dt$Date), "%Y-%m-%d"), " a ",
                           format(max(all_returns_dt$Date), "%Y-%m-%d"))
plot_title <- "Outliers en Rentabilidades Diarias: IBEX 35 vs S&P 500 vs Nasdaq"
plot_subtitle <- "Comparación de distribuciones y valores atípicos en los últimos ~3 años"

# Generar caption (Asegúrate que tienes la versión con colores claros por defecto en utils.R)
caption_day7 <- generate_caption(
  day = 7,
  source_text = source_text_day7,
  config = config,
  color_text_source = "#E8EAED",
   color_text_author = "#4FC3F7"
)

# --- Definir Degradados Lineales ---
library(grid) # Necesario para linearGradient

panel_col <- "#303134"
text_col <- "#E8EAED"
outlier_col <- paleta_week2_tech[4] # Amarillo/Ámbar

# Crear un degradado para cada índice (Panel -> Color Acento)
gradient_ibex <- linearGradient(colours = c(panel_col, paleta_week2_tech[1]), # Panel -> Cyan
                                  y1 = unit(0, "npc"), y2 = unit(1, "npc")) # De abajo a arriba
gradient_sp500 <- linearGradient(colours = c(panel_col, paleta_week2_tech[2]),# Panel -> Verde
                                   y1 = unit(0, "npc"), y2 = unit(1, "npc"))
gradient_nasdaq <- linearGradient(colours = c(panel_col, paleta_week2_tech[3]),# Panel -> Rosa
                                    y1 = unit(0, "npc"), y2 = unit(1, "npc"))

# Mapear los degradados a los nombres de los índices
gradient_map <- list(
  "IBEX 35" = gradient_ibex,
  "S&P 500" = gradient_sp500,
  "Nasdaq Comp." = gradient_nasdaq
)

# Añadir columna con el objeto de degradado al data.table
all_returns_dt[, fill_gradient := gradient_map[Index]]


# --- Crear el boxplot con relleno degradado ---
gg_day7 <- ggplot(all_returns_dt, aes(x = Index, y = Return_Pct)) +
  geom_boxplot(
    aes(fill = fill_gradient),
    color = text_col,
    outlier.colour = outlier_col, # Color general outliers
    outlier.shape = 18,
    outlier.size = 1.2, # Reducir tamaño de outliers generales
    outlier.alpha = 0.5,
    width = 0.5,
    show.legend = FALSE, 
    linewidth = 0.2
  ) +
  scale_fill_identity() +
  scale_y_continuous(labels = scales::percent_format(scale = 1, accuracy = 1.0)) + # Ajustar accuracy si quieres

  geom_point(data = outlier_extremo, aes(x = Index, y = Return_Pct),
             color = "orange", # Color diferente para destacar
             size = 2.5,           # Tamaño mayor
             shape = 8) +        # Forma de estrella/asterisco
  ggrepel::geom_text_repel(data = outlier_extremo,
            aes(x = Index, y = Return_Pct,
                label = paste(format(Date, "%Y-%m-%d"), "\n", number(Return_Pct, accuracy=0.1, suffix="%"))),
            color = "orange",         # Color etiqueta
            family = "Roboto Mono",
            size = 4.5,               # Tamaño etiqueta
            nudge_y = ifelse(outlier_extremo$Return_Pct > 0, 1, -1), # Empujar etiqueta arriba/abajo
            min.segment.length = 0 # Dibujar siempre línea corta
            ) +

  # Aplicar tema tech
  theme_week2_tech(base_size = 11) +
  # Ajustes adicionales al tema
  theme(
    axis.title.x = element_blank(),
    axis.text.x = element_text(size = rel(0.9), color = text_col)
  ) +
  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    y = "Rentabilidad Logarítmica Diaria",
    caption = caption_day7
  )


# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_7_outliers_indices_boxplot.png") 
ggsave(
  filename = output_file,
  plot = gg_day7,
  width = 1200, height = 1200, units = "px", dpi = 300,
  bg = config$defaults$theme_background_color %||% "#202124" 
)

message("Gráfico del Día 7 (Outliers Índices Boxplot) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_7.R ---

# --- Day 8: Histogram (Daily Log Return Distributions) ---
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
library(grid) # Necesario para linearGradient


# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"

source(themes_file) # Carga paletas y temas
source(utils_file)  # Carga caption y setup_fonts
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Obtener y Preparar Datos (Reciclado de Día 7) ---

# Definir fechas y tickers
end_date <- Sys.Date()
start_date <- end_date - years(3) # Últimos 3 años
tickers <- c("^IBEX", "^GSPC", "^IXIC", "^STOXX50E")
names(tickers) <- c("IBEX 35", "S&P 500", "Nasdaq Comp.", "Euro Stoxx 50")

list_dt_returns <- list()
for (i in seq_along(tickers)) {
  ticker_symbol <- tickers[i]
  ticker_name <- names(tickers)[i]
  message("Descargando datos para ", ticker_name, " (", ticker_symbol, ")")
  data_xts <- tryCatch(getSymbols(ticker_symbol, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE), error = function(e) NULL)
  if (!is.null(data_xts)) {
    price_col <- if (paste0(gsub("\\^", "", ticker_symbol), ".Adjusted") %in% names(data_xts)) Ad(data_xts) else Cl(data_xts)
    returns_xts <- periodReturn(price_col, period = "daily", type = "log") # Log Returns
    returns_xts <- returns_xts[-1, ]
    dt_temp <- data.table(Date = index(returns_xts), Log_Return = as.numeric(coredata(returns_xts)))
    dt_temp[, Index := ticker_name]
    list_dt_returns[[ticker_name]] <- dt_temp
    message("Procesado ", ticker_name)
  }
}
all_returns_dt <- rbindlist(list_dt_returns)
all_returns_dt[, Index := factor(Index, levels = names(tickers))]
all_returns_dt <- na.omit(all_returns_dt) # Quitar NAs por si acaso

# --- 5. Crear Gráfico de Histogramas Facetados ---

# Definir textos
source_text_day8 <- paste0("Yahoo Finance | Periodo: ",
                           format(min(all_returns_dt$Date), "%Y-%m-%d"), " a ",
                           format(max(all_returns_dt$Date), "%Y-%m-%d"))
plot_title <- "Distribución de Rentabilidades Logarítmicas Diarias"
plot_subtitle <- "Histogramas comparativos para IBEX 35, S&P 500, Nasdaq Comp. y Euro Stoxx 50 (~3 años)"

# Generar caption
caption_day8 <- generate_caption(
  day = 8,
  source_text = source_text_day8,
  config = config,
  color_text_source = "#E8EAED",
  color_text_author = "#4FC3F7"
)

# --- Definir Degradados Lineales (como en Día 7) ---
# Colores del tema tech para cada índice 
colores_indices <- paleta_week2_tech[1:length(names(tickers))]
names(colores_indices) <- names(tickers)
text_col <- "#E8EAED"
bg_col <- "#202124"  

gradient_ibex <- linearGradient(colours = c(bg_col, paleta_week2_tech[1]), y1 = unit(0, "npc"), y2 = unit(1, "npc"))
gradient_sp500 <- linearGradient(colours = c(bg_col, paleta_week2_tech[2]), y1 = unit(0, "npc"), y2 = unit(1, "npc"))
gradient_nasdaq <- linearGradient(colours = c(bg_col, paleta_week2_tech[3]), y1 = unit(0, "npc"), y2 = unit(1, "npc"))
gradient_stoxx50 <- linearGradient(colours = c(bg_col, paleta_week2_tech[4]), y1 = unit(0, "npc"), y2 = unit(1, "npc"))


# Mapear degradados a nombres de índices
gradient_map <- list(
  "IBEX 35" = gradient_ibex,
  "S&P 500" = gradient_sp500,
  "Nasdaq Comp." = gradient_nasdaq,
  "Euro Stoxx 50" = gradient_stoxx50
)

# Añadir columna con el objeto de degradado al data.table
all_returns_dt[, fill_gradient := gradient_map[Index]]

# Ancho de las barras del histograma
bin_width <- all_returns_dt[, .(bw = 100 * (2 * IQR(Log_Return) / length(Log_Return)^(1/3))), Index]
print("Ancho de barras por índice:")
print(bin_width)
# Seleccionar promedio
bin_width_chosen <- bin_width[, max(bw)]

# Crear los histogramas facetados
gg_day8 <- ggplot(all_returns_dt, aes(x = Log_Return * 100)) + # Multiplicar por 100 para eje %
  # Histograma: y=after_stat(density) para eje Y densidad, fill por índice
  geom_histogram( aes(y = after_stat(density), color = NULL, fill = fill_gradient),
                 binwidth = bin_width_chosen, # Ancho de barra en %
                 boundary = 0, # Alinear barras con 0
                 position = "identity",
                 show.legend = FALSE) +
  # Añadir linea vertical en 0
  geom_vline(xintercept = 0, show.legend = FALSE, color = text_col, linetype = "dashed", linewidth = 0.4) +
  # Asignar colores de relleno/línea manualmente
  # scale_fill_manual(values = colores_indices) +
  scale_fill_identity() +
  # scale_color_manual(values = colores_indices) + # Descomentar si usas geom_density con color
  # Formatear eje X como porcentaje
  scale_x_continuous(labels = scales::percent_format(scale = 1, accuracy = 0.1)) + 
  # Facetar por índice
  facet_wrap(~ Index, ncol = 2) +
  # Aplicar tema tech
  theme_week2_tech(base_size = 11) +
  # # Ajustes adicionales al tema
  theme(
    strip.background = element_blank(),
    strip.text = element_text(face = "bold", color = "#E8EAED", size = rel(1.1), hjust = 0.05),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    # Quitar grid Y porque el degradado puede interferir visualmente
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    axis.line.x = element_line(color = "#5F6368", linewidth = 0.5) # Mantener línea eje X
  ) +
  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Rentabilidad Logarítmica Diaria (%)",
    y = "Densidad", # Eje Y es densidad
    caption = caption_day8
  )

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_8_histogram_indices.png") # Nuevo nombre
ggsave(
  filename = output_file,
  plot = gg_day8,
  width = 1200, height = 1200, units = "px", dpi = 300, bg = bg_col # Más alto por facets
)

message("Gráfico del Día 8 (Histogramas Índices) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_8.R ---
# --- Day 12: Distributions (US Treasury Yield Spread 10Y-2Y) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: FRED via quantmod / U.S. Department of the Treasury)
# Análisis de la distribución del spread de tipos de interés
# del Tesoro de EE.UU. (10 años - 2 años).

# --- 1. Cargar Librerías ---
library(quantmod)
library(ggplot2)
library(data.table)
library(lubridate)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(grid)

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

# --- 4. Obtener y Preparar Datos de Tasas del Tesoro ---

# Definir fechas y tickers de FRED para tasas de interés
# DGS10: 10-Year Treasury Constant Maturity Rate
# DGS2:  2-Year Treasury Constant Maturity Rate
end_date <- Sys.Date() # Fecha actual: 2025-04-12
start_date <- as.Date("1976-06-01") # Desde que hay datos para ambas series en FRED
fred_tickers <- c("DGS10", "DGS2")

message("Descargando datos de tasas del Tesoro desde FRED para: ", paste(fred_tickers, collapse=", "))
# quantmod descarga los datos y los asigna a objetos con el nombre del ticker
getSymbols(fred_tickers, src = "FRED", from = start_date, to = end_date)

# Verificar que los objetos existen
if (!exists("DGS10") || !exists("DGS2")) {
  stop("Error al descargar los datos de FRED.")
}

# Unir los datos xts
yields_xts <- merge(DGS10, DGS2, all = TRUE) # Usar all=TRUE por si empiezan/terminan distinto

# Convertir a data.table
yields_dt <- data.table(
  Date = index(yields_xts),
  Rate_10Y = as.numeric(coredata(yields_xts$DGS10)),
  Rate_2Y = as.numeric(coredata(yields_xts$DGS2))
)

# Eliminar filas con NA en cualquiera de las dos tasas
yields_dt <- na.omit(yields_dt, cols = c("Rate_10Y", "Rate_2Y"))

# Calcular el Spread 10Y - 2Y (en puntos porcentuales)
yields_dt[, Spread_10Y_2Y := Rate_10Y - Rate_2Y]

message("Calculado el spread 10Y-2Y para ", nrow(yields_dt), " días.")

# Verificar datos
print(head(yields_dt))
summary(yields_dt$Spread_10Y_2Y)

# --- 5. Crear Gráfico de Distribución del Spread ---

# Definir textos
source_text_day12 <- paste0("FRED (Federal Reserve Economic Data: DGS10, DGS2) / U.S. Dept. of the Treasury. Periodo: ",
                           format(min(yields_dt$Date), "%Y-%m"), " a ",
                           format(max(yields_dt$Date), "%Y-%m"))
plot_title <- "Distribución del Spread de Tipos del Tesoro USA (10 Años - 2 Años)"
plot_subtitle <- paste0("Histograma y curva de densidad del spread diario desde ",
                       format(min(yields_dt$Date), "%Y"),
                       ".\nValores < 0 indican curva de tipos invertida.")

# Generar caption
caption_day12 <- generate_caption(
  day = 12,
  source_text = source_text_day12,
  config = config
)

# Colores y tema
hist_gradient_color <- paleta_week2_tech[1] 
density_line_color <- paleta_week2_tech[3] 
text_col <- "#E8EAED"
bg_col <- "#202124"
vline_col <- paleta_week2_tech[5] # Color para línea de inversión



# Crear el objeto de degradado lineal vertical
# Va desde el color de fondo (abajo) hasta el color del histograma (arriba)
the_gradient <- linearGradient(colours = c(bg_col, hist_gradient_color),
                               y1 = unit(0, "npc"), # Inicio abajo del panel
                               y2 = unit(1, "npc")) # Fin arriba del panel


# Calcular el ancho de los bines
bin_width <- yields_dt[, 2 * IQR(Spread_10Y_2Y) / length(Spread_10Y_2Y)^(1/3)]

# Crear el gráfico
gg_day12 <- ggplot(yields_dt, aes(x = Spread_10Y_2Y, y = after_stat(density))) +
  # Histograma normalizado
  geom_histogram(binwidth = bin_width,
                 fill = the_gradient,
                 color = NA) +
  # Curva de densidad
  geom_density(color = density_line_color,
               linewidth = 1.0,
               adjust = 0.8) + # Ajusta suavizado si es necesario

  # Línea vertical en CERO (inversión de la curva)
  geom_vline(xintercept = 0,
             linetype = "dashed",
             color = vline_col,
             linewidth = 0.9) +
  # Anotación para la línea cero (opcional)
  annotate(geom="text", x = -0.15, y = 0.6, label="Inversión (Spread < 0)", 
          color=vline_col, hjust=1, size=4.5, family="Roboto", angle=90) + 
  scale_fill_identity() +
  # Aplicar tema personalizado
  theme_week2_tech(base_size = 11) +

  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Spread 10Y - 2Y (Puntos Porcentuales, %)",
    y = "Densidad",
    caption = caption_day12
  ) +
  # Añadir formato de ejes si se desea (ej, sufijo %)
   theme(plot.subtitle = element_markdown(lineheight = 1.2))


# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_12_distribution_yield_spread.png")
ggsave(
  filename = output_file,
  plot = gg_day12,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col
)

message("Gráfico del Día 12 (Yield Spread Distribution) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_12_distribution_yield_spread.R ---

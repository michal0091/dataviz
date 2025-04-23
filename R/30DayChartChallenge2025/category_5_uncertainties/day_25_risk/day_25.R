# --- Day 25: Risk (Value at Risk & Expected Shortfall for IBEX 35) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Yahoo Finance via quantmod)
# Visualización del VaR y ES (95%) sobre la distribución de retornos diarios del IBEX 35.

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(quantmod)   
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(lubridate)

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"

# Verificar existencia de archivos y cargar
source(themes_file) # Carga paletas y theme_week5_uncertainty
source(utils_file)
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa) # Carga "Lato"

# --- 4. Cargar Datos y Calcular Métricas de Riesgo ---

# Parámetros
ticker <- "^IBEX"
ticker_name <- "IBEX 35"
start_date <- Sys.Date() - years(20) # Últimos 20 años
end_date <- Sys.Date()
alpha_risk <- 0.05 # Nivel de significancia para VaR/ES (5% -> 95% Confianza)

message("Descargando datos para ", ticker_name, " (", ticker, ")")
data_xts <- tryCatch(getSymbols(ticker, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE), error = function(e) NULL)

if (is.null(data_xts)) stop("No se pudieron descargar los datos de Yahoo Finance.")

# Calcular retornos logarítmicos diarios
price_col <- Ad(data_xts) # Usar precio ajustado
returns_xts <- periodReturn(price_col, period = "daily", type = "log")
returns_xts <- returns_xts[-1, ] # Eliminar primer NA
returns_xts <- PerformanceAnalytics::Return.clean(returns_xts) # Limpiar retornos

# Convertir a data.table
returns_dt <- data.table(
    Date = index(returns_xts),
    Log_Return = as.numeric(coredata(returns_xts))
)
returns_dt <- na.omit(returns_dt)
message(nrow(returns_dt), " retornos diarios obtenidos.")

# Calcular VaR y ES (basado en datos históricos/empíricos)
var_level <- as.numeric(quantile(returns_dt$Log_Return, probs = alpha_risk, na.rm = TRUE))
es_level <- as.numeric(mean(returns_dt[Log_Return <= var_level, Log_Return], na.rm = TRUE))

message(sprintf("VaR (%.0f%%) Diario: %.4f (%.2f%%)", (1-alpha_risk)*100, var_level, var_level*100))
message(sprintf("ES  (%.0f%%) Diario: %.4f (%.2f%%)", (1-alpha_risk)*100, es_level, es_level*100))

# Calcular datos de densidad para sombreado
dens <- density(returns_dt$Log_Return, na.rm = TRUE)
dens_dt <- data.table(x = dens$x, y = dens$y)


# --- 5. Crear Gráfico de Densidad con VaR y ES ---

# Definir textos
source_text_day25 <- paste0("Fuente: Yahoo Finance (^IBEX) via quantmod. Periodo: ",
                           format(min(returns_dt$Date), "%Y-%m"), " - ", format(max(returns_dt$Date), "%Y-%m"))
plot_title <- paste0("Riesgo de Mercado: VaR y Expected Shortfall (", ticker_name, ")")
plot_subtitle <- paste0("Distribución de retornos log. diarios y métricas de riesgo calculadas al ",
                       (1-alpha_risk)*100, "% de confianza (VaR = Límite; ES = Pérdida media si se supera VaR).")

# Generar caption
caption_day25 <- generate_caption(
  day = 25, 
  source_text = source_text_day25,
  config = config, 
  color_text_source = "#000000",
  color_text_author = "#455A64"

)

# Colores y tema
bg_col <- "#FFFFFF" # O el de week5 si lo prefieres
text_col <- "#333333"
density_fill <- challenge_palettes$week5_uncertainty['light_grey'] # Gris claro para densidad base
risk_color <- challenge_palettes$week5_uncertainty['orange_accent']     # Rosa para destacar VaR/ES
# O usa el accent2 (rojo) de el_pais si quieres rojo: challenge_palettes$el_pais['accent2']

# Crear el gráfico
gg_day25 <- ggplot(returns_dt, aes(x = Log_Return)) +

  # Área sombreada para ES (cola izquierda por debajo del VaR)
  geom_area(data = dens_dt[x <= var_level], aes(x = x, y = y),
            fill = risk_color, alpha = 0.6) +

  # Curva de densidad base
  geom_density(aes(y = after_stat(density)), fill = density_fill, color = NA, alpha = 0.5) +
  # Podríamos añadir la línea de densidad también
  geom_line(data=dens_dt, aes(x=x, y=y), color = challenge_palettes$week5_uncertainty['medium_grey'], linewidth=0.8) +

  # Línea vertical para VaR
  geom_vline(xintercept = var_level, color = risk_color, linetype = "dashed", linewidth = 1) +
  # Línea vertical para ES (media de la cola)
  geom_vline(xintercept = es_level, color = risk_color, linetype = "dotted", linewidth = 1) +

  # Anotaciones para VaR y ES
  annotate("text", x = var_level * 1.2, y = max(dens_dt$y)*0.8, angle=90, hjust=1.1, vjust=0.5, size=4.5, family="Roboto bold",
           label = paste0("VaR ", (1-alpha_risk)*100, "% = ", scales::percent(var_level, accuracy=0.01)), color = risk_color,
           face = "bold") +
  annotate("text", x = es_level * 1.2, y = max(dens_dt$y)*0.5, angle=90, hjust=1.1, vjust=0.5, size=4.5, family="Roboto bold",
           label = paste0("ES ", (1-alpha_risk)*100, "% = ", scales::percent(es_level, accuracy=0.01)), color = risk_color) +

  # Formato ejes
  scale_x_continuous(labels = scales::percent_format(scale = 100, accuracy = 0.1), name = "Retorno Logarítmico Diario (%)") +
  scale_y_continuous(name = "Densidad de Probabilidad") +

  # Aplicar tema Semana 5
  theme_week5_uncertainty(base_family = "Lato", base_size = 11) +

  # Etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = caption_day25
  ) +
  theme(legend.position = "none")

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_25_risk_var_es_ibex.png")
ggsave(
  filename = output_file,
  plot = gg_day25,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col
)

message("Gráfico del Día 25 (Risk VaR/ES) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_25_risk_var.R ---

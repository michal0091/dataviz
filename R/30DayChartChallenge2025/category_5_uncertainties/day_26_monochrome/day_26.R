# --- Day 26: Monochrome (IBEX 35 Rolling Sharpe Ratio & Volatility) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Yahoo Finance via quantmod)
# Evolución del Ratio de Sharpe anualizado (ventana 252 días, Rf=0) y la
# volatilidad anualizada del IBEX 35, en estilo monocromático.

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(quantmod)
library(PerformanceAnalytics) # Para Return.calculate
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

# --- 4. Cargar Datos y Calcular Métricas Móviles (CON Rf) ---

# Parámetros
ticker_ibex <- "^IBEX"
ticker_name <- "IBEX 35"
bund_name <- "Bono Alemán 10A"
start_date <- Sys.Date() - years(22)
end_date <- Sys.Date()
window_size <- 252


message("Descargando datos para ", ticker_name, " (", ticker_ibex, ")")
ibex_xts <- tryCatch(getSymbols(ticker_ibex, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE), error = function(e) NULL)

message("Cargand datos para ", bund_name)
bund_yield <- fread("R/30DayChartChallenge2025/data/10YBYGER.csv")
bund_xts <- xts(bund_yield$Price, order.by = as.Date(bund_yield$Date, "%m/%d/%Y"))
names(bund_xts) <- "Bund_Yield"

# Calcular retornos logarítmicos diarios IBEX
ibex_price_col <- Ad(ibex_xts)
ibex_returns_xts <- Return.calculate(ibex_price_col, method = "log")
ibex_returns_xts <- ibex_returns_xts[-1, ]
ibex_returns_xts <- PerformanceAnalytics::Return.clean(ibex_returns_xts, method = "boudt") # Limpiar retornos

# Convertir % anual a decimal diario
bund_xts$Rf_daily <- (bund_xts$Bund_Yield / 100) / window_size # Aproximación

# Unir retornos IBEX y Rf diaria
merged_xts <- merge(ibex_returns_xts, bund_xts$Rf_daily, join = "inner") # inner join para tener solo días con ambos datos
merged_xts$join <- NULL
names(merged_xts) <- c("Log_Return", "Rf_daily") # Renombrar columnas

# Convertir a data.table
returns_dt <- data.table(
  Date = index(merged_xts),
  Log_Return = as.numeric(coredata(merged_xts$Log_Return)),
  Rf_daily = as.numeric(coredata(merged_xts$Rf_daily))
)
returns_dt <- na.omit(returns_dt) # Quitar NAs que puedan quedar del merge/calculo

# Calcular Retorno Excedente Diario
returns_dt[, Excess_Return := Log_Return - Rf_daily]

message(nrow(returns_dt), " retornos diarios y Rf obtenidos tras merge.")

# Calcular métricas móviles
message("Calculando métricas móviles (ventana ", window_size, " días)...")
# Media móvil del RETORNO EXCEDENTE
returns_dt[, roll_mean_excess := frollmean(Excess_Return, n = window_size, align = "right")]
# SD móvil del RETORNO ORIGINAL del activo
returns_dt[, roll_sd_return := frollapply(Log_Return, n = window_size, FUN = sd, align = "right")]

# Calcular Sharpe Anualizado (usando Rf) y Volatilidad Anualizada
returns_dt[, roll_sharpe_ann := fifelse(roll_sd_return > 1e-9, (roll_mean_excess / roll_sd_return) * sqrt(window_size), 0)]
returns_dt[, roll_vol_ann := roll_sd_return * sqrt(window_size)]

# Eliminar NAs iniciales de las ventanas móviles
rolling_dt <- na.omit(returns_dt, cols = c("roll_sharpe_ann", "roll_vol_ann"))

message("Cálculos móviles completados. ", nrow(rolling_dt), " puntos finales.")
print(summary(rolling_dt[, .(roll_sharpe_ann, roll_vol_ann)]))


# --- 5. Crear Gráfico Monocromático (Sharpe + Volatilidad) ---

# Definir textos
source_text_day26 <- paste0("Fuente: Yahoo Finance (^IBEX) via quantmod. Ventana móvil: ", window_size, " días. Rf: Bono Alemán 10A (Investing).")
plot_title <- paste("Evolución Ratio de Sharpe y Volatilidad (", ticker_name, ")")
plot_subtitle <- "Ratio de Sharpe anualizado (línea sólida) y Volatilidad anualizada (línea discontinua)\ncalculados sobre ventana móvil, usando Bono Alemán 10A como Tasa Libre de Riesgo."

# Generar caption
caption_day26 <- generate_caption(
  day = 26,
  source_text = source_text_day26,
  config = config,
  color_text_source = "#212121",
  color_text_author = "#757575"
)

# Colores Monocromáticos (Grises)
bg_col <- "#FFFFFF"
sharpe_line_col <- "#212121" # Casi negro para Sharpe (principal)
vol_line_col <- "#757575"    # Gris medio para Volatilidad (secundaria)
text_color_mono <- "#212121"

# --- Cálculo del Factor de Escala (Importante para que quepan las dos líneas) ---
max_sharpe <- max(rolling_dt$roll_sharpe_ann, na.rm=T)
min_sharpe <- min(rolling_dt$roll_sharpe_ann, na.rm=T)
max_vol <- max(rolling_dt$roll_vol_ann, na.rm=T)
scale_factor_vol <- (max_sharpe - min_sharpe) / max_vol * 0.4 # Factor para que Vol ocupe ~60% del rango Y de Sharpe
message("Factor de escala para volatilidad: ", scale_factor_vol)

# Crear el gráfico
gg_day26 <-  ggplot(rolling_dt, aes(x = Date)) +

# Línea de Ratio de Sharpe
geom_line(aes(y = roll_sharpe_ann), color = sharpe_line_col, linewidth = 1.0) +

# Línea de Volatilidad (para mostrar el componente de riesgo)
geom_line(aes(y = roll_vol_ann * scale_factor_vol), # <- Escalar Vol para que quepa bien (ver abajo)
          color = vol_line_col, linetype = "dashed", linewidth = 0.8, alpha=0.8) +

# Eje Y principal para Sharpe Ratio
# Ajustar límites según el rango de tu Sharpe calculado
scale_y_continuous(
  name = "Ratio de Sharpe Anualizado (Rf = Bono Alemán 10A)", # <-- CORREGIDO
  limits = c(min(rolling_dt$roll_sharpe_ann, na.rm=T)-0.1, max(rolling_dt$roll_sharpe_ann, na.rm=T)+0.1),
  expand = expansion(mult=c(0.05, 0.05)),
  sec.axis = sec_axis(~ . / scale_factor_vol, name = "Volatilidad Anualizada (%)",
                     labels = label_percent(scale=100, accuracy=1))
 ) +
  
# Escala X (Fecha)
scale_x_date(date_breaks = "2 years", date_labels = "%Y", expand = expansion(mult=c(0.01, 0.01))) +

# Aplicar tema (versión monocroma basada en theme_light)
theme_light(base_family = "Lato", base_size = 10) %+replace%
  theme(
        plot.background = element_rect(fill = bg_col, color = NA),
        panel.background = element_rect(fill = bg_col, color = NA),
        panel.border = element_blank(),
        panel.grid.major = element_line(color = "#E0E0E0", linewidth = 0.3),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = text_color_mono, linewidth = 0.5),
        axis.ticks = element_line(color = "#CCCCCC"),
        text = element_text(color = text_color_mono),
        plot.title = element_text(hjust = 0, face = "bold", size=rel(1.6), margin = margin(b = 8), color=text_color_mono),
        plot.subtitle = element_text(hjust = 0, size=rel(1.1), margin=margin(b=15)),
        plot.caption = element_markdown(hjust = 0, size=rel(0.8)),
        plot.caption.position = "plot",
        legend.position = "none",
        aspect.ratio = NULL
    ) +

# Etiquetas y caption
labs(
  title = plot_title,
  subtitle = plot_subtitle,
  x = "Año",
  caption = caption_day26
) +
# Anotación para explicar la línea discontinua
annotate("text", x=max(rolling_dt$Date) - years(4), y=max(rolling_dt$roll_sharpe_ann), # Ajusta posición Y
         label="Línea discontinua:\nVolatilidad Anualizada\n(Eje Derecho %)", 
         hjust=0, vjust=1, size=6, family="Lato", color=vol_line_col, lineheight=0.9)



# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_26_monochrome_sharpe_vol.png")
ggsave(
  filename = output_file,
  plot = gg_day26,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col 
)

message("Gráfico del Día 26 (Monochrome Sharpe & Vol) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_26_monochrome_sharpe.R ---
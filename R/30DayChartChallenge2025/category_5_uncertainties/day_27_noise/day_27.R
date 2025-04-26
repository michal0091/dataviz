# --- Day 27: Noise (CAPM Residuals & ADF Test for TEF.MC) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Yahoo Finance, Investing via User CSV)
# Visualización de los residuos ("ruido") del modelo CAPM para Telefónica (TEF.MC)
# y resultado del test de estacionariedad Dickey-Fuller (ADF).

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(quantmod)
library(PerformanceAnalytics)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(lubridate)
library(tseries) 

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

# --- 4. Cargar Datos, Calcular Retornos y Estimar CAPM ---

# Parámetros
ticker_stock <- "TEF.MC" # Acción: Telefónica
ticker_market <- "^IBEX"  # Mercado: IBEX 35
stock_name <- "Telefónica (TEF.MC)"
market_name <- "IBEX 35"
bund_csv_path <- "R/30DayChartChallenge2025/data/10YBYGER.csv" # Bono Alemán 10A
start_date <- Sys.Date() - years(20) # Últimos 20 años para regresión
end_date <- Sys.Date()
window_size <- 252 # Para Rf diaria (podría usarse otro método si se quiere Rf más precisa)

# Descargar datos de acciones y mercado
message("Descargando datos para ", stock_name, " y ", market_name, "...")
tickers_list <- c(ticker_stock, ticker_market)
data_list_xts <- tryCatch(
    lapply(tickers_list, function(tk) {
        getSymbols(tk, src = "yahoo", from = start_date, to = end_date, auto.assign = FALSE)
    }), error = function(e) {warning("Error descargando datos de Yahoo: ", e$message); NULL}
)
# Cargar datos del Bund
bund_yield <- fread(bund_csv_path)
bund_xts <- xts(bund_yield$Price, order.by = as.Date(bund_yield$Date, "%m/%d/%Y"))
names(bund_xts) <- "Bund_Yield"

if (is.null(data_list_xts) || length(data_list_xts) != 2 || is.null(data_list_xts[[1]]) || is.null(data_list_xts[[2]])) {
    stop("No se pudieron descargar todos los datos necesarios (Acción y/o Mercado).")
}

stock_xts <- data_list_xts[[1]]
market_xts <- data_list_xts[[2]]

# Calcular retornos logarítmicos diarios
stock_ret_xts <- Return.calculate(Ad(stock_xts), method = "log")[-1, ]
stock_ret_xts <- PerformanceAnalytics::Return.clean(stock_ret_xts, method = "boudt") # Limpiar retornos
market_ret_xts <- Return.calculate(Ad(market_xts), method = "log")[-1, ]
market_ret_xts <- PerformanceAnalytics::Return.clean(market_ret_xts, method = "boudt") # Limpiar retornos
names(stock_ret_xts) <- "Stock_Ret"
names(market_ret_xts) <- "Market_Ret"

# Preparar Rf diaria (aproximación)
bund_xts$Rf_daily <- (bund_xts$Bund_Yield / 100) / window_size

# Unir todas las series
merged_xts <- merge(stock_ret_xts, market_ret_xts, bund_xts$Rf_daily, join = "inner")
merged_dt <- data.table(Date = index(merged_xts), coredata(merged_xts))
merged_dt[, join := NULL]

# Cambiar a numericos
cols <- c("Stock_Ret", "Market_Ret", "Rf_daily")
merged_dt[, (cols) := lapply(.SD, as.numeric), .SDcols = cols]
merged_dt <- na.omit(merged_dt)

# Calcular Retornos Excedentes
merged_dt[, Stock_Exc_Ret := Stock_Ret - Rf_daily]
merged_dt[, Market_Exc_Ret := Market_Ret - Rf_daily]

message("Datos listos para regresión CAPM: ", nrow(merged_dt), " observaciones.")

# Estimar Modelo CAPM
message("Estimando modelo CAPM...")
capm_model <- lm(Stock_Exc_Ret ~ Market_Exc_Ret, data = merged_dt)
print(summary(capm_model))

# Extraer Residuos ("Ruido")
merged_dt[, Residuals := residuals(capm_model)]

# Realizar Test ADF sobre los Residuos
message("Realizando Test Dickey-Fuller Aumentado sobre los residuos...")
adf_test_result <- tseries::adf.test(merged_dt$Residuals)
print(adf_test_result)

# Guardar resultados del test para el subtítulo
adf_stat <- round(adf_test_result$statistic, 2)
adf_pvalue_raw <- adf_test_result$p.value
adf_pvalue_text <- ifelse(adf_pvalue_raw < 0.01, "< 0.01", round(adf_pvalue_raw, 3))


# --- 5. Crear Gráfico de la Serie Temporal de Residuos ---

# Definir textos (Incluir resultados ADF)
source_text_day27 <- paste0("Fuente: Yahoo Finance (", ticker_stock,", ", ticker_market,"), Investing (Bund), Cálculos Propios.")
plot_title <- paste0("Ruido Idiosincrático: Residuos del Modelo CAPM (", stock_name, ")")
plot_subtitle <- paste0("Serie temporal del error diario de la regresión RetEx(", ticker_stock,") ~ RetEx(", market_name,").\n",
                       "Test Dickey-Fuller Aumentado sobre residuos:\nEstadístico = ", adf_stat, ", p-valor = ", adf_pvalue_text, ".")

# Interpretar p-valor para subtítulo (opcional)
adf_interpretation <- ifelse(adf_pvalue_raw < 0.05,
                            " (Indica residuos probablemente estacionarios)",
                            " (Indica residuos posiblemente NO estacionarios - raíz unitaria)")
plot_subtitle <- paste0(plot_subtitle, adf_interpretation)


# Generar caption
caption_day27 <- generate_caption(
  day = 27,
  source_text = source_text_day27,
  config = config,
  color_text_source = "#000000",
  color_text_author = "#455A64"

)

# Colores y tema
bg_col <- "#FFFFFF"
resid_color <- challenge_palettes$week5_uncertainty['medium_grey'] 
text_color_mono <- "#333333"
zero_line_color <- challenge_palettes$week5_uncertainty['orange_accent'] 

# Crear el gráfico de residuos
gg_day27 <- ggplot(merged_dt, aes(x = Date, y = Residuals)) +
  # Línea cero para referencia
  geom_hline(yintercept = 0, linetype = "solid", color = zero_line_color, linewidth = 0.9, alpha=0.7) +
  # Serie temporal de los residuos
  geom_line(color = resid_color, linewidth = 0.6, alpha = 0.8) +

  # Formato de ejes
  scale_y_continuous(labels = scales::percent_format(scale = 100, accuracy = 0.1),
                     name = "Residuos CAPM (Error Diario %)") +
  scale_x_date(date_breaks = "2 years", date_labels = "%Y") +

  # Aplicar tema Semana 5
  theme_week5_uncertainty(base_family = "Lato", base_size = 10) +

  # Etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Fecha",
    caption = caption_day27
  ) +
  theme(legend.position = "none")


# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_27_noise_capm_adf.png")
ggsave(
  filename = output_file,
  plot = gg_day27,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col 
)

message("Gráfico del Día 27 (Noise CAPM Residuals) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_27_noise_capm_adf.R ---
# --- Day 6: Florence Nightingale (Coxcomb Plot - Monthly Housing Sales) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: INE - ETDP)

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(stringr)    # Para extraer mes/año
library(lubridate)  # Para nombres de mes
library(forcats)    # Para manejar factores
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(patchwork)
library(seasonal)

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"
# Archivo con los datos de transacciones
trans_file <- "R/30DayChartChallenge2025/data/trans_prop_vivienda.csv"

# Verificar archivos...
if (!file.exists(utils_file)) stop("Archivo de utilidades no encontrado.")
if (!file.exists(config_file)) stop("Archivo de configuración no encontrado.")
if (!file.exists(trans_file)) stop("Archivo de datos trans_prop_vivienda.csv no encontrado.")

source(themes_file)
source(utils_file)
config <- read_yaml(config_file)


# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa) # Carga Roboto

# --- 4. Cargar y Preparar Datos ---

dt_trans <- fread(trans_file, sep = ";", dec = ",", na.strings = '""', encoding = "UTF-8", header = TRUE)
# Limpiar nombres (ajusta si es necesario)
original_names <- names(dt_trans)
new_names <- c("Ambito1", "Ambito2", "Ambito3", "RegimenEstado", "Periodo", "Total")
if (length(original_names) == length(new_names)) setnames(dt_trans, new_names)

# Total como entero
dt_trans[, Total := as.integer(gsub("\\.", "", Total))]

# Extraer Año y Mes
dt_trans[, Year := as.integer(str_extract(Periodo, "[0-9]{4}"))]
dt_trans[, Month_Num := as.integer(str_extract(Periodo, "[0-9]{2}$"))]

# Filtrar datos y ordenar cronológicamente
dt_sales <- dt_trans[Ambito1 == "Total Nacional" & RegimenEstado == "Viviendas: Total" & !is.na(Total)]
setorder(dt_sales, Year, Month_Num)

# --- 5. Descomposición Estacional ---

# Crear objeto de serie temporal (ts)
# Necesitamos saber el primer año y mes para 'start'
start_year <- min(dt_sales$Year)
start_month <- dt_sales[Year == start_year, min(Month_Num)]
sales_ts <- ts(dt_sales$Total, start = c(start_year, start_month), frequency = 12)

# Seasonal Adjustment with X-13ARIMA-SEATS
seas_result <- seas(sales_ts, x11 = "") # Usamos x11="" para obtener componentes estándar
message("Ajuste estacional completado.")

# Extraer los factores estacionales finales (serie d10 - multiplicativos)
# Estos factores están centrados alrededor de 100
seasonal_factors_ts <- seasonal::series(seas_result, "d10")

# Crear data table con los factores y la fecha/mes
# Necesitamos las fechas correspondientes a la serie 'seasonal_factors_ts'
dates_ts <- seq.Date(from = as.Date(paste0(start_year,"-",start_month,"-01")),
                     length.out = length(seasonal_factors_ts), by = "month")

dt_seasonal <- data.table(
  Date = dates_ts,
  Seasonal_Factor = as.numeric(seasonal_factors_ts) # Convertir ts a numeric
)
dt_seasonal[, Month_Num := month(Date)]

# Calcular el FACTOR estacional PROMEDIO para cada mes
# (Estará centrado alrededor de 100)
dt_avg_seasonal <- dt_seasonal[, .(Avg_Seasonal_Factor = mean(Seasonal_Factor, na.rm=TRUE)), by = Month_Num]

# Crear factor de Mes ordenado Ene-Dic para el gráfico
month_labels_es_short <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
dt_avg_seasonal[, Month_Factor := factor(Month_Num, levels = 1:12, labels = month_labels_es_short)]
setorder(dt_avg_seasonal, Month_Num)
dt_avg_seasonal[, Avg_Seasonal_Factor := Avg_Seasonal_Factor * 100]

# Verificar
print("Factor Estacional Promedio (Base 100) por Mes:")
print(dt_avg_seasonal)


# --- 5. Crear GRÁFICO BASE (Coxcomb sin títulos/caption) ---

gg_coxcomb <- ggplot(dt_avg_seasonal, aes(x = Month_Factor, y = Avg_Seasonal_Factor), alpha = .8) +
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
  geom_col(aes(fill = Avg_Seasonal_Factor), width = 1, color = NA, show.legend = FALSE) +
  scale_fill_gradient2(low = paleta_week1[4], mid = "white", high = paleta_week1[5], midpoint = 100) +
  coord_polar(start = -pi/12) +
  scale_x_discrete(name = NULL) +
  scale_y_continuous(name = NULL, breaks = pretty_breaks(n=3), labels = NULL,
                     limits = c(
                      0,
                      120),
                     expand = c(0.25, 0)) +
  # Tema súper limpio solo para el coxcomb
  theme_void(base_family = "Roboto", base_size = 11) +
  theme(
    axis.text.x = element_text(color = "#000000", size = rel(1.4)), 
    panel.grid.major.y = element_line(color="#b4a08c", linewidth=0.3), # Rejilla circular
    plot.margin = margin(5, 5, 5, 5), # Margen pequeño alrededor del coxcomb,
    plot.background = element_rect(fill = "#c8baab", color = NA),
    panel.background = element_rect(fill = "#c8baab", color = NA)
  )

# --- 6. Definir Textos y Caption ---
source_text_day6 <- "INE. Serie 2007-2024. Ajuste X-13ARIMA-SEATS." 
plot_title <- "Estacionalidad Promedio de la Compraventa de Viviendas"
plot_subtitle <- "Factores estacionales promedio (Base 100 = Media Anual) para España, 2007-2024.\nValores > 100 indican meses con ventas típicamente superiores a la media."
caption_day6 <- generate_caption(day = 6, source_text = source_text_day6, config = config)

# --- 7. Ensamblar con Patchwork y Añadir Anotaciones ---

# Colores y familias de fuente para anotaciones
title_font <- "Baumans"
base_font <- "Roboto"
text_col_main <- "#000000"
caption_col <- "#373737" # Usar el color secundario del caption
bg_col <- "#c8baab"

# Ensamblar usando plot_annotation
gg_final <- gg_coxcomb +
  plot_annotation(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = caption_day6,
    theme = theme(
      # Tema para los elementos añadidos por patchwork
      plot.title = element_text(family = title_font, size = rel(3), face = "bold", hjust = 0.5, margin = margin(b = 5)),
      plot.subtitle = element_text(family = base_font, size = rel(1.8), color = "grey30", hjust = 0.5, margin = margin(b = 20)), # Más margen bajo subtítulo
      plot.caption = element_markdown(family = base_font, color = caption_col, size = rel(1.4), lineheight = 1.0, hjust = 0.5, margin = margin(t = 15, b=5)),
      plot.background = element_rect(fill = bg_col, color = NA), # Fondo general
      plot.margin = margin(15, 15, 10, 15) # Margen exterior del conjunto
    )
  )

# --- 8. Guardar Gráfico Ensamblado ---
output_file <- file.path(output_path, "day_6_nightingale_sales_patchwork.png") # Nuevo nombre
ggsave(
  filename = output_file,
  plot = gg_final, # Guardar el objeto patchwork final
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col # Ajustar altura/bg
)

message("Gráfico del Día 6 (Coxcomb Patchwork) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_6.R ---
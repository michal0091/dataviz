# --- Day 1: Fractions (Régimen Tenencia Vivienda por Edad) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: INE)

# --- 1. Cargar Librerías ---
library(ggplot2)
library(dplyr)
library(data.table) # Usaremos fread por eficiencia y manejo de decimales/NA
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
data_file <- "R/30DayChartChallenge2025/data/day_1.csv" 

# Verificar archivos
if (!file.exists(themes_file)) stop("Archivo de temas no encontrado.")
if (!file.exists(utils_file)) stop("Archivo de utilidades no encontrado.")
if (!file.exists(config_file)) stop("Archivo de configuración no encontrado.")
if (!file.exists(data_file)) stop("Archivo de datos day_1.csv no encontrado en data/.")

source(themes_file)
source(utils_file)
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa) # Verifica ruta a fa-brands!

# --- 4. Cargar y Preparar Datos ---

# Leer el CSV descargado del INE
dt <- fread(data_file, sep = ";", dec = ",", na.strings = c('""', ".."), encoding = "Latin-1")

# Limpiar nombres de columnas 
original_names <- names(dt)
new_names <- c("Sexo", "Edad", "Tenencia", "Periodo", "Total")
setnames(dt, old = original_names, new = new_names)

# Filtrar año 2024 y regímenes relevantes
dt_2024 <- dt[Periodo == 2024 & Tenencia %in% c("Propiedad", "Propiedad con hipoteca", "Propiedad sin hipoteca")]

# Seleccionar columnas necesarias y pivotar para tener el % por tipo de tenencia
dt_prop <- dt_2024[Tenencia %in% c("Propiedad con hipoteca", "Propiedad sin hipoteca"),
                   .(Edad, Tenencia, Total)]

# Calcular el porcentaje de "Alquiler u otros" (100 - % Propiedad Total)
dt_prop_total <- dt_2024[Tenencia == "Propiedad", .(Edad, Total_Propiedad = Total)]
dt_calc <- merge(dt_prop, dt_prop_total, by = "Edad", all = TRUE)
dt_calc[, Pct_Alquiler_Otros := 100 - Total_Propiedad]

# Combinar y dar formato final para ggplot
dt_plot <- rbindlist(list(
  dt_calc[!is.na(Tenencia), .(Edad, Tenencia, Total)],
  unique(dt_calc[, .(Edad, Tenencia = "Alquiler u otros", Total = Pct_Alquiler_Otros)])
))

# Renombrar grupos de edad (opcional) y ordenar factores
dt_plot[, Generacion := factor(Edad,
                               levels = c("De 16 a 29 años", "De 30 a 44 años", "De 45 a 64 años", "65 y más años"),
                               labels = c("16-29 años\n(Gen Z / Mill.)", "30-44 años\n(Mill. / Gen X)", "45-64 años\n(Gen X / Boomer)", "65+ años\n(Boomer / Sil.)"))]

# Ordenar factor de Tenencia para el gráfico apilado
dt_plot[, Tenencia := factor(Tenencia, levels = c("Alquiler u otros", "Propiedad con hipoteca", "Propiedad sin hipoteca"))]

# Convertir Total a numérico (si fread no lo hizo ya por el dec=",")
dt_plot[, Total := as.numeric(Total)]

# --- 5. Crear Gráfico ---

# Definir textos
source_text_day1 <- "INE: Indicadores de Calidad de Vida (2024)"
plot_title <- "Régimen de Tenencia de Vivienda\npor Edad en España (2024)"
plot_subtitle <- "Fracción de la población en cada grupo de edad\nsegún el tipo de tenencia de su vivienda principal"

# Generar caption
caption_day1 <- generate_caption(
  day = 1,
  source_text = source_text_day1,
  config = config
)

# Crear el objeto ggplot
gg_day1 <- ggplot(dt_plot, aes(x = Generacion, y = Total, fill = Tenencia)) +
  geom_col(position = "fill") + 
  # Aplicar paleta personalizada de la semana 1
  scale_fill_challenge(palette = "week1", discrete = TRUE, # Usaremos los 3 primeros colores
                       reverse = TRUE, # Invertir orden de colores
                       guide = guide_legend(reverse = FALSE)) + 
  # Formatear eje Y como porcentaje
  scale_y_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
  # Aplicar tema personalizado (ajusta base_size si es necesario)
  theme_week1(base_size = 14) +
  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Grupo de Edad (Generación Aproximada)",
    y = "Porcentaje de Población",
    fill = NULL, # QUitar el título de leyenda
    caption = caption_day1
  ) +
  theme(axis.text.x = element_text(angle = 0, hjust = 0.5, size = rel(0.9))) # Ajustar texto eje x

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_1_fractions_tenencia.png") 
ggsave(
  filename = output_file,
  plot = gg_day1,
  width = 1200, height = 1200, units = "px", dpi = 300, bg = "white"
)

message("Gráfico del Día 1 (Tenencia Vivienda) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_1.R ---
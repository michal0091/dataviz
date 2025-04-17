# --- Day 19: Smooth (Polish Presidential Polls Trends) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: User CSV - Polish Election Polls 2025)
# Evolución suavizada de la intención de voto para candidatos presidenciales
# en Polonia, incluyendo orientación política.

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(lubridate)
library(viridis) 

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"

# Verificar existencia de archivos y cargar
source(themes_file) # Carga paletas y theme_week4_social
source(utils_file)  # Carga caption y setup_fonts (con "Lato")
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Cargar y Preparar Datos de Encuestas Polonia ---

data_file <- "R/30DayChartChallenge2025/data/elecciones_polonia_2025.csv"
if (!file.exists(data_file)) stop("Archivo CSV de encuestas no encontrado: ", data_file)

message("Cargando datos desde: ", data_file)
# Leer CSV especificando separador y decimal
polls_dt <- data.table::fread(data_file, sep = ";", dec = ",", header = TRUE)
message("Datos cargados. ", nrow(polls_dt), " filas iniciales.")
print(head(polls_dt))

# --- Limpieza y Preparación ---

# Convertir fecha y apoyo
polls_dt[, fecha := lubridate::as_date(fecha)]
polls_dt[, apoyo_pct := as.numeric(apoyo) * 100] 

# Limpiar nombres y orientación (quitar espacios extra)
cols_to_trim <- c("candidato", "partido", "orientación")
polls_dt[, (cols_to_trim) := lapply(.SD, trimws), .SDcols = cols_to_trim]

# Crear etiqueta combinada para leyenda
polls_dt[, label_legend := paste0(candidato, " (", orientación, ")")]


# Filtrar NAs generados y rango de fechas si es necesario
cols_check_na <- c("fecha", "apoyo_pct", "label_legend")
polls_clean_dt <- na.omit(polls_dt, cols = cols_check_na)

# Seleccionar 6 candidatos con más apoyo medio
top_6_candidatos <- polls_clean_dt[, .(apoyo_medio = mean(apoyo_pct, na.rm = TRUE)), label_legend][order(-apoyo_medio), head(label_legend, 6)]
polls_clean_dt <- polls_clean_dt[label_legend %in% top_6_candidatos]

# Convertir a factor para ordenar leyenda 
polls_clean_dt[, label_legend := factor(label_legend, levels = top_6_candidatos)]

message("Datos limpios y preparados: ", nrow(polls_clean_dt), " registros válidos.")
message("Rango de fechas: ", min(polls_clean_dt$fecha), " a ", max(polls_clean_dt$fecha))
message("Candidatos (Orientación) encontrados: ", paste(levels(polls_clean_dt$label_legend), collapse=", "))

# --- 5. Crear Gráfico de Tendencias Suavizadas ---

# Definir textos
source_text_day19 <- "Fuente: Diversas encuestadoras (elaboración propia)"
plot_title <- "Evolución Intención de Voto: Presidenciales Polonia 2025"
plot_subtitle <- paste0("Tendencias suavizadas (LOESS/GAM) basadas en encuestas publicadas.\nPeriodo: ",
                       format(min(polls_clean_dt$fecha), "%b %Y"), " - ", format(max(polls_clean_dt$fecha), "%b %Y"))

# Generar caption
caption_day19 <- generate_caption(
  day = 19, 
  source_text = source_text_day19,
  config = config,
  color_text_source = "#5c5c5c",
  color_text_author = "#757de8"
)

# Colores y tema
bg_col <- "#F2F2F2"
text_col <- challenge_palettes$week4_social['grey']
num_candidatos <- length(levels(polls_clean_dt$label_legend))

# Crear el gráfico
gg_day19 <- ggplot(polls_clean_dt, aes(x = fecha, y = apoyo_pct, color = label_legend, group = label_legend)) +

  # Opcional: Puntos de encuestas individuales (muy transparentes)
  geom_point(alpha = 0.3, size = 2, shape = 19, show.legend = FALSE) +

  # Líneas suavizadas (método por defecto LOESS/GAM)
  geom_smooth(se = FALSE, linewidth = 1.2, span = 0.95) +

  # Escala de color
  scale_color_manual(values = challenge_pal("week4_social")(num_candidatos), name = NULL) +

  # Formato de ejes
  scale_y_continuous(labels = scales::label_percent(scale = 1), limits = c(0, 40), expand = expansion(mult=c(0, 0.05))) +
  scale_x_date(date_breaks = "1 month", date_labels = "%B %Y") + # Ajusta breaks según rango real

  # Aplicar tema
  theme_week4_social(base_family = "Lato", base_size = 11) + # Usar tema semana 4

  # Leyenda
  guides(color = guide_legend(override.aes = list(alpha = 1, linewidth=2), ncol = 3)) + # Leyenda más clara y en 2 cols si es larga

  # Etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Fecha de Encuesta",
    y = "Apoyo Estimado (%)",
    caption = caption_day19
  ) +
  theme(legend.title.align = 0.5) # Centrar título leyenda

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_19_smooth_polls_poland.png")
ggsave(
  filename = output_file,
  plot = gg_day19,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col 
)

message("Gráfico del Día 19 (Smooth Polls) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_19_smooth_polls.R ---
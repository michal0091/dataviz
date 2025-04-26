# --- Day 28: Inclusion (Unemployment Rate Disparity & Uncertainty by Spanish Region) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: INE - Encuesta de Población Activa)
# Tasa de paro estimada por CCAA incluyendo Intervalo de Confianza del 95%
# para visualizar disparidad territorial e incertidumbre de la estimación.

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
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

# --- 4. Cargar y Preparar Datos EPA por CCAA ---

data_file <- "R/30DayChartChallenge2025/data/epa_paro_ccaa.csv"

if (!file.exists(data_file)) {
  stop("Archivo CSV de EPA por CCAA no encontrado en: ", data_file,
       "\nDescárgalo desde INE.es o busca la publicación adecuada.")
}

message("Cargando datos EPA desde: ", data_file)
epa_dt <- data.table::fread(data_file, encoding = "Latin-1") # Ajusta sep, dec, skip si es necesario
message("Datos EPA cargados. ", nrow(epa_dt), " filas iniciales.")
print(head(epa_dt))
print(names(epa_dt)) 


# --- Limpieza y Preparación ---
setnames(epa_dt,
  old = c("Edad", "Sexo", "Comunidades y Ciudades Autónomas", "Periodo", "Total"),
  new = c("Edad_Grp", "Sexo_Grp", "CCAA_Raw", "Periodo_Str", "Tasa_Paro"),
  skip_absent=TRUE)

# Filtrar datos necesarios
epa_clean_dt <- epa_dt[Edad_Grp == "Total" & Sexo_Grp == "Ambos sexos"]

# Limpiar nombre CCAA (quitar código inicial)
epa_clean_dt[, CCAA := trimws(sub("^[0-9]{2} ", "", CCAA_Raw))]

# Convertir Periodo a Fecha (inicio del trimestre)
epa_clean_dt[, Date := as.Date(zoo::as.yearqtr(Periodo_Str, format = "%YT%q"))]

# Asegurar que Tasa_Paro es numérica (ya debería serlo si dec="," funcionó)
epa_clean_dt[, Tasa_Paro := as.numeric(Tasa_Paro)]

# Seleccionar columnas finales y quitar NAs
epa_clean_dt <- epa_clean_dt[!is.na(Date) & !is.na(Tasa_Paro) & !is.na(CCAA),
  .(CCAA, Date, Rate = Tasa_Paro)]

# Corregir algunos nombres de CCAA
epa_clean_dt[, CCAA := fcase(
  CCAA == "Madrid, Comunidad de", "Madrid",
  CCAA == "Murcia, Región de", "Murcia",
  CCAA == "Navarra, Comunidad Foral de", "Navarra",
  CCAA == "Rioja, La", "La Rioja",
  CCAA == "Asturias, Principado de", "Asturias",
  CCAA == "Balears, Illes", "Balears",
  CCAA == "Total Nacional", "España",
  default = CCAA
)]

# --- Seleccionar CCAA para visualizar  ---
ccaa_seleccionadas <- c(
"Andalucía", "Navarra", "Madrid", "País Vasco", "Castilla y León", "España"
)
epa_plot_dt <- epa_clean_dt[CCAA %in% ccaa_seleccionadas]
# Convertir a factor para leyenda consistente
epa_plot_dt[, CCAA := factor(CCAA, levels = intersect(ccaa_seleccionadas, unique(CCAA)))]


message("Datos EPA listos para graficar para: ", paste(levels(epa_plot_dt$CCAA), collapse=", "))
print(tail(epa_plot_dt))

# --- 5. Crear Gráfico de Líneas Comparativo ---

# Definir textos
epa_periodo_plot <- paste(min(year(epa_plot_dt$Date)), "-", max(year(epa_plot_dt$Date)))
source_text_day28 <- paste0("Fuente: INE - Encuesta de Población Activa (EPA). Periodo: ", epa_periodo_plot)
plot_title <- "Evolución de la Disparidad Territorial en el Paro (España)"
plot_subtitle <- paste0("Tasa de paro trimestral estimada por CC.AA. seleccionadas.")

# Generar caption
caption_day28 <- generate_caption(
  day = 28,
  source_text = source_text_day28,
  config = config,
  color_text_source = "#000000",
  color_text_author = "#455A64"
)

# Colores y tema
bg_col <- "#FFFFFF"
num_ccaa <- length(levels(epa_plot_dt$CCAA))
# Usar paleta week4_social o week5_uncertainty
plot_colors <- challenge_pal("week4_social")(num_ccaa) # O "week5_uncertainty"
names(plot_colors) <- levels(epa_plot_dt$CCAA)

# Cambiar color para España
plot_colors["España"] <- "#DE1D1A"

# Line width
l_width <- rep(1.1, num_ccaa)
names(l_width) <- levels(epa_plot_dt$CCAA)
l_width["España"] <- 1.4

# Crear el gráfico de líneas
gg_day28 <- ggplot(epa_plot_dt, aes(x = Date, y = Rate, color = CCAA, linewidth = CCAA, group = CCAA)) +
geom_line(alpha = 0.85) +

# Escala de colores manual
scale_color_manual(values = plot_colors, name = NULL) + 
scale_linewidth_manual(values = l_width, name = NULL) +

# Formato de ejes
scale_y_continuous(labels = scales::label_percent(scale = 1),
name = "Tasa de Paro Estimada (%)") +
scale_x_date(date_breaks = "3 years", date_labels = "%Y", 
expand = expansion(mult=c(0.01, 0.01))) +

# Aplicar tema Semana 5 
theme_week5_uncertainty(base_family = "Lato", base_size = 10) +

# Leyenda
guides(color = guide_legend(nrow = 1)) + # Ajustar columnas

# Etiquetas y caption
labs(
title = plot_title,
subtitle = plot_subtitle,
x = "Año",
caption = caption_day28
) +
theme(legend.title = element_blank()) # Confirmar leyenda sin título

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_28_inclusion_epa_ts.png")
ggsave(
filename = output_file,
plot = gg_day28,
width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col 
)

message("Gráfico del Día 28 (Inclusion EPA TS) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_28_inclusion_epa_ts.R ---
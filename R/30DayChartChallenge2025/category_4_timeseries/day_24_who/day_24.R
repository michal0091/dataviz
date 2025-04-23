# --- Day 24: WHO (data day) - DTP3 Vaccination Coverage in Spain ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: WHO Global Health Observatory)
# Evolución de la cobertura de vacunación con DTP3 en niños de 1 año por grupo de ingresos del Banco Mundial.

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
source(themes_file)
source(utils_file)
config <- read_yaml(config_file)


# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa) # Carga "Lato"

# --- 4. Cargar y Preparar Datos de Cobertura Vacunal OMS ---

data_url <- "https://srhdpeuwpubsa.blob.core.windows.net/whdh/DATADOT/INDICATOR/F8E084C_ALL_LATEST.csv"

message("Cargando datos de cobertura vacunal desde: ", data_url)
vacc_dt <- data.table::fread(data_url)

if (nrow(vacc_dt) == 0) {
  stop("Problema en la lectura de los datos, revisa la URL.")
}
message("Datos cargados. ", nrow(vacc_dt), " filas iniciales.")


# --- Limpieza y Preparación ---
col_geo_name <- "GEO_NAME_SHORT"
col_geo_type <- "DIM_GEO_CODE_TYPE"
col_year <- "DIM_TIME" 
col_coverage <- "RATE_PER_100_N"

# Verificar que las columnas existen
required_cols <- c(col_geo_name, col_geo_type, col_year, col_coverage)
if (!all(required_cols %in% names(vacc_dt))) {
    stop("El archivo CSV de WHO no contiene las columnas esperadas ('", col_year, "', '", col_coverage,"'). Revisa nombres.")
}

# Seleccionar, renombrar y asegurar tipos
vacc_clean_dt <- vacc_dt[, .(
    Country = as.character(get(col_geo_name)),
    Geo_Type = as.character(get(col_geo_type)),
    Year = as.numeric(get(col_year)),
    Coverage_Pct = as.numeric(get(col_coverage)) # Ya debería ser % (0-100)
)]

# Filtrar NAs y ordenar
vacc_clean_dt <- na.omit(vacc_clean_dt)
setorder(vacc_clean_dt, Geo_Type, Country, Year)

# Subset zona de interés 
selected_geo_type <- "WORLDBANKINCOMEGROUP"
vacc_clean_dt <- vacc_clean_dt[Geo_Type  == selected_geo_type]

# Traducir grupos de ingresos
vacc_clean_dt[, Income_Group := fcase(
  Country == "Low-income economies", "Bajo",
  Country == "Lower-middle-income economies", "Medio-Bajo",
  Country == "Upper-middle-income economies", "Medio-Alto",
  Country == "High-income economies", "Alto"
)]

# Income_Group como factor ordenado
vacc_clean_dt[, Income_Group := factor(Income_Group, levels = c("Bajo", "Medio-Bajo", "Medio-Alto", "Alto"))]

message("Datos de vacunación preparados. Rango: ", min(vacc_clean_dt$Year), "-", max(vacc_clean_dt$Year))
print(tail(vacc_clean_dt))
print(summary(vacc_clean_dt))

# --- 5. Crear Gráfico de Líneas (Cobertura Vacunal) ---

# Definir textos
source_text_day24 <- "Fuente: WHO/UNICEF Estimates (via WHO Global Health Observatory)"
plot_title <- "Cobertura Vacunal con DTP3 por grupo de ingresos del Banco Mundial"
plot_subtitle <- paste0("Porcentaje estimado de niños/as de 1 año vacunados con 3 dosis de DTP.\nPeriodo: ",
                       min(vacc_clean_dt$Year), "-", max(vacc_clean_dt$Year), ". Línea objetivo: 95%.")

# Generar caption
caption_day24 <- generate_caption(
  day = 24,
  source_text = source_text_day24, 
  config = config, 
  color_text_source = "#5c5c5c",
  color_text_author = "#757de8"
)

# Colores y tema
bg_col <- "#F2F2F2"
num_groups <- length(levels(vacc_clean_dt$Income_Group))
color_map_income_group <- setNames(
    challenge_pal("week4_social")(num_groups), # Usa tu paleta social
    levels(vacc_clean_dt$Income_Group)     # Asigna a los niveles del factor
)
target_color <- challenge_palettes$week4_social['pink'] # Rosa para línea objetivo

# Crear el gráfico de línea
gg_day24 <- ggplot(vacc_clean_dt, aes(x = Year, y = Coverage_Pct, color = Income_Group, group = Income_Group)) +

  # Línea objetivo (ej. 95%)
  geom_hline(yintercept = 95, linetype = "dashed", color = target_color, linewidth = 1, alpha=0.8) +
  annotate("text", x = min(vacc_clean_dt$Year), y = 96, label = "Objetivo 95%",
           hjust = 0, vjust = 0, size = 4.5, color = target_color, family="Lato", fontface="italic") +

  # Línea de cobertura
  geom_line(linewidth = 1.3, alpha = 0.9) +
  geom_point(size = 2, shape=21, fill=bg_col, stroke=0.6) +

  # Escala de colores manual
  scale_color_manual(values = color_map_income_group, name = "Grupo de Ingresos") +

  # Formato de ejes
  scale_y_continuous(limits = c(0, 100), # Eje Y de 0 a 100%
                     labels = scales::label_percent(scale = 1),
                     expand = expansion(mult=c(0, 0.05)),
                     name = "Cobertura Estimada (%)") +
  scale_x_continuous(breaks = seq(min(vacc_clean_dt$Year), max(vacc_clean_dt$Year), 3)) + 

  # Aplicar tema
  theme_week4_social(base_family = "Lato", base_size = 10) +

  # Etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Año",
    # Y ya definido en scale_y_continuous
    caption = caption_day24
  ) +
  theme(legend.title = element_text(hjust = 0.5))

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_24_who_vaccination_esp.png")
ggsave(
  filename = output_file,
  plot = gg_day24,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col # Apaisado
)

message("Gráfico del Día 24 (WHO Vaccination) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_24_who_vaccination ---
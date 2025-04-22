# --- Day 22: Stars (Confirmed Exoplanet Discoveries Over Time) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: NASA Exoplanet Archive)
# Número acumulado de exoplanetas confirmados descubiertos por año.

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

# --- 4. Cargar y Preparar Datos de Exoplanetas ---

data_file <- "R/30DayChartChallenge2025/data/nasa_exoplanets_confirmed.csv"

if (!file.exists(data_file)) {
  stop("Archivo CSV de exoplanetas no encontrado en: ", data_file,
       "\nDescárgalo desde https://exoplanetarchive.ipac.caltech.edu/ (Confirmed Planets Table).")
}

message("Cargando datos de exoplanetas desde: ", data_file)
exoplanet_dt <- fread(data_file, skip = 99, header = TRUE)

message("Datos cargados. ", nrow(exoplanet_dt), " planetas confirmados iniciales.")
print(head(exoplanet_dt))
print(names(exoplanet_dt)) # <- ¡Verifica el nombre exacto de la columna del año!

# --- Procesamiento ---
# Asumiendo que la columna se llama 'disc_year' 
col_year <- "disc_year"
if (!col_year %in% names(exoplanet_dt)) {
    # Intenta con otros nombres comunes si falla
    alt_col_year <- names(exoplanet_dt)[grep("discoveryyear|disc_year", names(exoplanet_dt), ignore.case = TRUE)][1]
    if(!is.na(alt_col_year)) {
        warning("Columna '", col_year, "' no encontrada, usando '", alt_col_year, "' en su lugar.")
        col_year <- alt_col_year
    } else {
        stop("No se encontró una columna con el año de descubrimiento. Revisa el CSV.")
    }
}


# Asegurar que el año es numérico y válido
exoplanet_dt[, Year := as.numeric(get(col_year))]
exoplanet_clean_dt <- exoplanet_dt[!is.na(Year) & Year > 0] # Filtrar años inválidos

# Contar descubrimientos por año
discoveries_per_year <- exoplanet_clean_dt[, .N, by = .(Year)][order(Year)]

# Calcular el acumulado
discoveries_per_year[, Cumulative_Discoveries := cumsum(N)]

message("Datos agregados por año. Rango: ", min(discoveries_per_year$Year), "-", max(discoveries_per_year$Year))
print(tail(discoveries_per_year))

# --- 5. Crear Gráfico de Líneas (Acumulado Exoplanetas) ---

# Definir textos
source_text_day22 <- "Fuente: NASA Exoplanet Archive (exoplanetarchive.ipac.caltech.edu)"
plot_title <- "La Explosión en el Descubrimiento de Exoplanetas"
current_year <- year(Sys.Date())
plot_subtitle <- paste0("Número acumulado de exoplanetas confirmados descubiertos por año.\n",
                       "Nota: El dato de ", current_year, " incluye descubrimientos hasta la fecha actual (",
                       format(Sys.Date(), "%b %Y"), ").")


# Generar caption
caption_day22 <- generate_caption(
  day = 22,
  source_text = source_text_day22,
  config = config,
  color_text_source = "#5c5c5c",
  color_text_author = "#757de8"

)

# Colores y tema
bg_col <- "#F2F2F2"
# Usar un color de la paleta social (ej. el primario índigo o el acento rosa)
line_color <- challenge_palettes$week4_social['pink']

# Crear el gráfico de línea acumulada
gg_day22 <- ggplot(discoveries_per_year, aes(x = Year, y = Cumulative_Discoveries)) +
  geom_line(color = line_color, linewidth = 1.3) +
  geom_point(color = line_color, size = 1.5, shape = 21, fill = bg_col, stroke = 0.5) + # Puntos opcionales

  # Formato de ejes
  scale_y_continuous(labels = scales::label_comma(), expand = expansion(mult=c(0, 0.05))) + # Números grandes con comas
  scale_x_continuous() + # Ajusta breaks si es necesario

  # Aplicar tema
  theme_week4_social(base_family = "Lato", base_size = 10) +

  # Etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Año de Descubrimiento",
    y = "Número Acumulado de Exoplanetas Confirmados",
    caption = caption_day22
  ) +
  theme(legend.position = "none") 


# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_22_stars_exoplanets.png")
ggsave(
  filename = output_file,
  plot = gg_day22,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col # Apaisado
)

message("Gráfico del Día 22 (Stars Exoplanets) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_22_stars_exoplanets.R ---
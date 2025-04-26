# --- Day 29: Extraterrestrial (Exoplanet Radius vs. Insolation & Uncertainty) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: NASA Exoplanet Archive)
# Relación entre radio planetario e insolación recibida para exoplanetas confirmados,
# destacando la zona habitable aproximada y la incertidumbre inherente.

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

# --- 4. Cargar y Preparar Datos de Exoplanetas ---

# Ruta al archivo CSV descargado manualmente desde NASA Exoplanet Archive
data_file <- "R/30DayChartChallenge2025/data/nasa_exoplanets_confirmed.csv"

if (!file.exists(data_file)) stop("Archivo CSV de exoplanetas no encontrado: ", data_file)

message("Cargando datos de exoplanetas desde: ", data_file)
exoplanet_dt <- fread(data_file, skip = 99, header = TRUE)
message("Datos cargados. ", nrow(exoplanet_dt), " planetas confirmados iniciales.")

# --- Limpieza y Selección de Columnas (AÑADIR pl_eqt) ---
col_species <- "pl_name"     # Usar nombre de planeta
col_radius <- "pl_rade"      # Radio en radios terrestres
col_insol <- "pl_insol"      # Insolación relativa a la Tierra
col_temp <- "pl_eqt"         # Temperatura de Equilibrio [K]
col_error <- "pl_insolerr1"  # Error en insolación

required_cols <- c(col_species, col_radius, col_insol, col_temp) # Quitar col_method si no se usa
if (!all(required_cols %in% names(exoplanet_dt))) {
  stop("El archivo CSV no contiene todas las columnas esperadas. Revisa nombres.")
}

# Seleccionar, asegurar numérico
exo_clean_dt <- exoplanet_dt[, .(
  Planet = get(col_species),
  Radius_Earth = as.numeric(get(col_radius)),
  Insolation_Earth = as.numeric(get(col_insol)),
  Eq_Temp_K = as.numeric(get(col_temp)),
  Error_Insolation = as.numeric(get(col_error))
)]

# Filtrar NAs y valores no positivos
cols_to_check <- c("Radius_Earth", "Insolation_Earth", "Eq_Temp_K")
exo_clean_dt <- na.omit(exo_clean_dt, cols = cols_to_check)
exo_clean_dt <- exo_clean_dt[Radius_Earth > 0 & Insolation_Earth > 0 & Eq_Temp_K > 0]

# Aplicar Logaritmo (base 10) - NO a la Temperatura aún, se usa para color
exo_clean_dt[, log10_Radius := log10(Radius_Earth)]
exo_clean_dt[, log10_Insolation := log10(Insolation_Earth)]
exo_clean_dt[, log10_Insolation_error := fifelse(abs(Error_Insolation) < 1.01, 0.01, log10(abs(Error_Insolation) + 0.01))]

message("Número de exoplanetas después de limpiar: ", nrow(exo_clean_dt))

# --- Crear datos de referencia Sistema Solar ---
solar_system_ref <- data.table(
  Planet = factor(c("Venus", "Tierra", "Marte")),
  Radius_Earth = c(0.949, 1.000, 0.532),
  Insolation_Earth = c(1.911, 1.000, 0.431),
  Eq_Temp_K = c(227, 255, 210) # Temp equilibrio estimada SIN atmósfera densa
  # Nota: La Temp real de Venus es ~737K por efecto invernadero
)
# Calcular logs también para estos
solar_system_ref[, log10_Radius := log10(Radius_Earth)]
solar_system_ref[, log10_Insolation := log10(Insolation_Earth)]


print(solar_system_ref)


# --- 5. Crear Scatter Plot Mejorado (Radio vs Insolación, Color x Temp) ---

# Definir textos
source_text_day29 <- "Fuente: NASA Exoplanet Archive (Consultado Abr 2025)"
plot_title <- "Exoplanetas: Radio, Insolación y Temperatura"
plot_subtitle <- paste("Relación Radio vs. Insolación (log-log), coloreado por Temp. de Equilibrio (K).",
                       "Halos grises sugieren incertidumbre inherente en las mediciones.",
                       "\nBanda verde: Zona Habitable (insolación aprox.). Ref: Venus, Tierra, Marte.", sep="\n")

# Generar caption 
caption_day29 <- generate_caption(
  day = 29,
  source_text = source_text_day29,
  config = config, 
  color_text_source = "#000000",
  color_text_author = "#455A64"
)

# Colores y tema
bg_col <- "#FFFFFF"
temp_palette_option = "plasma" 
hab_zone_color <- alpha("#6B8E23", 0.15)
solar_system_color <- "black"
solar_system_fill <- "white"
halo_color <- "grey70"


min_x_radius <- min(exo_clean_dt$Radius_Earth)
max_x_radius <- max(exo_clean_dt$Radius_Earth)
# Usaremos estos límites exactos; ggplot expandirá un poco los ejes igualmente.
xmin_rect_hz <- min_x_radius
xmax_rect_hz <- max_x_radius

# Cargar ggrepel si no está cargado
library(ggrepel)

# Crear el gráfico
gg_day29 <- ggplot() +  # Empezar lienzo vacío para añadir capas

  # 1. Banda Zona Habitable
  annotate("rect",
           xmin = xmin_rect_hz,  
           xmax = xmax_rect_hz,  
           ymin = hz_lower_insol,
           ymax = hz_upper_insol,
           fill = hab_zone_color, alpha = 0.75) +

  # 2a. Puntos de "Halo" 
  geom_point(data = exo_clean_dt,
    aes(x = Radius_Earth, y = Insolation_Earth, size = log10_Insolation_error),         
    color = halo_color,
    alpha = 0.2) +       

  # 2b. Puntos Principales 
  geom_point(data = exo_clean_dt,
      aes(x = Radius_Earth, y = Insolation_Earth, color = Eq_Temp_K),
      alpha = 0.7, 
      size = 1.8) +  

  # 3. Puntos Sistema Solar 
  geom_point(data = solar_system_ref,
             aes(x = Radius_Earth, y = Insolation_Earth),
             shape = 23, fill = solar_system_fill, color = solar_system_color, size = 3.5, stroke=1) +

  # 4. Etiquetas Sistema Solar
  geom_text_repel(data = solar_system_ref,
                  aes(x = Radius_Earth, y = Insolation_Earth, label = Planet),
                  family = "Lato", color = solar_system_color, size = 5,
                  nudge_y = 0.2, 
                  nudge_x = 0.05,
                  segment.color = "grey50", segment.size=0.3,
                  bg.color = alpha(bg_col, 0.7), bg.r = 0.1) + # Fondo semitransparente

  # Escalas Logarítmicas
  scale_x_log10(
      name = "Radio Planetario (Radios Terrestres) [Escala Log]",
      breaks = c(0.1, 0.3, 1, 3, 10, 30),
      labels = label_number(accuracy = 0.1)
  ) +
  scale_y_log10(
      name = "Flujo de Insolación (relativo a la Tierra) [Escala Log]",
      breaks = c(0.01, 0.1, 1, 10, 100, 1000, 10000), 
      labels = label_log()
  ) +

  # Escala de color 
  scale_color_viridis_c(option = temp_palette_option, name = "Temp. Equilibrio (K)") +
  
  # Quitar escala de tamaño
  scale_size(guide = "none") +

  # Aplicar tema Semana 5
  theme_week5_uncertainty(base_family = "Lato", base_size = 10) +

  # Leyenda (ajustar)
  guides(color = guide_colorbar(title.position = "top", title.hjust = 0.5, barwidth = unit(10, "lines"))) + # Barra de color más ancha

  # Etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = caption_day29
  )

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_29_extraterrestrial_exohab_temp.png") # Nuevo nombre
ggsave(
  filename = output_file,
  plot = gg_day29,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col
)

message("Gráfico del Día 29 (Extraterrestrial ExoHab Temp) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_29_extraterrestrial_exohab_temp.R ---
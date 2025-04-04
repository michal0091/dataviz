# --- Day 4: Big or Small (Precio Vivienda Distritos Madrid) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Idealista])

# --- 1. Cargar Librerías ---
library(ggplot2)
library(dplyr)
library(sf)        # Para trabajar con datos espaciales
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

if (!file.exists(utils_file)) stop("Archivo de temas no encontrado.")
if (!file.exists(utils_file)) stop("Archivo de utilidades no encontrado.")
if (!file.exists(config_file)) stop("Archivo de configuración no encontrado.")

source(themes_file)
source(utils_file)
config <- read_yaml(config_file)
text_col_main <- "#000"

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)


source(themes_file)
source(utils_file)
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Preparar Datos ---

# --- 4.0 Descarga y Descompresión Automática (Opcional) ---

# Definir URL, archivo zip destino y directorio de extracción
zip_url <- "https://geoportal.madrid.es/fsdescargas/IDEAM_WBGEOPORTAL/LIMITES_ADMINISTRATIVOS/Distritos/Distritos.zip"
zip_destfile <- "R/30DayChartChallenge2025/data/Distritos.zip"     
unzip_exdir <- "R/30DayChartChallenge2025/data/Distritos" 
# Ruta esperada al archivo .shp DESPUÉS de descomprimir 
path_limites_distritos <- file.path(unzip_exdir, "Distritos.shp")

# Crear directorios si no existen
dir.create(dirname(zip_destfile), showWarnings = FALSE, recursive = TRUE) # Crea data/
dir.create(unzip_exdir, showWarnings = FALSE, recursive = TRUE)      # Crea data/Distritos/

# Comprobar si el archivo .shp ya existe para evitar re-descargar
if (!file.exists(path_limites_distritos)) {
  message("Archivo de límites no encontrado. Intentando descargar y descomprimir...")

  # Descargar el archivo zip
  tryCatch({
    message("Descargando ", basename(zip_url), "...")
    download.file(zip_url, destfile = zip_destfile, mode = "wb") # mode="wb" es importante para zips
    message("Descarga completa.")

    # Descomprimir el archivo
    message("Descomprimiendo en ", normalizePath(unzip_exdir, mustWork = FALSE), "...")
    unzip(zipfile = zip_destfile, exdir = unzip_exdir)
    message("Descompresión completa.")

    # Verificar si el .shp esperado ahora sí existe
    if (!file.exists(path_limites_distritos)) {
      stop("El archivo .shp esperado (", basename(path_limites_distritos), ") no se encontró después de descomprimir. Revisa el contenido del zip.")
    }

    # (Opcional) Borrar el archivo zip descargado después de descomprimir
     # file.remove(zip_destfile)

  }, error = function(e) {
    stop("Error durante la descarga o descompresión: ", e$message)
  })

} else {
  message("Archivo de límites ya existe en: ", normalizePath(path_limites_distritos, mustWork = FALSE))
}

# 4.1 Datos de Precios 
distritos <- c("Salamanca", "Chamberí", "Retiro", "Chamartín", "Centro", "Moncloa - Aravaca",
               "Arganzuela", "Tetuán", "Hortaleza", "Fuencarral - El Pardo", "Ciudad Lineal",
               "Barajas", "Moratalaz", "San Blas - Canillejas", "Latina", "Vicálvaro",
               "Carabanchel", "Villa de Vallecas", "Usera", "Puente de Vallecas", "Villaverde")
precios <- c(9374, 7677, 7014, 6988, 6816, 5500, 5341, 5162, 4745, 4572, 4219,
             4070, 3607, 3362, 3275, 3171, 3039, 3032, 2880, 2648, 2294) # Idealista Mar 2025
fuente_datos_precios <- "Idealista (Marzo 2025)"
fuente_datos_limites <- "Ayto. Madrid Geoportal" # Fuente de los límites

data_precios <- data.frame(
  NOMBRE = distritos,
  Precio_m2 = precios
)

# 4.2 Cargar Límites Geográficos de Distritos
path_limites_distritos <- "R/30DayChartChallenge2025/data/Distritos/Distritos.shp"

if (!file.exists(path_limites_distritos)) stop("Archivo Shapefile de límites de distritos no encontrado en la ruta especificada.")
map_distritos <- sf::st_read(path_limites_distritos)

# print(head(map_distritos))

# 4.3 Unir Datos Espaciales y de Precios
map_data <- dplyr::left_join(map_distritos, data_precios, by = "NOMBRE")

# Verificar si la unión introdujo NAs en Precio_m2 (indicaría nombres no coincidentes)
if(any(is.na(map_data$Precio_m2))) {
  warning("Algunos distritos no pudieron unirse con los datos de precios. Verifica los nombres en ambas fuentes.")
  print("Distritos sin precio asignado:")
  print(map_data[is.na(map_data$Precio_m2), "NOMBRE"]) # Ajusta "NOMBRE" si es diferente
}

# 4.4 Calcular Quintiles y Crear Variable Categórica
# Calcular los puntos de corte para 5 grupos (quintiles)
quintile_breaks <- quantile(map_data$Precio_m2, probs = seq(0, 1, 0.20), na.rm = TRUE)

# Asegurarse de que los breaks son únicos
quintile_breaks <- unique(quintile_breaks)
num_groups <- length(quintile_breaks) - 1
if (num_groups < 1) stop("No se pudieron calcular los quintiles, revisa los datos de precios.")

# Crear etiquetas para los quintiles (ej. "Q1 (Más Barato)", "Q2", ..., "Q5 (Más Caro)")
quintile_labels <- paste0("Q", 1:num_groups,
                       ifelse(1:num_groups == 1, " (Más Barato)",
                              ifelse(1:num_groups == num_groups, " (Más Caro)", "")))

# Crear la nueva columna asignando cada distrito a un quintil
map_data <- map_data %>%
  mutate(
    Precio_Quintil = cut(Precio_m2,
                         breaks = quintile_breaks,
                         labels = quintile_labels[1:num_groups], # Usar labels correctos si hay menos grupos
                         include.lowest = TRUE, # Incluir el valor mínimo en el primer intervalo
                         right = TRUE) # Intervalos (min, max]
  )


# --- 5. Crear Gráfico ---

# Definir textos
plot_title <- "Big vs Small: Precio Vivienda (€/m²) por Quintiles"
plot_subtitle <- "Distritos de Madrid (Marzo 2025) agrupados en 5 quintiles de precio medio"
fuente_completa <- paste0("Precios: ", fuente_datos_precios, " | Límites: ", fuente_datos_limites)

# Generar caption
caption_day4 <- generate_caption(
  day = 4,
  source_text = fuente_completa, # Fuente combinada
  config = config
)

# Crear el mapa coroplético
gg_day4 <- ggplot(map_data) +
  geom_sf(aes(fill = Precio_Quintil), color = "#c8baab", linewidth = 0.1)  + # Polígonos coloreados por quintiles
   scale_fill_manual(
    values = paleta_semana1[1:num_groups], # Usar los primeros 'num_groups' colores de la paleta week1
    name = "Quintil de Precio (€/m²)", # Título de la leyenda
    na.value = "#525252", # Color para distritos sin dato (si hubiera)
    guide = guide_legend(reverse = TRUE) 
  ) +
  theme_void(base_family = "Roboto", base_size = 11) + # Tema limpio para mapas
  theme(
      plot.background = element_rect(fill = config$defaults$plot_background %||% "#c8baab", color = NA),
      plot.title = element_text(family = "Baumans", size = rel(3.2), hjust = 0.5, margin = margin(t=10, b = 5), color = text_col_main),
      plot.subtitle = element_text(family = "Roboto", size = rel(2), hjust = 0.5, margin = margin(b = 15), color = text_col_main),
      plot.caption = element_markdown(color = text_col_main, size = rel(1.6), hjust = 0.5, halign=0.5, margin = margin(t = 10, b = 5), lineheight = 1.0),
      legend.position = "right", # Posición de la leyenda de color
      legend.title = element_text(color = text_col_main, size = rel(1.6)),
      legend.text = element_text(color = text_col_main, size = rel(1.6)),
      plot.margin = margin(15, 15, 10, 60)
  ) +
  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = caption_day4
  )

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_4_big_small_madrid_mapa.png") # Nuevo nombre
ggsave(
  filename = output_file,
  plot = gg_day4,
  width = 1200, height = 1200, units = "px", dpi = 300, bg = "#c8baab"
)

message("Gráfico del Día 4 (Mapa Precios Madrid) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_4.R ---

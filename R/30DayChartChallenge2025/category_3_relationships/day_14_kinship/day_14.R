# --- Day 14: Kinship (Animal Trait Similarity Dendrogram) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Kaggle - Animal Information Dataset by Sourav Banerjee)
# Visualización de la similitud entre especies animales basada en rasgos
# (Masa Corporal y Longevidad) mediante un dendrograma jerárquico.

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(stringr)
library(stats)      # Para dist() y hclust()
library(ggdendro)   # Para ggdendrogram()

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"

# Verificar existencia de archivos
if (!file.exists(themes_file)) stop("Archivo de temas no encontrado: ", themes_file)
if (!file.exists(utils_file)) stop("Archivo de utilidades no encontrado: ", utils_file)
if (!file.exists(config_file)) stop("Archivo de configuración no encontrado: ", config_file)

source(themes_file)
source(utils_file)
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa) # Asegura que "Cabin" se carga

# --- 4. Cargar y Preparar Datos ---

# --- Configuración de Archivos ---
data_file <- "R/30DayChartChallenge2025/data/Animals_information.csv" 
if (!file.exists(data_file)) {
  stop("Archivo de datos animales no encontrado en: ", data_file)
}

message("Cargando datos desde: ", data_file)
traits_dt <- data.table::fread(data_file)

# --- Limpieza y Selección ---
col_species <- "Animal"
col_grouping <- "Diet" 
col_mass <- "Weight (kg)"
col_longevity <- "Lifespan (years)"

required_cols <- c(col_species, col_grouping, col_mass, col_longevity)
if (!all(required_cols %in% names(traits_dt))) {
  stop("El archivo CSV no contiene todas las columnas esperadas para el gráfico. Faltan: ",
       paste(setdiff(required_cols, names(traits_dt)), collapse=", "))
}

# Seleccionar, renombrar y limpiar
traits_clean_dt <- traits_dt[, .(
  Species = get(col_species),
  Grouping = get(col_grouping), 
  Mass_kg = as.numeric(get(col_mass)),
  Longevity_yrs = as.numeric(get(col_longevity))
)]
traits_dt <- traits_dt[, .SD, .SDcols = required_cols]
setnames(traits_dt, required_cols, c("Species", "Grouping", "Mass_kg", "Longevity_yrs"))

# --- Función Auxiliar para Parsear Rangos ---
# Devuelve el promedio de un string "X-Y" o "X - Y"
parse_range_mean <- function(range_str) {
  parts <- str_split(string = range_str, pattern = "-", )
  lapply(1:length(parts), function(i) {
    if (length(parts[[i]]) == 1 || !grepl("-", range_str[i])) {
      # Intenta convertir el string completo 
      single_num <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", range_str[i]))) # Quita todo menos números y punto
      return(single_num)
   } else if (length(parts[[i]]) == 2) {
    num1 <- suppressWarnings(as.numeric(parts[[i]][1]))
    num2 <- suppressWarnings(as.numeric(parts[[i]][2]))
    if (!is.na(num1) && !is.na(num2)) {
      mean(c(num1, num2))
    } else {
      NA_real_
    }
   } else {
     NA_real_
   }

  }) %>% unlist()
}


# --- Aplicar Lógica de Parseo Vectorizada con data.table ---

# 1. Parsear Masa (Weight kg)
traits_dt[, Mass_kg_parsed := gsub(",", "", Mass_kg)]
traits_dt[, Mass_kg_parsed := fifelse(
  grepl("Not Applicable|Varies", Mass_kg_parsed, ignore.case = TRUE), # Caso 1: Texto inválido
  NA_real_,
  parse_range_mean(Mass_kg_parsed)
)]

# 2. Parsear Longevidad (Lifespan years)
traits_dt[, Longevity_yrs_parsed := fifelse(
  grepl("Not Applicable|Varies", Longevity_yrs, ignore.case = TRUE), # Texto inválido
  NA_real_,
  fifelse(
    grepl("months", Longevity_yrs, ignore.case = TRUE), # Meses
    parse_range_mean(Longevity_yrs) / 12, # Calcula media del rango (si hay) y divide por 12
    fifelse(
      grepl("weeks", Longevity_yrs, ignore.case = TRUE), # Semanas
      parse_range_mean(Longevity_yrs) / 52, # Calcula media y divide por 52
      fifelse(
         grepl("days", Longevity_yrs, ignore.case = TRUE), # Días (menos común)
         parse_range_mean(Longevity_yrs) / 365.25,
        fifelse(
          grepl("Up to", Longevity_yrs, ignore.case = TRUE), # "Up to X [years]"
          as.numeric(sub(".*?([0-9.]+).*", "\\1", gsub(",", "", Longevity_yrs))), # Extrae número
          # Rangos o números simples en años
           parse_range_mean(Longevity_yrs)
        )
      )
    )
  )
)]

traits_clean_dt <- na.omit(traits_dt)
traits_clean_dt <- traits_clean_dt[Mass_kg_parsed  > 0 & Longevity_yrs_parsed > 0]


# --- Preparación para Clustering ---
traits_cluster_dt <- traits_clean_dt[,
                                     .(Species,
                                       log_Mass_kg = log(Mass_kg_parsed),
                                       log_Longevity_yrs = log(Longevity_yrs_parsed))]

# Convertir a matriz y escalar (estandarizar)
# Usar Species como nombres de fila (rownames)
traits_matrix <- as.matrix(traits_cluster_dt[, .(log_Mass_kg, log_Longevity_yrs)])
rownames(traits_matrix) <- traits_cluster_dt$Species

# Escalar los datos (media 0, desviación estándar 1)
scaled_matrix <- scale(traits_matrix)

# Calcular matriz de distancias (Euclídea por defecto)
dist_matrix <- dist(scaled_matrix, method = "euclidean")

# Realizar clustering jerárquico (ej. método de Ward)
hclust_result <- hclust(dist_matrix, method = "ward.D2")

message("Clustering jerárquico completado.")

# --- 5. Crear Gráfico Dendrograma ---

# Definir textos
source_text_day14 <- "Source: Kaggle - Animal Information Dataset by S. Banerjee (Clustering on scaled log Mass & Longevity)"
plot_title <- "Similitud Animal Basada en Masa y Longevidad"
plot_subtitle <- "Dendrograma mostrando agrupaciones jerárquicas (Método Ward.D2, Distancia Euclídea)."

# Generar caption
caption_day14 <- generate_caption(
  day = 14,
  source_text = source_text_day14,
  config = config,
  color_text_source = "#5D4037",
  color_text_author = "#6B8E23"
)

# Colores y tema
bg_col <- "#FAF0E6" 

# Crear el gráfico dendrograma usando ggdendro
dendro_data <- dendro_data(hclust_result, type = "triangle") # Extraer datos para ggplot

# 1/3 of labels
labels <- label(dendro_data)

labels_short <- labels[seq(1, length(labels$label), 3), ]

gg_day14 <- ggplot(segment(dendro_data)) + # Usar los segmentos del dendrograma
  geom_segment(aes(x = x, y = y, xend = xend, yend = yend)) +

  geom_text(data = labels_short, aes(x = x, y = y, label = label), hjust = 0, , angle = 0, size = 3.5, 
            family = "Cabin", nudge_y = 1) +

  # Aplicar tema de la semana 3
  coord_flip() +
  scale_y_reverse(expand = c(0.3, 0)) +
  theme_week3_animals(base_family = "Cabin") +

  # Ajustes específicos para dendrograma
  theme(
    axis.text.x = element_blank(),  # Ocultar texto eje X (labels) si son muchos/ilegibles
    axis.ticks.x = element_blank(), # Ocultar ticks eje X
    axis.line.x = element_blank(),  # Ocultar línea eje X
    axis.title.x = element_blank()  # Ocultar título eje X
  ) +

  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    y = "Altura (Distancia / Disimilaridad)", # El eje Y en dendrograma es la 'altura' del cluster
    x = NULL,
    caption = caption_day14
  )


# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_14_kinship_dendrogram.png")
ggsave(
  filename = output_file,
  plot = gg_day14,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col
)

message("Gráfico del Día 14 (Animal Kinship Dendrogram) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_14_kinship_dendrogram.R ---
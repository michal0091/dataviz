# --- Day 13: Clusters (Animal Traits: Mass vs Longevity using Kaggle Dataset) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Kaggle - Animal Information Dataset by Sourav Banerjee)
# Scatter plot mostrando clusters de animales por Clase Taxonómica,
# basado en su Masa Corporal (kg) y Longevidad (años) (escalas log).

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(grid)
library(stringr)

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
setup_fonts(fa_brands_path = font_path_fa) 


# --- 4. Cargar y Preparar Datos de Rasgos Animales ---

# Ajusta la ruta si guardaste el archivo en otro lugar
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
  lapply(1:length(nums), function(i) {
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
traits_clean_dt <- traits_clean_dt[Mass_kg > 0 & Longevity_yrs > 0]


# Limpiar y convertir la columna de agrupación (Diet) a factor
traits_clean_dt[, Grouping := trimws(as.character(Grouping))] 
traits_clean_dt[Grouping %in% c("Insectivore", "Piscivore"), Grouping := "Carnivore"]
traits_clean_dt[Grouping %in% c("Frugivore", "Folivore"), Grouping := "Herbivore"]
traits_clean_dt <- traits_clean_dt[Grouping %in% c("Herbivore", "Carnivore", "Omnivore")] # Quedarse con las principales si hay muchas

traits_clean_dt[, Grouping := as.factor(Grouping)]
message("Número de especies después de limpiar: ", nrow(traits_clean_dt))
message("Grupos de Dieta encontrados: ", paste(levels(traits_clean_dt$Grouping), collapse=", "))

# --- 5. Crear Scatter Plot con Clusters por Dieta ---

# Definir textos
source_text_day13 <- "Source: Kaggle - Animal Information Dataset by Sourav Banerjee"
plot_title <- "Relación Masa Corporal vs. Longevidad en Animales"
plot_subtitle <- "Especies agrupadas por Dieta (escalas logarítmicas)."

# Generar caption
caption_day13 <- generate_caption(
  day = 13,
  source_text = source_text_day13,
  config = config,
  color_text_source = "#5D4037",
  color_text_author = "#6B8E23"
)

# Colores y tema
bg_col <- "#FAF0E6"
num_groups <- length(levels(traits_clean_dt$Grouping))
plot_colors <- challenge_pal("week3_animals")(num_groups)
names(plot_colors) <- levels(traits_clean_dt$Grouping) # Asignar nombres para scale_manual

# Crear el gráfico
gg_day13 <- ggplot(traits_clean_dt, aes(x = Mass_kg_parsed, y = Longevity_yrs_parsed, color = Grouping)) + # <- Usar Grouping (Diet)
  geom_point(alpha = 0.7, size = 2.5) +

  # Escalas logarítmicas
  scale_x_log10(
    labels = scales::label_number(suffix = " kg", accuracy = 0.1),
    breaks = scales::trans_breaks("log10", function(x) 10^x),
    name = "Masa Corporal (kg, escala log)"
   ) +
  scale_y_log10(
     labels = scales::label_comma(suffix = " años"),
     breaks = scales::trans_breaks("log10", function(x) 10^x),
     name = "Longevidad Máxima (años, escala log)"
   ) +

  # Aplicar paleta de colores manualmente
  scale_color_manual(values = plot_colors, name = "Dieta") + # <- Título leyenda

  # Aplicar tema de la semana 3
  theme_week3_animals(base_family = "Cabin") +

  # Guías para leyenda
  guides(color = guide_legend(override.aes = list(size = 4), ncol = 1)) + # 1 columna para pocas dietas

  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = caption_day13
  )

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_13_clusters_animals_diet.png") # Nombre actualizado
ggsave(
  filename = output_file,
  plot = gg_day13,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col
)

message("Gráfico del Día 13 (Animal Clusters by Diet) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_13_clusters_animals_diet.R ---
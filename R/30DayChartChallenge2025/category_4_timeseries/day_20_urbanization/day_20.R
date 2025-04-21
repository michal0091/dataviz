# --- Day 20: Urbanization (Growth of Major Spanish Cities) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: INE - Padrón Municipal / Censos Históricos)
# Evolución de la población en las principales ciudades españolas.

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

# --- 4. Cargar y Preparar Datos de Población Municipal INE (Formato Texto) ---

# Ruta al archivo de texto con formato especial
data_file <- "R/30DayChartChallenge2025/data/poblacion_municipios_ine.csv" 

if (!file.exists(data_file)) {
  stop("Archivo de texto de población municipal no encontrado en: ", data_file)
}

message("Leyendo archivo de texto línea por línea desde: ", data_file)
all_lines <- readLines(data_file, encoding = "UTF-8")
all_lines <- all_lines[-1] # Quitar header

# Inicializar listas para guardar los datos extraídos
municipalities_list <- list()
years_list <- list()
populations_list <- list()

current_municipality <- NA_character_ # Para guardar el municipio actual

message("Procesando líneas para extraer datos...")
for (line in all_lines) {
  line <- trimws(line) # Quitar espacios al inicio/final

  # Intentar detectar si es una línea de Municipio (ej: "44001 Ababuj")
  if (grepl("^[0-9]{5}\\s+", line)) {
    # Extraer solo el nombre, quitando el código inicial y el espacio
    current_municipality <- gsub(";", "", sub("^[0-9]{5}\\s+", "", line))
    # message("Municipio encontrado: ", current_municipality) # Descomentar para depurar
    next # Pasar a la siguiente línea
  }

  # Intentar detectar si es una línea de Datos (ej: "1 de enero de 2022   72")
  # Busca un patrón como "[texto] YYYY [espacios] NUMEROS_AL_FINAL"
  # Y asegurarse que tenemos un municipio actual asignado
  if (!is.na(current_municipality) && grepl(" de [[:alpha:]]+ de [0-9]{4}", line) && grepl("[0-9]+$", line)) {
    # Extraer el año (4 dígitos)
    year_match <- stringr::str_extract(line, "[0-9]{4}")
    # Extraer la población (números al final de la línea)
    pop_match <- stringr::str_extract(line, "[0-9]+$")

    if (!is.na(year_match) && !is.na(pop_match)) {
      year_val <- as.numeric(year_match)
      pop_val <- as.numeric(pop_match)

      # Añadir a las listas si ambos son válidos
      if (!is.na(year_val) && !is.na(pop_val)) {
        municipalities_list[[length(municipalities_list) + 1]] <- current_municipality
        years_list[[length(years_list) + 1]] <- year_val
        populations_list[[length(populations_list) + 1]] <- pop_val
      }
    }
  }
}

message("Procesamiento de líneas completado.")

# Crear el data.table final
if (length(municipalities_list) > 0) {
  ine_pop_dt <- data.table(
    Ciudad = unlist(municipalities_list),
    Year = unlist(years_list),
    Poblacion = unlist(populations_list)
  )
  message("Creado data.table con ", nrow(ine_pop_dt), " registros.")
} else {
  stop("No se pudieron extraer datos válidos del archivo. Revisa el formato o las expresiones regulares.")
}

# Clean ;
ine_pop_dt[, Ciudad := gsub(";", "", Ciudad)]

# --- Definir Grupos de Tamaño de Municipio ---
size_breaks <- c(0, 5001, 20001, 50001, 100001, Inf) # Límites 
size_labels <- c("< 5k", "5k - 20k", "20k - 50k", "50k - 100k", "> 100k") # Etiquetas para cada grupo

message("Agrupando municipios por tamaño y año...")
ine_pop_dt[, Size_Group := cut(Poblacion, breaks = size_breaks, labels = size_labels,
                               right = FALSE, include.lowest = TRUE)]

# Quitar filas donde no se pudo asignar grupo
ine_pop_dt <- na.omit(ine_pop_dt, cols="Size_Group")

# --- Agregar Población por Grupo y Año ---
pop_by_group_year <- ine_pop_dt[, .(Group_Population = sum(as.numeric(Poblacion), na.rm = TRUE)), # Asegurar suma numérica
                                  by = .(Year, Size_Group)]

# --- Calcular Población Total por Año ---
total_pop_year <- pop_by_group_year[, .(Total_Population = sum(Group_Population)), by = .(Year)]

# --- Unir Totales y Calcular Porcentajes ---
pop_by_group_year[total_pop_year, on = "Year", Total_Population := i.Total_Population]
pop_by_group_year[, Percentage := fifelse(Total_Population > 0, Group_Population / Total_Population * 100, 0)]

# Asegurar que Size_Group sea un factor ordenado para el gráfico de áreas
pop_by_group_year[, Size_Group := factor(Size_Group, levels = size_labels)]

message("Datos agregados por tamaño de municipio y año listos.")
print(tail(pop_by_group_year))
print(summary(pop_by_group_year))


# --- 5. Crear Gráfico de Áreas Apiladas (Proporción por Tamaño) ---

# Definir textos (AJUSTA FECHAS Y FUENTE EXACTA DEL INE)
source_text_day20 <- "Fuente: INE - Padrón Municipal"
plot_title <- "Distribución de la Población Española por Tamaño de Municipio"
plot_subtitle <- paste0("Evolución del porcentaje de población residente según tamaño del municipio.\nPeriodo: ",
                       min(pop_by_group_year$Year), "-", max(pop_by_group_year$Year))

# Generar caption
caption_day20 <- generate_caption(
  day = 20,
  source_text = source_text_day20,
  config = config,
  color_text_source = "#5c5c5c",
  color_text_author = "#757de8"
)

# Colores y tema
bg_col <- "#F2F2F2"
num_groups <- length(levels(pop_by_group_year$Size_Group))
plot_colors <- paleta_week4_social[1:num_groups]

# Usaremos RColorBrewer "YlGnBu" que es buena para secuencial
# library(RColorBrewer)
# plot_colors <- brewer.pal(n = max(3, num_groups), name = "YlGnBu") # max(3,...) por si hay pocos grupos
# # Asegurarse de tener suficientes colores si hay más de 9 grupos (improbable aquí)
# if (num_groups > length(plot_colors)) {
#     plot_colors <- colorRampPalette(plot_colors)(num_groups)
# }
names(plot_colors) <- levels(pop_by_group_year$Size_Group)


# Crear el gráfico de áreas
gg_day20 <- ggplot(pop_by_group_year, aes(x = Year, y = Percentage, fill = Size_Group, group = Size_Group)) +
  geom_area(alpha = 0.85, position = "stack", color="white", linewidth=0.1) + # Color blanco fino entre áreas

  # Escala de relleno manual con la paleta Brewer
  scale_fill_manual(values = plot_colors, name = "Tamaño Municipio") +

  # Formato de ejes
  scale_y_continuous(labels = scales::label_percent(scale = 1), expand = expansion(mult=c(0, 0.01))) +
  scale_x_continuous(expand = expansion(mult=c(0.01, 0.01))) + # Ajusta breaks si es necesario

  # Aplicar tema
  theme_week4_social(base_family = "Lato", base_size = 10) +

  # Leyenda (ordenada por defecto por niveles del factor)
  guides(fill = guide_legend(reverse = TRUE)) + # Invertir orden leyenda para que coincida con el stack

  # Etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Año",
    y = "Porcentaje Población Total",
    caption = caption_day20
  ) +
  theme(legend.title.align = 0.5)


# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_20_urbanization_cities.png")
ggsave(
  filename = output_file,
  plot = gg_day20,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col # Apaisado
)

message("Gráfico del Día 20 (Urbanization Cities) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_20_urbanization_cities.R ---

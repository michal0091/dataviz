# --- Day 15: Complicated (Palmer Penguins Traits Matrix) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: palmerpenguins R package)
# Matriz de scatter plots (ggpairs) mostrando relaciones complejas
# entre rasgos de pingüinos, coloreado por especie.

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)   
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(grid)
library(palmerpenguins) 
library(GGally)         

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"

# Verificar existencia de archivos y cargar
if (!file.exists(themes_file)) stop("Archivo de temas no encontrado: ", themes_file)
if (!file.exists(utils_file)) stop("Archivo de utilidades no encontrado: ", utils_file)
if (!file.exists(config_file)) stop("Archivo de configuración no encontrado: ", config_file)
source(themes_file)
source(utils_file)
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa) # Carga "Cabin"

# --- 4. Cargar y Preparar Datos ---

message("Cargando datos del paquete palmerpenguins...")
penguins_dt <- as.data.table(penguins)

# Seleccionar columnas relevantes y eliminar filas con NAs en ellas
cols_for_plot <- c("species", "bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g")
penguins_clean_dt <- na.omit(penguins_dt, cols = cols_for_plot)

# No necesitamos logaritmos aquí, las escalas son razonables
message("Número de pingüinos después de limpiar NAs: ", nrow(penguins_clean_dt))
message("Especies: ", paste(levels(penguins_clean_dt$species), collapse=", "))

message("Renombrando columnas a Español...")
setnames(penguins_clean_dt,
         old = c("bill_length_mm", "bill_depth_mm", "flipper_length_mm", "body_mass_g", "species"),
         new = c("Longitud Pico (mm)", "Profundidad Pico (mm)", "Longitud Aleta (mm)", "Masa Corporal (g)", "Especie")
         )
print(summary(penguins_clean_dt))


# --- 5. Crear Matriz de Scatter Plots (ggpairs) ---

# Definir textos
source_text_day15 <- "Source: palmerpenguins R package (Gorman, Williams & Fraser, 2014. PLoS ONE)"
plot_title <- "Relaciones Complejas: Pingüinos de Palmer"
plot_subtitle <- "Matriz de relaciones entre medidas corporales por especie.\nInf: Scatterplots, Sup: Correlación (Pearson), Diag: Densidad."

# Generar caption
caption_day15 <- generate_caption(
  day = 15, 
  source_text = source_text_day15,
  config = config,
  color_text_source = "#5D4037",
  color_text_author = "#6B8E23"
)


# Columnas numéricas para incluir en la matriz
ggpairs_cols <- c("Longitud Pico (mm)", "Longitud Aleta (mm)", "Masa Corporal (g)")

# Colores 
bg_col <- "#FAF0E6"
num_species <- length(levels(penguins_clean_dt$Especie)) 
plot_colors <- challenge_pal("week3_animals")(num_species)
names(plot_colors) <- levels(penguins_clean_dt$species)

message("Generando gráfico ggpairs...")
# Crear el objeto ggpairs
gg_day15_matrix <- ggpairs(
  data = penguins_clean_dt,
  columns = ggpairs_cols,
  mapping = aes(color = Especie, alpha = 0.7), # Color por especie
  upper = list(continuous = wrap("cor", size = 4.5, stars = FALSE)), # Correlación
  lower = list(continuous = wrap("points", size = 2.5)),       # Scatterplot
  diag = list(continuous = wrap("densityDiag", alpha = 0.6)),  # Densidad
  title = plot_title, # Título principal integrado
  progress = FALSE    # Ocultar barra de progreso de ggpairs
)

# Aplicar tema y escalas DESPUÉS de crear el objeto ggpairs
gg_day15 <- gg_day15_matrix +
  theme_week3_animals(base_size = 9, base_family = "Cabin") + # Tamaño base reducido para la matriz
  scale_color_manual(values = plot_colors, name = "Especie") +
  scale_fill_manual(values = plot_colors, name = "Especie") + # Para las densidades
  labs(subtitle = plot_subtitle, caption = caption_day15) + # Añadir subtítulo y caption
  # Ajustes específicos para ggpairs si son necesarios
  theme(
     strip.background = element_rect(fill=alpha(bg_col, 0.8), color=NA), # Fondo etiquetas panel semi-transp
     strip.text = element_text(face="bold", size=rel(1.5), color="#5D4037"), # Texto etiquetas panel
     aspect.ratio = NULL
   )

message("Gráfico ggpairs generado.")

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_15_complicated_penguins.png")
# Darle un poco más de tamaño para que la matriz se vea bien
ggsave(
  filename = output_file,
  plot = gg_day15,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col
)

message("Gráfico del Día 15 (Complicated Penguins ggpairs) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_15_complicated_penguins.R ---
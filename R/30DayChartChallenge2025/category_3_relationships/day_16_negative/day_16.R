# --- Day 16: Negative (Metabolic Rate vs. Lifespan from AnAge) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: AnAge Database - build 14)
# Scatter plot mostrando la relación negativa esperada entre
# la tasa metabólica basal y la longevidad máxima en animales (log-log).

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(grid)
# library(ggpubr) # Para añadir correlación opcionalmente

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
setup_fonts(fa_brands_path = font_path_fa) # Carga "Cabin"

# --- 4. Cargar y Preparar Datos de AnAge ---

# Ruta al archivo descargado
data_file <- "R/30DayChartChallenge2025/data/anage_data.txt"

if (!file.exists(data_file)) {
  stop("Archivo de datos AnAge no encontrado en: ", data_file)
}

message("Cargando datos AnAge desde: ", data_file)
anage_dt <- data.table::fread(data_file, sep = "\t", header = TRUE, quote = "")

message("Nombres de columna encontrados: ", paste(names(anage_dt), collapse=", "))

# --- Selección, Renombrado y Limpieza ---
# Usar los nombres exactos del header que proporcionaste
col_longevity <- "Maximum longevity (yrs)"
col_metabolic <- "Metabolic rate (W)"
col_mass <- "Body mass (g)"
col_class <- "Class"
col_species_gen <- "Genus" 
col_species_sp <- "Species"

required_cols <- c(col_longevity, col_metabolic, col_mass, col_class, col_species_gen, col_species_sp)
if (!all(required_cols %in% names(anage_dt))) {
  stop("El archivo AnAge no contiene todas las columnas esperadas. Revisa los nombres.")
}

# Select only high quality data
anage_clean_dt <- anage_dt[`Data quality` %in% c("high", "acceptable") &
                           `Sample size` %in% c("medium", "large", "huge")] 

# Seleccionar, renombrar y asegurar tipos numéricos
anage_clean_dt <- anage_dt[, .(
  Species = paste(get(col_species_gen), get(col_species_sp)), # Crear nombre completo
  Class = as.factor(get(col_class)),
  Longevity_yrs = as.numeric(get(col_longevity)),
  MetabolicRate_W = as.numeric(get(col_metabolic)),
  Mass_g = as.numeric(get(col_mass))
)]

# Filtrar NAs y valores no positivos necesarios para logs
numeric_cols_needed <- c("Longevity_yrs", "MetabolicRate_W", "Mass_g")
anage_clean_dt <- na.omit(anage_clean_dt, cols = numeric_cols_needed)
anage_clean_dt <- anage_clean_dt[Longevity_yrs > 0 & MetabolicRate_W > 0 & Mass_g > 0]
anage_clean_dt[, MassSpecific_MR_W_kg := MetabolicRate_W / (Mass_g / 1000)]

# Aplicar Logaritmo (base 10 es común en biología para estos logs)
anage_clean_dt[, log10_Longevity := log10(Longevity_yrs)]
anage_clean_dt[, log10_Mass_g  := log10(Mass_g)]
anage_clean_dt[, log10_MassSpecific_MR_W_kg  := log10(MassSpecific_MR_W_kg)]

# Filtrar Clases principales si hay muchas (Opcional)
main_classes <- c("Mammalia", "Aves", "Reptilia", "Amphibia", "Actinopterygii") # Peces óseos
anage_clean_dt <- anage_clean_dt[Class %in% main_classes]
anage_clean_dt[, Class := factor(Class, levels=main_classes)] # Re-factorizar

message("Calculando límites IQR para quitar outliers...")

# Función para calcular límites IQR
get_iqr_bounds <- function(x) {
  q <- quantile(x, probs = c(0.25, 0.75), na.rm = TRUE)
  iqr <- q[2] - q[1] # IQR = Q3 - Q1
  lower <- as.numeric(q[1] - 1.5 * iqr)
  upper <- as.numeric(q[2] + 1.5 * iqr)
  return(c(lower = lower, upper = upper))
}

# Calcular límites para Longevidad
bounds_longevity <- get_iqr_bounds(anage_clean_dt$log10_Longevity)
# Calcular límites para masa 
bounds_mr_spec <- get_iqr_bounds(anage_clean_dt$log10_MassSpecific_MR_W_kg)

message(sprintf("Límites Log10 Longevidad: [%.2f, %.2f]", bounds_longevity['lower'], bounds_longevity['upper']))
message(sprintf("Límites Log10 Masa Específica MR (W/kg): [%.2f, %.2f]", bounds_mr_spec['lower'], bounds_mr_spec['upper']))

# Filtrar el data.table
anage_filtered_dt <- anage_clean_dt[
  log10_Longevity >= bounds_longevity['lower'] & log10_Longevity <= bounds_longevity['upper'] &
    log10_MassSpecific_MR_W_kg >= bounds_mr_spec['lower'] & log10_MassSpecific_MR_W_kg <= bounds_mr_spec['upper']
]

nrow_after <- nrow(anage_filtered_dt)
nrow_before <- nrow(anage_clean_dt)
message(sprintf("Se quitaron %d outliers (%.1f%%). Especies restantes: %d",
                nrow_before - nrow_after, (nrow_before - nrow_after) / nrow_before * 100, nrow_after))

# Verificar resumen del dataset filtrado
print("Resumen tras quitar outliers:")
print(summary(anage_filtered_dt[, .(log10_Longevity, log10_MassSpecific_MR_W_kg)]))

# --- 5. Crear Scatter Plot con Relación Negativa ---

# Definir textos
source_text_day16 <- "Source: AnAge Database (Build 14) by The Human Ageing Genomic Resources"
plot_title <- "Relación Negativa: Tasa Metabólica vs. Longevidad Animal"
plot_subtitle <- "Relación inversa (~ -1/4) esperada por teorías metabólicas (escalas log10)."

# Generar caption
caption_day16 <- generate_caption(
  day = 16,
  source_text = source_text_day16,
  config = config,
  color_text_source = "#5D4037",
  color_text_author = "#6B8E23"
)

# Colores y tema
bg_col <- "#FAF0E6"
# Usaremos la paleta week3 para colorear por Clase
num_classes <- length(levels(anage_filtered_dt$Class))
plot_colors <- challenge_pal("week3_animals")(num_classes)
names(plot_colors) <- levels(anage_filtered_dt$Class)
line_color <- "#5D4037" # Marrón oscuro para la línea de regresión

# Crear el gráfico
gg_day16 <- ggplot(anage_filtered_dt, aes(x =  log10_MassSpecific_MR_W_kg, y = log10_Longevity)) +
  # Puntos coloreados por Clase Taxonómica
  geom_point(aes(color = Class), alpha = 0.6, size = 2.5) +

  # Línea de regresión lineal para mostrar la tendencia negativa global
  geom_smooth(method = "lm", color = line_color, fill = line_color, alpha = 0.1, se = TRUE, linewidth = 1) +

  # Aplicar paleta de colores manualmente para las Clases
  scale_color_manual(values = plot_colors, name = "Clase") +

  # Aplicar tema de la semana 3
  theme_week3_animals(base_family = "Cabin") +

  # Guías de leyenda
  guides(color = guide_legend(override.aes = list(size = 4, alpha=1), ncol = ifelse(num_classes > 5, 2, 1))) +

  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Log10 ( Tasa Metabólica Basal [Watts/kg] )", # Eje X
    y = "Log10 ( Longevidad Máxima [años] )",   # Eje Y
    caption = caption_day16
  ) + 
  scale_x_continuous(expand = c(0.1, 0.1)) +
  scale_y_continuous(expand = c(0.05, 0.05)) +
  ggpubr::stat_cor(method = "pearson", 
                  label.x.npc = 0.8, 
                  label.y.npc = "top", 
                  size = 4.5, 
                  color = line_color,
                  cor.coef.name = "rho", 
                  family = "Roboto"
                )  
  # facet_wrap(Class ~.)
# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_16_negative_metabolism_anage.png") # Nombre archivo
ggsave(
  filename = output_file,
  plot = gg_day16,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col
)

message("Gráfico del Día 16 (Negative Metabolism AnAge) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_16_negative_metabolism_anage.R ---
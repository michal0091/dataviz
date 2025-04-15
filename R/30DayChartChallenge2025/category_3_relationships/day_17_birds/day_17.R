# --- Day 17: Birds (AVONET Wingspan vs. Mass Relationship) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: AVONET Dataset - Tobias et al. 2022, Ecol Lett)
# Scatter plot (log-log) mostrando la relación entre envergadura y
# masa corporal en aves (AVONET), coloreado por Orden taxonómico.

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(grid)
library(readxl) 

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

# --- 4. Descargar (si es necesario), Cargar y Preparar Datos AVONET ---

# --- Configuración de Archivos AVONET ---
data_dir <- "R/30DayChartChallenge2025/data"
# URL encontrada en Dryad asociada a Tobias et al. 2022 (¡Verifica si sigue activa!)
avonet_url <- "https://figshare.com/ndownloader/files/34480856?private_link=b990722d72a26b5bfead"
# Nombre de archivo local esperado para el Excel
dest_file_excel <- file.path(data_dir, "AVONET_Tobias_etal_2022_EcolLets.xlsx")
sheet_name <- "AVONET1_BirdLife"

# --- Lógica de Descarga ---
if (!file.exists(dest_file_excel)) {
  message("Archivo de datos AVONET Excel no encontrado. Intentando descargar desde Dryad...")
  dir.create(data_dir, showWarnings = FALSE) # Crear carpeta 'data' si no existe
  tryCatch({
    download.file(avonet_url, dest_file_excel, mode = "wb")
    message("Descarga completada: ", dest_file_excel)
  }, error = function(e) {
    stop("Falló la descarga del archivo AVONET desde: ", avonet_url,
         "\nPuede que el enlace haya cambiado. Verifica en OpenTraits/Dryad ",
         "o descarga el archivo Excel manualmente y guárdalo como:\n'",
         dest_file_excel, "'.\nError original: ", e$message)
  })
} else {
  message("Archivo AVONET Excel encontrado: ", dest_file_excel)
}

# --- Cargar Datos desde Excel ---
message("Cargando datos AVONET desde Excel (Hoja: '", sheet_name, "')...")
avonet_raw_df <- tryCatch({
    readxl::read_excel(dest_file_excel, sheet = sheet_name)
}, error = function(e) {
    stop("Error al leer el archivo Excel. Asegúrate que la ruta es correcta y la hoja '",
         sheet_name, "' existe.\nError original: ", e$message)
})

avonet_dt <- as.data.table(avonet_raw_df) # Convertir a data.table
message("Datos AVONET cargados. ", nrow(avonet_dt), " filas iniciales.")
setnames(avonet_dt, names(avonet_dt), make.names(names(avonet_dt)))
message("Nombres de columna limpios: ", paste(head(names(avonet_dt)), collapse=", "), "...")

# --- Limpieza y Selección de Columnas AVONET ---
col_species <- "Species1" # Nombre científico
col_order <- "Order1"
col_mass <- "Mass"          # Masa en gramos (g)
col_wingspan <- "Wing.Length" # Envergadura en milímetros (mm) - 
col_habitat <- "Habitat"

required_cols <- c(col_species, col_order, col_habitat, col_mass, col_wingspan)
if (!all(required_cols %in% names(avonet_dt))) {
  stop("El archivo AVONET no contiene las columnas esperadas tras limpiar nombres. Columnas encontradas: ",
       paste(names(avonet_dt), collapse=", "))
}

# Seleccionar, renombrar, convertir unidades y asegurar tipos numéricos
bird_clean_dt <- avonet_dt[, .(
  Species = get(col_species),
  Order = as.factor(get(col_order)),
  Habitat = get(col_habitat),
  Mass_g = as.numeric(get(col_mass)),
  Wingspan_cm = as.numeric(get(col_wingspan)) / 10 # Convertir de mm a cm
)]

# Filtrar NAs y valores no positivos necesarios para logs
bird_clean_dt <- na.omit(bird_clean_dt)
bird_clean_dt <- bird_clean_dt[Mass_g > 0 & Wingspan_cm > 0]


# Aplicar Logaritmo (base 10)
bird_clean_dt[, log10_Mass := log10(Mass_g)]
bird_clean_dt[, log10_Wingspan := log10(Wingspan_cm)]

# Filtrar Órdenes con pocas especies
order_counts <- bird_clean_dt[, .N, by = Order][order(-N)]
print("Top 12 Órdenes por número de especies:")
print(head(order_counts, 12))

# Filtrar por los 6 hábitats más comunes
habitats <- avonet_dt[, .N, Habitat][order(-N), head(Habitat, 6)]
bird_clean_dt <- bird_clean_dt[Habitat %in% habitats]
orders_to_keep <- head(order_counts, 12)$Order
bird_clean_dt <- bird_clean_dt[Order %in% orders_to_keep]
bird_clean_dt[, Order := factor(Order, levels=orders_to_keep)] # Re-factorizar con los seleccionados

message("Número de especies final tras filtrar y limpiar: ", nrow(bird_clean_dt))
message("Órdenes considerados: ", paste(levels(bird_clean_dt$Order), collapse=", "))
print("Resumen de variables (log10):")
print(summary(bird_clean_dt[, .(log10_Mass, log10_Wingspan)]))

# Traducir el hábito a español
bird_clean_dt[, habitat := fcase(
  Habitat == "Forest", "Bosque",
  Habitat == "Shrubland", "Matorral",
  Habitat == "Grassland", "Pradera",
  Habitat == "Woodland", "Bosque arbolado",
  Habitat == "Wetland", "Humedal",
  Habitat == "Marine", "Marino"
)]


# --- 5. Crear Scatter Plot (Masa vs Envergadura) ---

# Definir textos
source_text_day17 <- "Source: AVONET dataset (Tobias et al. 2022, Ecology Letters)"
plot_title <- "Relación Longitud del Ala vs. Masa Corporal en Aves"
plot_subtitle <- "Especies de los 12 Órdenes más comunes en AVONET (escalas log10)."

# Generar caption
caption_day17 <- generate_caption(
  day = 17,
  source_text = source_text_day17,
  config = config,
  color_text_source = "#5D4037",
  color_text_author = "#6B8E23"
)

# Colores y tema
bg_col <- "#FAF0E6"
num_orders <- length(levels(bird_clean_dt$Order)) 
plot_colors <- challenge_pal("week3_animals", space = "rgb", bias = 2, interpolate = "linear", alpha = FALSE)(num_orders) # Ajusta el número si es necesario
names(plot_colors) <- levels(bird_clean_dt$Order)
line_color <- "#5D4037"
text_col <- "#5D4037"

# Crear el gráfico
gg_day17 <- ggplot(bird_clean_dt, aes(x = log10_Mass, y = log10_Wingspan)) +
  geom_point(aes(color = Order), alpha = 0.6, size = 3.5) + # Puntos algo más pequeños/transparentes
  scale_color_manual(values = plot_colors, name = "Orden") +
  theme_week3_animals(base_family = "Cabin") +
  theme(
    axis.text.y = element_text(vjust = 0.5),
    # Personalizar facet_wrap
    strip.background = element_rect(fill = bg_col, color = NA),
    strip.text = element_text(
      color = text_col,
      size = rel(1.4),
      face = "bold"
    )
  ) + 
  guides(color = guide_legend(override.aes = list(size = 4, alpha=1), ncol = 5)) +
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Log10 ( Masa Corporal [g] )",
    y = "Log10 ( Longitud del Ala [cm] )",
    caption = caption_day17
  ) +
  facet_wrap(habitat ~ . , ncol = 3) 

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_17_birds_wingspan_avonet.png") # Nombre archivo
ggsave(
  filename = output_file,
  plot = gg_day17,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col
)

message("Gráfico del Día 17 (Birds Wingspan AVONET) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_17_birds_wingspan_avonet.R ---
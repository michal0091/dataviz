# --- Day 18: El País (Theme Day) - Housing Effort vs Pet Ratio (Madrid) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: datos.madrid.es / INE - via User CSV)
# Relación entre esfuerzo inmobiliario y ratio mascotas/niño por distrito y año.
# Estilo inspirado en El País.

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(lubridate)
library(viridis) # Para escala de color de año

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"

# Verificar existencia de archivos y cargar
source(themes_file) # Carga paletas y TEMA EL PAIS
source(utils_file)  # Carga caption y setup_fonts (con "Lato")
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Cargar y Preparar Datos de Madrid ---
data_dir <- "R/30DayChartChallenge2025/data"
csv_file <- file.path(data_dir, "madrid.csv") # <- Nombre del archivo CSV

if (!file.exists(csv_file)) stop("Archivo CSV de Madrid no encontrado: ", csv_file)

message("Cargando datos desde: ", csv_file)
# Leer CSV, ¡cuidado con separador decimal y de miles si no es punto!
# Asumiré punto decimal por defecto. Si es coma, usa dec=",".
madrid_dt <- data.table::fread(csv_file, header = TRUE)
message("Datos cargados. ", nrow(madrid_dt), " filas iniciales.")
print(head(madrid_dt, 2)) # Ver las primeras filas y nombres

# --- Limpieza y Cálculo de Variables ---
# Limpiar nombres de distrito
madrid_dt[, Distrito := sub("^[0-9]+\\. ", "", Distrito)]

# Asegurar tipos numéricos (puede necesitar gsub(",",".",...) si decimal es coma)
cols_num <- c("Renta neta media por persona", "Menores de 16",
              "ESPECIE CANINA", "ESPECIE FELINA", "precio €/m2 vivienda venta")
madrid_dt[, (cols_num) := lapply(.SD, as.numeric), .SDcols = cols_num] 

# Calcular variables necesarias
madrid_dt[, Total_Pets := `ESPECIE CANINA` + `ESPECIE FELINA`]
# Calcular Ratio (evitando división por cero)
madrid_dt[, Pet_Ratio := fifelse(`Menores de 16` > 0, Total_Pets / `Menores de 16`, NA_real_)]
# Calcular Ingreso Hogar (Asunción: 2x ingreso per cápita ANUAL)
madrid_dt[, Household_Income := `Renta neta media por persona` * 2]
# Calcular Precio Vivienda 75m2
madrid_dt[, Price_75m2 := `precio €/m2 vivienda venta` * 75]
# Calcular Años de Esfuerzo
madrid_dt[, Years_Effort := fifelse(Household_Income > 0, Price_75m2 / Household_Income, NA_real_)]

# Seleccionar datos finales y quitar NAs
madrid_clean_dt <- na.omit(madrid_dt, cols=c("Distrito", "year", "Pet_Ratio", "Years_Effort", "Household_Income"))

madrid_clean_dt[, year_f := factor(year)]

message("Datos calculados y limpios: ", nrow(madrid_clean_dt), " observaciones Distrito-Año.")
print(summary(madrid_clean_dt[, .(Pet_Ratio, Years_Effort)]))
print("Años incluidos:")
print(levels(madrid_clean_dt$year_f))

# --- 5. Crear Scatter Plot (Estilo El País) ---

# Definir textos
source_text_day18 <- "Fuente: Portal de Datos Abiertos Ayto. Madrid / INE (elaboración propia)"
plot_title <- "¿El Ladrillo ahoga a la Cigüeña? Mascotas y Esfuerzo Inmobiliario"
plot_subtitle <- "Relación positiva en distritos de Madrid (2015-2022):\nA más años para pagar el piso, ¿más 'perrhijos' por niño?"

# Generar caption
caption_day18 <- generate_caption(
  day = 18,
  source_text = source_text_day18, 
  config = config,
  color_text_source = challenge_palettes$el_pais['text2'],
  color_text_author = challenge_palettes$el_pais['text']
)

# Colores
bg_col <- challenge_palettes$el_pais['bg']
text_col <- challenge_palettes$el_pais['text']
accent_color <- challenge_palettes$el_pais['accent1'] # Azul para regresión

# Crear el gráfico
gg_day18 <- ggplot(madrid_clean_dt, aes(x = Pet_Ratio, y = Years_Effort)) +
  # Puntos coloreados por año
  geom_point(aes(color = year_f), alpha = 1, size = 3.5) +

  # Línea de regresión lineal
  geom_smooth(method = "lm", color = accent_color, fill = accent_color, alpha = 0.1, se = TRUE, linewidth = 0.8) +

  # Escala de color para año (Viridis es buena opción)
  scale_color_manual(values = c(
    "#2c85b1", "#96bbd7", "#41373f", "#c6c08c", 
    "#df3a4d", "#ef9ca6", "#3d5a0e", "#7ab41d"), 
    name = "Año") + # colores más de El País
  
  # Guías para leyenda (ajustar ncol si es necesario para 8 años)
  guides(color = guide_legend(override.aes = list(size = 4, alpha=1), ncol = 4)) + 


  # Formato de ejes
  scale_x_continuous(expand = expansion(mult=c(0.02, 0.05))) + # Empezar eje X en 0
  scale_y_continuous(expand = expansion(mult=c(0.02, 0.05))) + # Empezar eje Y en 0

  # Aplicar tema El País
  theme_el_pais(base_family = "Lato", base_size = 10) + # Usar Lato, ajustar tamaño base

  # Etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Ratio Mascotas (Perros+Gatos) / Menores 16",
    y = "Años de Renta Familiar (Vivienda 75m²)",
    caption = caption_day18
  ) +
  facet_wrap(Distrito ~ ., ncol = 4, scales = "free")


# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_18_el_pais_pets_effort.png")
ggsave(
  filename = output_file,
  plot = gg_day18,
  width = 1400, height = 1400, units = "px", dpi = 150, bg = bg_col
)

message("Gráfico del Día 18 (El Pais - Pets vs Effort) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_18_el_pais.R ---
# --- Day 5: Ranking (Lollipop Chart - Precio Vivienda Distritos Madrid) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Idealista)

# --- 1. Cargar Librerías ---
library(data.table)
library(ggplot2)
library(dplyr)
library(yaml)
library(showtext)
library(ggtext)
library(scales) # Para formatear etiquetas

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

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Preparar Datos ---
distritos <- c("Salamanca", "Chamberí", "Retiro", "Chamartín", "Centro", "Moncloa",
               "Arganzuela", "Tetuán", "Hortaleza", "Fuencarral", "Ciudad Lineal",
               "Barajas", "Moratalaz", "San Blas", "Latina", "Vicálvaro",
               "Carabanchel", "Villa de Vallecas", "Usera", "Puente de Vallecas", "Villaverde")
precios <- c(9374, 7677, 7014, 6988, 6816, 5500, 5341, 5162, 4745, 4572, 4219,
             4070, 3607, 3362, 3275, 3171, 3039, 3032, 2880, 2648, 2294) # Idealista Mar 2025
fuente_datos_real <- "Idealista (Marzo 2025)"

# Crear el data.table inicial
dt_distritos_ranking <- data.table(
  Distrito = factor(distritos), 
  Precio_m2 = precios
)

# Precio medio Madrid
precio_medio_madrid <- 5321 # Idealista Mar 2025

# Calcular diferencia y tipo usando sintaxis data.table (:= para añadir columnas)
dt_distritos_ranking[, Diff_vs_Avg := Precio_m2 - precio_medio_madrid]
dt_distritos_ranking[, Tipo := fifelse(Diff_vs_Avg >= 0, "Superior a la Media", "Inferior a la Media")]
dt_distritos_ranking[, Tipo := factor(Tipo, levels = c("Superior a la Media", "Inferior a la Media"))]

# --- 5. Crear Gráfico ---

# Definir textos
plot_title <- "Ranking de Distritos de Madrid"
plot_subtitle <- paste0("Diferencia del precio medio (€/m²) de cada distrito\nrespecto a la media de Madrid (Media Madrid = ",
                       number(precio_medio_madrid, accuracy=1, big.mark=".", decimal.mark=","), " €/m²)")

# Generar caption
caption_day5 <- generate_caption(
  day = 5,
  source_text = fuente_datos_real,
  config = config
)

# Colores para los tipos
colores_tipo <- c("Superior a la Media" = paleta_week1[1], # Azul oscuro
                  "Inferior a la Media" = paleta_week1[4]) # Rojo oscuro

# Rango de variación
rango <- dt_distritos_ranking[, round((max(c(max(Diff_vs_Avg), abs(min(Diff_vs_Avg)))) * 1.1) / 500) * 500 ]

# Crear el gráfico de piruleta divergente horizontal ordenado
gg_day5 <- dt_distritos_ranking[, 
  ggplot(.SD, aes(x = Diff_vs_Avg, y = reorder(Distrito, Diff_vs_Avg))) +
  # Línea vertical en cero (la media)
  geom_vline(xintercept = 0, color = paleta_week1[2], linewidth = 1.05) +
  # Segmento (palo de la piruleta) desde 0 hasta la diferencia
  geom_segment(aes(x = 0, xend = Diff_vs_Avg,
                   y = reorder(Distrito, Diff_vs_Avg), 
                   yend = reorder(Distrito, Diff_vs_Avg)),
                   color = fifelse(Tipo == "Superior a la Media",  paleta_week1[1],  paleta_week1[4]),
                   linewidth = 1.1) +
  # Punto (cabeza de la piruleta), coloreado por Tipo (encima/debajo media)
  geom_point(aes(color = Tipo), size = 2.5) +
  # Asignar colores manualmente
  scale_color_manual(values = colores_tipo, guide = "none") + # Ocultar leyenda de color
  # Formatear eje X
  scale_x_continuous(
    limits = c(-rango, rango),
    breaks = seq(-rango, rango, 1500),
    expand = c(0.1, 0.1),
    labels = label_number(big.mark = ".", decimal.mark = ",", suffix = " €")) +
  # Aplicar tema personalizado
  theme_week1(base_size = 10) +
  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Diferencia vs Precio Medio (€/m²)",
    y = NULL,
    caption = caption_day5
  ) +
  # Ajustes adicionales al tema
  theme(
    panel.grid.major.y = element_blank(), # Sin líneas grid horizontales
    panel.grid.minor.x = element_blank(), # Sin grids menores en X
    axis.text.y = element_text(size = rel(1)),
    plot.margin = margin(15, 30, 10, 15)
  )]

# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_5_ranking_madrid_diverging_lollipop.png") 
ggsave(
  filename = output_file,
  plot = gg_day5,
  width = 1200, height = 1200, units = "px", dpi = 300, bg = "white"
)

message("Gráfico del Día 5 (Diverging Lollipop Madrid) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_5.R ---
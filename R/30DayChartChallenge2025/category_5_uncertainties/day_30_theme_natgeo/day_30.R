# --- Day 30: National Geographic (Desertification Risk Map - Spain) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: MITECO - PAND, 2008)
# Mapa del riesgo de desertificación en España peninsular y Baleares, estilo NatGeo.

# --- 1. Cargar Librerías ---
library(ggplot2)
library(sf)      
library(data.table)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(RColorBrewer) 
library(ggspatial)    
library(mapSpain) 
library(grid)

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
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Cargar y Preparar Datos Espaciales PAND ---

# --- Cargar Contorno de España ---
esp_can <- esp_get_country()
can_prov <- esp_get_can_provinces()
can_box <- esp_get_can_box()

# Ficheros riesgo de desertificación (PAND)
shapefile_p_path <- "R/30DayChartChallenge2025/data/pand-p-shp/" 
shapefile_p_layer <- "pand_p"

shapefile_c_path <- "R/30DayChartChallenge2025/data/pand-c-shp/" 
shapefile_c_layer <- "pand_c"

if (!dir.exists(shapefile_p_path) || !dir.exists(shapefile_c_path)) {
  stop("Descarga el fichero pand_*.zip y descomprimelo en la carpeta: R/30DayChartChallenge2025/data\nDisponible en: https://www.miteco.gob.es/es/biodiversidad/servicios/banco-datos-naturaleza/informacion-disponible/descarga_pand.html")
}

message("Cargando Shapefile PAND desde: ", shapefile_p_path)
risk_map_p_sf <- sf::st_read(dsn = shapefile_p_path, layer = shapefile_p_layer, options = "ENCODING=LATIN1")

message("Shapefile cargado. CRS inicial: ", st_crs(risk_map_p_sf)$input)
# Reproyectar  a EPSG:4258 
risk_map_p_sf <- sf::st_transform(risk_map_p_sf, crs = 4258)

message("Cargando Shapefile PAND desde: ", shapefile_c_path)
risk_map_c_sf <- sf::st_read(dsn = shapefile_c_path, layer = shapefile_c_layer, options = "ENCODING=LATIN1")

message("Shapefile cargado. CRS inicial: ", st_crs(risk_map_c_sf)$input)
# Reporyectar a EPSG:4258 
risk_map_c_sf <- sf::st_transform(risk_map_c_sf, crs = 4258)



# --- Limpieza y Preparación de la Variable de Riesgo ---
risk_col_name <- "DESER_CLA" # Columna con códigos numéricos
if (!risk_col_name %in% names(risk_map_c_sf) || !risk_col_name %in% names(risk_map_p_sf)) stop("Columna 'DESER_CLA' no encontrada.")

# 1. Filtrar solo los niveles de riesgo válidos (1 a 4)
risk_map_c_filtered_sf <- risk_map_c_sf[risk_map_c_sf[[risk_col_name]] %in% c(1, 2, 3, 4), ]
risk_map_p_filtered_sf <- risk_map_p_sf[risk_map_p_sf[[risk_col_name]] %in% c(1, 2, 3, 4), ]

# 2. Crear Factor Ordenado con Etiquetas en Español
risk_levels_num <- c(1, 2, 3, 4)
risk_labels_es <- c("Bajo", "Medio", "Alto", "Muy Alto")
risk_map_c_filtered_sf$Nivel_Riesgo <- factor(risk_map_c_filtered_sf[[risk_col_name]],
                                          levels = risk_levels_num,
                                          labels = risk_labels_es)
risk_map_p_filtered_sf$Nivel_Riesgo <- factor(risk_map_p_filtered_sf[[risk_col_name]],
                                          levels = risk_levels_num,
                                          labels = risk_labels_es)



# --- 5. Crear Mapa Estilo National Geographic ---

# Definir textos
source_text_day30 <- "Fuente: Mapa de Riesgo de Desertificación (PAND), MITECO (2008)"
plot_title <- "Riesgo de Desertificación en España"
plot_subtitle <- "Clasificación de zonas según vulnerabilidad (Península y Baleares).\nLa desertificación es una incertidumbre ambiental y socioeconómica clave."

# Generar caption
caption_day30 <- generate_caption(
  day = 30, 
  source_text = source_text_day30,
  config = config,
  color_text_source = "#000000",
  color_text_author = "#455A64"
)

# Paleta de colores estilo NatGeo para riesgo 
# Paleta NatGeo para Riesgo Desertificación (4 niveles: Bajo -> Muy Alto)
natgeo_risk_palette <- setNames(
  c("#ffffcc", "#fed976", "#fd8d3c", "#800026"), 
  levels(risk_map_p_filtered_sf$Nivel_Riesgo)
)
# Thema estilo NatGeo:
theme_natgeo_custom <- theme_minimal(base_family = "Lato", base_size = 11) %+replace%
    theme(
        panel.grid.major = element_blank(), # Sin rejilla
        panel.grid.minor = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        axis.title = element_blank(),
        plot.title = element_text(family="Gudea", face="bold", size=rel(2.8), hjust = 0, margin=margin(b=5, l=40)),
        plot.subtitle = element_text(family="Lato", hjust = 0, size=rel(2), margin=margin(b=15, l=40), lineheight = 1.1),
        legend.position = "bottom", # Leyenda abajo
        legend.title = element_text(face="bold", size=rel(1.4)),
        legend.text = element_text(size=rel(1.2)),
        legend.key.size = unit(0.8, "lines"),
        plot.title.position = "plot",
        plot.background = element_rect(fill = "#F7F7F7", color = NA), # Fondo off-white
        panel.background = element_rect(fill = "#E4E4E4", color = NA), # Panel gris claro (agua/fondo)
        plot.caption = element_markdown(margin=margin(t=10), hjust = 0, halign = 0, size=rel(1.4)),
        plot.caption.position = "plot",
        plot.margin = margin(5, 10, 5, 10)
    )

bg_col <- "#F7F7F7" # Color fondo para guardar
natgeo_yellow <- "#FFCC00" 


# Crear el mapa base
gg_day30 <- ggplot() +
  # Capa de fondo para el panel (simula mar/área no mapeada)
  geom_sf(data = st_as_sfc(st_bbox(risk_map_p_filtered_sf)), fill = "#E4E4E4", color = NA) + # Usa el color de fondo del panel
  geom_sf(data = esp_move_can(st_as_sfc(st_bbox(risk_map_c_filtered_sf))), fill = "#E4E4E4", color = NA) + # Usa el color de fondo del panel

  # Capa base del contorno Península/Baleares
  geom_sf(data = esp_can, fill = "#E4E4E4", color = "grey50", linewidth = 0.2) + # Color tierra/mar de fondo
  # Capa base del contorno Canarias
  geom_sf(data = can_prov, fill = "#E4E4E4", color = "grey50", linewidth = 0.2) +
  # Capa base separación Canarias
  geom_sf(data = can_box) +  

  # Capa principal con las zonas de riesgo Península/Baleares (encima)
  geom_sf(data = risk_map_p_filtered_sf, aes(fill = Nivel_Riesgo), color = "white", linewidth = 0.05) +
  
  # Capa principal con las zonas de riesgo Canarias (encima) MOVER CANARIAS!
  geom_sf(data = esp_move_can(risk_map_c_filtered_sf), aes(fill = Nivel_Riesgo), color = "white", linewidth = 0.05) +
  
  # Escala de color
  scale_fill_manual(values = natgeo_risk_palette, name = "Nivel de Riesgo") + 

  # Elementos de mapa
  annotation_scale(location = "bl", width_hint = 0.25, style="ticks", line_col="grey20", text_family="Lato", text_col="grey20") +
  annotation_north_arrow(location = "tr", which_north = "true",
                         style = north_arrow_fancy_orienteering(
                          line_width = 1,
                          line_col = "grey20",
                          fill = c("#F7F7F7", "grey20"),
                          text_col = "grey20",
                          text_family = "Lato",
                          text_face = NULL,
                          text_size = 10,
                          text_angle = 0
                        ),
                         height = unit(1.5, "cm"), width = unit(1.5, "cm")) +

  # Aplicar tema
  theme_natgeo_custom +

  # Etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    caption = caption_day30
  ) +
   guides(fill = guide_legend(title.position = "top", title.hjust = 0.5, label.position="bottom", nrow=1)) 


# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_30_natgeo_desertification.png")
ggsave(
  filename = output_file,
  plot = gg_day30,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col 
)

# Toque final: añadir banda amarilla
# Abrir la imagen
png(output_file, width = 1200, height = 1200, units = "px", bg = bg_col, res = 150)

# Visualizar
gg_day30

# Añadir banda amarilla
grid.rect(
  x = 0,
  y = 1,
  width = 1/1.618^6,
  height =1/1.618^5,
  just = c("left", "top"),
  gp = gpar(fill = natgeo_yellow, col = NA, lwd = 0)
)

# Cerrar la imagen
dev.off()


message("Gráfico del Día 30 (NatGeo Desertification) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_30_natgeo_desertification.R ---



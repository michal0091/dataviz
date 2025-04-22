# --- Day 23: Log Scale (Long-Term GDP per Capita Growth in Spain) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: World Bank via WDI package)
# Evolución del PIB per cápita de España (PPA, const.) en escala logarítmica.

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(lubridate)
library(WDI)    

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"

# Verificar existencia de archivos y cargar
source(themes_file) # Carga paletas y theme_week4_social
source(utils_file)  # Carga caption y setup_fonts (con "Lato")
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa) # Carga "Lato"

# --- 4. Cargar y Preparar Datos del Banco Mundial ---

# Indicador: PIB per capita, PPA (dólares internacionales constantes de 2017)
indicator_code <- "NY.GDP.PCAP.PP.KD"
country_code <- "ES" # Código ISO para España
start_year <- 1990 # Datos disponibles desde 1990 en este indicador
# Obtener hasta el último año completo con datos (normalmente con 1-2 años de retraso)
end_year <- year(Sys.Date()) - 2

message("Descargando datos del PIB per cápita para España desde World Bank (WDI)...")
gdp_raw_df <- tryCatch({
  WDI(country = country_code, indicator = indicator_code, start = start_year, end = end_year, extra = FALSE)
}, error = function(e) {
  warning("Error al descargar datos de WDI. Comprueba la conexión o el indicador.\nError: ", e$message)
  return(NULL)
})

if (is.null(gdp_raw_df) || nrow(gdp_raw_df) == 0) {
  stop("No se pudieron obtener datos del Banco Mundial.")
}

# Convertir a data.table y limpiar
gdp_dt <- as.data.table(gdp_raw_df)
setnames(gdp_dt, indicator_code, "GDP_pc_ppp_kd", skip_absent=TRUE) 
if (!"GDP_pc_ppp_kd" %in% names(gdp_dt)) {
    # Intentar encontrarla si WDI usó un nombre ligeramente diferente
    potential_name <- names(gdp_dt)[!names(gdp_dt) %in% c("iso2c", "country", "year", "iso3c")]
    if (length(potential_name) == 1) {
        message("Columna indicador encontrada como: ", potential_name)
        setnames(gdp_dt, potential_name, "GDP_pc_ppp_kd")
    } else {
        stop("No se pudo identificar la columna del indicador GDP descargada.")
    }
}
gdp_dt <- gdp_dt[, .(Year = year, GDP_pc = GDP_pc_ppp_kd)] # Seleccionar y renombrar
gdp_dt <- na.omit(gdp_dt) # Quitar años con NA
setorder(gdp_dt, Year) # Ordenar por año

message("Datos de PIB per cápita preparados. Rango: ", min(gdp_dt$Year), "-", max(gdp_dt$Year))
print(tail(gdp_dt))
print(summary(gdp_dt))

# --- Definir períodos de crisis ---
crisis_periods <- data.frame(
  start_year = c(1992, 2008, 2020), 
  end_year   = c(1994, 2013, 2021)  
)

# --- 5. Crear Gráfico de Líneas con Escala Logarítmica ---

# Definir textos
source_text_day23 <- "Fuente: World Bank (Indicator: NY.GDP.PCAP.PP.KD) via WDI package"
plot_title <- "Crecimiento Económico de España (1990-2023)"
plot_subtitle <- paste0("Evolución del PIB per cápita (PPA, dólares const. 2017).\n",
                       "La escala logarítmica del eje Y revela cambios en la tasa de crecimiento.\n",
                       "Las áreas sombreadas indican períodos de crisis económicas en España.",
                       "Periodo: ", min(gdp_dt$Year), "-", max(gdp_dt$Year), ".")
# Generar caption
caption_day23 <- generate_caption(
  day = 23,
  source_text = source_text_day23,
  config = config, 
  color_text_source = "#5c5c5c",
  color_text_author = "#757de8"
)

# Colores y tema
bg_col <- "#F2F2F2"
line_color <- challenge_palettes$week4_social['indigo'] # Azul índigo

# Crear el gráfico
gg_day23 <- ggplot(gdp_dt, aes(x = Year, y = GDP_pc)) +
  geom_line(color = line_color, linewidth = 1.2, alpha = 0.8) +
  geom_point(color = line_color, size = 2, shape=21, fill=bg_col, stroke=0.5) +

  # Añadir sombreado para periodos de crisis
  geom_rect(
    data = crisis_periods,
    mapping = aes(xmin = start_year, xmax = end_year, ymin = 0, ymax = Inf),
    fill = challenge_palettes$week4_social['pink'] , alpha = 0.2, 
    inherit.aes = FALSE 
  ) +

  scale_y_log10(
    name = "PIB per Cápita (PPA, $ const. 2017) [Escala Log]", # Título eje Y
    labels = scales::label_dollar(accuracy = 1), # Formato Dólar (simplificado)
    breaks = scales::log_breaks(n = 6) # Ajustar número de breaks si es necesario
   ) +

  # Escala X (Año)
  scale_x_continuous(n.breaks = 8) + 

  # Aplicar tema
  theme_week4_social(base_family = "Lato", base_size = 10) +

  # Etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Año",
    caption = caption_day23
  ) +
  theme(legend.position = "none") 





# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_23_logscale_gdp_spain.png")
ggsave(
  filename = output_file,
  plot = gg_day23,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col # Apaisado
)

message("Gráfico del Día 23 (Log Scale GDP) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_23_logscale_gdp.R ---
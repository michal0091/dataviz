# --- Day 21: Fossils (Spain's Peninsular Electricity Generation Mix) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: Red Eléctrica de España - REE)
# Evolución de la proporción de fuentes en la generación eléctrica peninsular,
# destacando la contribución de combustibles fósiles.

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table)
library(yaml)
library(showtext)
library(ggtext)
library(scales)
library(lubridate)
library(qs)       
library(httr)
library(jsonlite)
library(stringr)
library(glue)

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

# --- 4. Cargar y Preparar Datos REE ---

data_file <- "R/30DayChartChallenge2025/data/estructura_generacion_peninsula.qs"

if (file.exists(data_file)) {
  energy_dt <- qread(data_file)
  message("Cargando archivo QS de REE: ", data_file)
} else {
  message("Archivo QS de REE no encontrado: ", data_file, ". Descargando datos desde la API...")
  # Generate a sequence of months
  start_date <- as.Date("2014-12-01")
  end_date <- as.Date("2025-04-20")
  
  all_months <- seq(start_date, end_date, by = "month")
  
  # Create a data.table to store the first and last day of each month
  month_dates_dt <- data.table(MonthStart = all_months,
                               MonthEnd = ceiling_date(all_months, "month") - days(1))
  
  all_dates_list <- lapply(month_dates_dt$MonthStart, function(month_start) {
    this_month_end <- month_dates_dt$MonthEnd[month_dates_dt$MonthStart == month_start]
    url <- glue(
      "https://apidatos.ree.es/es/datos/generacion/estructura-generacion?start_date={month_start}T00:00&end_date={this_month_end}T23:59&time_trunc=day&geo_limit=peninsular&geo_ids=8741"
    )
    data_list <- read_json(url)
    data_list <- data_list$included
    ree_data_dt <- rbindlist(lapply(data_list, function(source_element) {
      source_name <- source_element$attributes$title
      source_category <- source_element$attributes$type 
      source_id <- source_element$id
      source_color <- source_element$attributes$color
      
      # Get the list of time-series values
      values_list <- source_element$attributes$values
      
      # Check if values_list is valid and has data
      if (is.null(values_list) || length(values_list) == 0) {
        return(NULL)
      }
      
      time_point_dt <- rbindlist(lapply(values_list, function(time_point) {
        data.table(
          SourceName = source_name,
          SourceCategory = source_category,
          SourceID = source_id,
          SourceColor = source_color,
          DateTime_Str = time_point$datetime,
          Value = time_point$value,
          Percentage = time_point$percentage
        )
      }))
      
      time_point_dt
      
    }), fill = TRUE)
    
    ree_data_dt
    
  })
  
  energy_dt <- rbindlist(all_dates_list)
  
  
  # --- Post-processing ---
  
  # Check if the resulting data.table is not empty
  if (nrow(energy_dt) > 0) {
    # Convert the DateTime_Str string column to date
    energy_dt[, Date := as.Date(stringr::str_sub(DateTime_Str, 1, 10))]
    # Delete the original string column
    energy_dt[, DateTime_Str := NULL]
    
    # Set column order for better readability (optional)
    setcolorder(
      energy_dt,
      c(
        "SourceName",
        "SourceCategory",
        "SourceID",
        "Date",
        "Value",
        "Percentage",
        "SourceColor"
      )
    )
    
    # --- Display Results ---
    message("Successfully converted JSON data to data.table:")
    print(head(energy_dt))
    message("\nStructure of the data.table:")
    str(energy_dt)
    message("\nSummary of the data.table:")
    print(summary(energy_dt))
    
  } else {
    warning(
      "The resulting data.table is empty. Check the structure of 'data_list' or the API response."
    )
  }
  
  # Save data
  qsave(energy_dt, data_file)
}


# --- Limpieza y Agrupación de Fuentes ---

energy_dt <- energy_dt[SourceCategory != "Generación total"]

# Verificar nombres y valores únicos de SourceName para agrupar correctamente
print("Fuentes Originales (SourceName):")
print(unique(energy_dt$SourceName))
print("Categorías Originales (SourceCategory):")
print(unique(energy_dt$SourceCategory))

# Crear nueva columna 'Grupo_Energia'
energy_dt[, Grupo_Energia := fcase(
  SourceName %in% c("Carbón", "Ciclo combinado", "Cogeneración", "Fuel + Gas", "Motores diésel", "Turbina de gas", "Turbina de vapor"), "Fósiles/Térmica No Ren.", 
  SourceName %in% c("Nuclear"), "Nuclear",
  SourceName %in% c("Hidráulica"), "Hidráulica",
  SourceName %in% c("Eólica"), "Eólica",
  SourceName %in% c("Solar fotovoltaica", "Solar térmica"), "Solar",
  # Agrupar el resto de renovables (o no renovables si las hubiera)
  SourceCategory == "Renovable" & !(SourceName %in% c("Hidráulica", "Eólica", "Solar fotovoltaica", "Solar térmica")), "Otras Renovables",
  default = "Otros/Ajustes"
)]

# Verificar los grupos creados
print("Grupos de Energía Creados:")
print(unique(energy_dt$Grupo_Energia))

# Calcular porcentaje 
energy_dt[, Share_Pct := Percentage * 100]

# Filtrar valores negativos o NAs en Percentage si los hubiera 
energy_dt <- energy_dt[!is.na(Share_Pct) & Share_Pct >= 0]

# --- Agregar Porcentajes por Grupo y Fecha ---
energy_agg_daily <- energy_dt[, .(Share_Agg_Pct = sum(Share_Pct)), by = .(Date, Grupo_Energia)]
energy_agg_daily[, Roll_Mean_Pct := frollmean(Share_Agg_Pct, n = 30, align = "right", na.rm = TRUE), Grupo_Energia]

# --- Ordenar Niveles del Factor para Stacking ---
# Definir el orden deseado para las capas (de abajo a arriba)
source_levels_ordered <- c("Fósiles/Térmica No Ren.", "Nuclear", "Hidráulica", "Eólica", "Solar", "Otras Renovables", "Otros/Ajustes")
# Filtrar grupos que no existen en los datos tras la agregación
actual_groups <- unique(energy_agg_daily$Grupo_Energia)
source_levels_ordered <- intersect(source_levels_ordered, actual_groups) # Mantener solo los presentes
energy_agg_daily[, Grupo_Energia := factor(Grupo_Energia, levels = source_levels_ordered)]

# Verificar suma de porcentajes por día (debería ser cercano a 100)
checksum <- energy_agg_daily[, .(TotalPct = sum(Share_Agg_Pct)), by = Date]
if(any(checksum$TotalPct < 98.75 | checksum$TotalPct > 101.25)) {
    warning("La suma de porcentajes por día se desvía significativamente de 100%. Revisar agregación.")
    print(summary(checksum$TotalPct))
}

message("Datos agregados por Grupo de Energía y Día listos.")
print(tail(energy_agg_daily))

# --- 5. Crear Gráfico de Áreas Apiladas (Mix Generación Eléctrica) ---

# Definir textos 
source_text_day21 <- "Fuente: Red Eléctrica de España (REE) - Estructura de Generación Peninsular"
plot_title <- "Evolución del Mix de Generación Eléctrica Peninsular"
plot_subtitle <- paste0("Porcentaje de contribución de cada tecnología a la generación total.\nPeriodo: ",
                       format(min(energy_agg_daily$Date), "%Y-%m"), " - ", format(max(energy_agg_daily$Date), "%Y-%m"))

# Generar caption
caption_day21 <- generate_caption(
  day = 21,
  source_text = source_text_day21, 
  config = config,
  color_text_source = "#5c5c5c",
  color_text_author = "#757de8"
)

# Colores y tema (Asignar colores de week4_social a los grupos)
bg_col <- "#F2F2F2"
num_groups <- length(levels(energy_agg_daily$Grupo_Energia))
color_map_energy <- setNames(
    challenge_pal("week4_social")(num_groups), # Usa tu paleta social
    levels(energy_agg_daily$Grupo_Energia)     # Asigna a los niveles del factor
)

# Ajustar los colores
color_map_energy["Fósiles/Térmica No Ren."] <- "#333333"
color_map_energy["Nuclear"] <- challenge_palettes$week4_social['orange']
color_map_energy["Hidráulica"] <- challenge_palettes$week4_social['indigo']
color_map_energy["Eólica"] <- challenge_palettes$week4_social['satin_gold'] 
color_map_energy["Solar"] <- challenge_palettes$week4_social['pink']
color_map_energy["Otras Renovables"] <- challenge_palettes$week4_social['teal'] 
color_map_energy["Otros/Ajustes"] <- "#5c5c5c" 

# Crear el gráfico de áreas
gg_day21 <- ggplot(energy_agg_daily[Date >= "2015-01-01"], aes(x = Date, y = Roll_Mean_Pct, fill = Grupo_Energia, group = Grupo_Energia)) +
  geom_area(alpha = 0.9, position = "stack", color = alpha("white", 0.3), linewidth=0.05) + # Borde blanco muy fino

  # Escala de relleno manual
  scale_fill_manual(values = color_map_energy, name = "Tecnología") +

  # Formato de ejes
  scale_y_continuous(labels = scales::label_percent(scale = 1), expand = expansion(mult=c(0, 0.01))) +
  scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = expansion(mult=c(0.01, 0.01))) +

  # Aplicar tema
  theme_week4_social(base_family = "Lato", base_size = 10) +

  # Leyenda (invertir para que coincida con el stack)
  guides(fill = guide_legend(reverse = TRUE)) +

  # Etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Fecha",
    y = "Porcentaje Generación (%)",
    caption = caption_day21
  ) +
  theme(legend.title = element_text(hjust = 0.5))


# --- 6. Guardar Gráfico ---
output_file <- file.path(output_path, "day_21_fossils_energy_mix_ree.png")
ggsave(
  filename = output_file,
  plot = gg_day21,
  width = 1200, height = 1200, units = "px", dpi = 150, bg = bg_col # Apaisado
)

message("Gráfico del Día 21 (Fossils Energy Mix REE) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_21_fossils.R ---
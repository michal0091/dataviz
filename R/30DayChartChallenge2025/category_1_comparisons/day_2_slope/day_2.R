# --- Day 2: Slope (IPV vs Salario Modal Constante) ---
# #30DayChartChallenge 2025
# Autor: Michal Kinel (Datos: INE)

# --- 1. Cargar Librerías ---
library(ggplot2)
library(data.table) # Usaremos data.table para eficiencia
library(stringr)    # Para extraer año de periodo
library(yaml)
library(showtext)
library(ggtext)
library(scales)     # Para formateo ejes

# --- 2. Cargar Configuración, Temas y Utilidades ---
themes_file <- "R/30DayChartChallenge2025/themes_30DCC2025.R"
utils_file <- "R/30DayChartChallenge2025/utils.R"
config_file <- "R/30DayChartChallenge2025/30DayChartChallenge2025.yml"
font_path_fa <- "fonts/fa-brands-400.ttf"
output_path <- "R/30DayChartChallenge2025/plots/"
data_file <- "R/30DayChartChallenge2025/data/day_1.csv" 


salario_file <- "R/30DayChartChallenge2025/data/salario_modal.csv"
ipv_file <- "R/30DayChartChallenge2025/data/ipv_trimestral.csv"
ipc_file <- "R/30DayChartChallenge2025/data/ipca_mensual.csv"

# Verificar archivos
if (!file.exists(themes_file)) stop("Archivo de temas no encontrado.")
if (!file.exists(utils_file)) stop("Archivo de utilidades no encontrado.")
if (!file.exists(config_file)) stop("Archivo de configuración no encontrado.")
if (!file.exists(salario_file)) stop("Archivo de datos salario_modal.csv no encontrado.")
if (!file.exists(ipv_file)) stop("Archivo de datos ipv_trimestral.csv no encontrado.")
if (!file.exists(ipc_file)) stop("Archivo de datos ipca_mensual.csv no encontrado.")

source(themes_file)
source(utils_file)
config <- read_yaml(config_file)

# --- 3. Configurar Fuentes ---
setup_fonts(fa_brands_path = font_path_fa)

# --- 4. Cargar y Procesar Datos ---

# Cargar Salario Modal 
dt_salario <- fread(salario_file, sep = ";", dec = ",", na.strings = '""', encoding = "Latin-1", header = TRUE)
# Limpiar nombres si es necesario 
setnames(dt_salario, c("Sexo_Brecha", "Tipo_Salario", "Periodo", "Total"))
# Convertir Total a número
dt_salario[, Total := as.numeric(str_replace(str_remove(Total, "\\."), ",", "."))]
# Filtrar Hombres Y Mujeres, periodo relevante
dt_salario <- dt_salario[Sexo_Brecha %in% c("Hombres", "Mujeres") & Periodo >= 2009 & Periodo <= 2021,
                             .(Year = Periodo, Sexo = Sexo_Brecha, Salario_Corriente = Total)]


# Cargar IPV Trimestral
dt_ipv <- fread(ipv_file, sep = ";", dec = ",", na.strings = '""', encoding = "Latin-1", header = TRUE)
# Limpiar nombres y seleccionar columnas 
setnames(dt_ipv, old=c(1,2,3,4,5,6), new=c("TipoIndice", "AmbitoNacional", "AmbitoCCAA", "TipoVivienda", "Periodo", "Total"))
# Extraer año y trimestre
dt_ipv[, Year := as.integer(str_extract(Periodo, "^[0-9]{4}"))]
# Calcular media anual
dt_ipv_anual <- dt_ipv[TipoIndice == "Índice" & AmbitoNacional == "Nacional" & TipoVivienda == "General" & Year >= 2009 & Year <= 2021,
                       .(IPV_Index_2015 = mean(Total, na.rm = TRUE)), by = Year]

# Cargar IPCA Mensual
dt_ipc <- fread(ipc_file, sep = ";", dec = ",", na.strings = '""', encoding = "Latin-1", header = TRUE)
# Limpiar nombres y seleccionar
setnames(dt_ipc, old=c(1,2,3,4), new=c("TipoIndice", "TipoGeneral", "Periodo", "Total"))
# Extraer año
dt_ipc[, Year := as.integer(str_extract(Periodo, "^[0-9]{4}"))]
# Calcular media anual
dt_ipc_anual <- dt_ipc[TipoIndice == "Índice" & TipoGeneral == "Índice general" & Year >= 2009 & Year <= 2021,
                       .(IPC_Index_2015 = mean(Total, na.rm = TRUE)), by = Year]

# --- 5. Unir Datos y Calcular Índices Comparables ---

# Unir Salarios con IPC Anual
dt_merged <- merge(dt_salario, dt_ipc_anual, by = "Year")

# Calcular Salario en Euros Constantes 2015 (para hombres y mujeres)
dt_merged[, Salario_Constante_2015 := Salario_Corriente / (IPC_Index_2015 / 100)]

# Calcular Índices de Salario Constante (Base 2015 = 100) POR SEXO
# Usamos un join para obtener el valor base de 2015 para cada sexo
dt_base_2015 <- dt_merged[Year == 2015, .(Sexo, Salario_Base_2015 = Salario_Constante_2015)]
dt_merged <- merge(dt_merged, dt_base_2015, by = "Sexo")
dt_merged[, Salario_Index_2015 := (Salario_Constante_2015 / Salario_Base_2015) * 100]


# --- 6. Preparar Datos para Plotting ---
# Seleccionar y preparar índice IPV
dt_plot_ipv <- dt_ipv_anual[, .(Year, Serie = "Precios Vivienda (IPV)", Indice = IPV_Index_2015)]

# Seleccionar y preparar índices de Salario
dt_plot_salario <- dt_merged[, .(Year,
                                 Serie = fcase(Sexo == "Hombres", "Salario Modal Hombres",
                                               Sexo == "Mujeres", "Salario Modal Mujeres",
                                               default = as.character(Sexo)), # Por si acaso
                                 Indice = Salario_Index_2015)]

# Combinar todo
dt_plot <- rbindlist(list(dt_plot_ipv, dt_plot_salario))

# Asegurar orden de factores para la leyenda
dt_plot[, Serie := factor(Serie, levels = c("Precios Vivienda (IPV)",
                                            "Salario Modal Hombres",
                                            "Salario Modal Mujeres"))]
# Seleccionar los datos desde el 2015
dt_plot <- dt_plot[Year >= 2015]

# --- 7. Crear Gráfico ---

# Definir textos
source_text_day2 <- "INE: IPV (base 2015), Enc. Estruct. Salarial, IPCA (base 2015)"
plot_title <- "Precios Vivienda vs Salario Modal en España"
plot_subtitle <- "Índices Base 2015=100\nSalarios modales en euros constantes 2015"

# Generar caption
caption_day2 <- generate_caption(day = 2, source_text = source_text_day2, config = config)

# Colores específicos para las 3 series (usando paleta week1)
colores_series <- c(
  "Precios Vivienda (IPV)" = paleta_week1[2], # Gris oscuro
  "Salario Modal Hombres" = paleta_week1[5], # Azul 
  "Salario Modal Mujeres" = paleta_week1[4]  # Rojo
)

# Crear el objeto ggplot
gg_day2 <- ggplot(dt_plot, aes(x = Year, y = Indice, color = Serie)) +
  geom_line(linewidth = 1.05) +
  geom_point(size = 1.4) +
  # Aplicar colores manualmente
  scale_color_manual(values = colores_series) +
  # Línea de referencia en 100
  geom_hline(yintercept = 100, linetype = "dashed", color = "grey50") +
  # Aplicar tema personalizado
  theme_week1(base_size = 12) +
  # Añadir etiquetas y caption
  labs(
    title = plot_title,
    subtitle = plot_subtitle,
    x = "Año",
    y = "Índice (Base 2015 = 100)",
    color = NULL,
    caption = caption_day2
  ) +
  theme(legend.position = "top", # Posición leyenda arriba
        legend.justification = "left") # Justificar leyenda a la izquierda

# --- 8. Guardar Gráfico ---
output_file <- file.path(output_path, "day_2_slope_ipv_vs_salario_HM.png") # Nuevo nombre
ggsave(
  filename = output_file,
  plot = gg_day2,
  width = 1200, height = 1200, units = "px", dpi = 300, bg = "white"
)

message("Gráfico del Día 2 (Slope H/M) guardado en: ", normalizePath(output_file, mustWork = FALSE))

# --- Fin day_2.R ---
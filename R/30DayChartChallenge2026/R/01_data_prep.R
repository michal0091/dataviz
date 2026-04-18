# =============================================================================
# 01_data_prep.R — Plantillas de preparación de datos
# #30DayChartChallenge 2026
#
# Convención de nombres:
#   prep_raw_diaXX()   → lee el CSV crudo y devuelve un data.table sin tocar
#   prep_datos_diaXX() → limpia y transforma; recibe el raw, devuelve el clean
# =============================================================================
library(data.table)
library(logger)
library(stringr)
library(ggridges)
library(climaemet)
library(tidygraph)
library(igraphdata)
library(quantmod)
library(dplyr)
library(lubridate)
library(PerformanceAnalytics)
library(MacroFilters)
library(ichimoku)
library(WDI)
library(rugarch)


# =============================================================================
# PLANTILLA DÍA XX — sustituir XX por el número de día
# =============================================================================

#' Lee el CSV crudo del Día XX.
#'
#' @param path Ruta al archivo CSV (relativa a la raíz del proyecto).
#' @return data.table con los datos crudos.
#'
prep_raw_diaXX <- function(path) {
  logger::log_info("[Día XX] Leyendo datos desde: {path}")

  dt <- data.table::fread(path, encoding = "UTF-8")

  logger::log_info("[Día XX] Filas leídas: {nrow(dt)} | Columnas: {ncol(dt)}")
  dt
}


#' Limpia y transforma los datos del Día XX.
#'
#' Pasos típicos:
#'   1. Renombrar columnas a snake_case.
#'   2. Convertir tipos (fechas, factores, numéricos).
#'   3. Filtrar/eliminar filas inválidas.
#'   4. Crear variables derivadas.
#'
#' @param dt_raw data.table devuelto por prep_raw_diaXX().
#' @return data.table limpio y listo para graficar.
#'
prep_datos_diaXX <- function(dt_raw) {
  logger::log_info("[Día XX] Iniciando limpieza de datos ({nrow(dt_raw)} filas)")

  dt <- data.table::copy(dt_raw)

  # --- Renombrar columnas (ajustar según los nombres reales) -----------------
  # data.table::setnames(dt, old = c("OldName1", "OldName2"),
  #                          new = c("new_name1", "new_name2"))

  # --- Conversión de tipos ---------------------------------------------------
  # dt[, fecha := lubridate::ymd(fecha)]
  # dt[, valor := as.numeric(valor)]
  # dt[, categoria := as.factor(categoria)]

  # --- Filtrado / limpieza ---------------------------------------------------
  # dt <- dt[!is.na(valor)]
  # dt <- dt[valor > 0]

  # --- Variables derivadas ---------------------------------------------------
  # dt[, pct := valor / sum(valor) * 100, by = grupo]
  # dt[, etiqueta := stringr::str_wrap(categoria, width = 20)]

  # --- Ordenar ---------------------------------------------------------------
  # data.table::setorder(dt, -valor)

  logger::log_success("[Día XX] Limpieza completada ({nrow(dt)} filas)")
  dt
}


# =============================================================================
# DÍA 01 — Part-to-Whole (Comparisons)
# =============================================================================

prep_dia01_satelites <- function(url_tsv) {
  
  log_info("Día 01: Descargando y leyendo TSV en vivo desde {url_tsv}")
  
  # 1. Leemos el archivo
  dt <- tryCatch({
    fread(url_tsv, header = FALSE, fill = TRUE)
  }, error = function(e) {
    log_error("Fallo al descargar el TSV de GCAT. Error: {e$message}")
    stop("Deteniendo pipeline del Día 01.")
  })
  
  # 2. Sacamos el diccionario de nombres
  nombres_gcat <- unname(as.matrix(dt)[1,])

  # Quitar 2 primeras filas y asignar nombres 
  dt <- dt[3:.N, ]
  setnames(dt, old = names(dt), new = nombres_gcat)

  log_info("Datos GCAT parseados. Total de objetos activos: {nrow(dt)}")
  
  # 4. Lipiar dueño (El Part-to-Whole)
  dt[, owner_low := tolower(Owner)]
  
  # Listas de agrupación
  us_gov <- c("nroc", "nroc/usn", "sda", "afsmc", "afrl", "gsfc", "jpl", "jsc", "noaa", "dod", "nasa")
  
  china_all <- c("zxw", "yuanx", "cgstl", "zlzb", "zzb", "cnsa", "cnsas", "geesp", 
                 "sitro", "casc", "cast", "yyao", "he360", "guog", "tianmu", "sast", 
                 "nsmc", "piesat", "siwei", "guox/zjlab", "ningx/casc")
  
  big_commercial <- c("plan", "plnsst", "irids", "spire", "eutsa", "iceye", 
                      "iceus", "intelu", "inteld", "kineis", "o3b", "o3bs", 
                      "dorbit", "globl", "unseen")
  
  oneweb_all <- c("oneweb", "onewebn", "onewebe")

  # El fcase ultrarrápido
  dt[, categoria_limpia := fcase(
    owner_low == "spxs", "SpaceX (Starlink)",
    owner_low %chin% oneweb_all, "OneWeb",
    owner_low == "kuip", "Amazon Kuiper",
    owner_low %chin% china_all, "China (Gov & Constelaciones)",
    owner_low %chin% us_gov, "EE.UU. (NASA, Militar, Inteligencia)",
    owner_low %chin% big_commercial, "Otros Gigantes Comerciales",
    default = "Resto del Mundo (Agencias y Univ.)"
  )]
  
  dt[, owner_low := NULL] # Liberamos memoria
  
  # 5. Agregar
  dt_resumen <- dt[, .(total_satelites = .N), by = categoria_limpia]
  dt_resumen[, pct := total_satelites / sum(total_satelites)]
  
  # 6. Ordenar
  setorder(dt_resumen, -total_satelites)
  niveles <- c(setdiff(dt_resumen$categoria_limpia, "Resto del Mundo (Agencias y Univ.)"), 
               "Resto del Mundo (Agencias y Univ.)")
  dt_resumen[, categoria_limpia := factor(categoria_limpia, levels = niveles)]
  
  # 7. LAbels
  dt_resumen[, label := paste0(categoria_limpia, "\n", round(pct*100, 1), "%")]
  
  log_success("Día 01 preparado. SpaceX domina con {dt_resumen[categoria_limpia == 'SpaceX (Starlink)', total_satelites]} satélites.")
  
  dt_resumen[]
}


# =============================================================================
# DÍA 02 — Pictogram (Comparisons)
# =============================================================================

prep_dia02_gaming <- function() {
  
  log_info("Día 02: Generando datos de ventas de consolas.")
  
  # Datos manuales (millones de unidades vendidas aprox)
  dt <- data.table(
    consola = c("PlayStation 2", "Nintendo DS", "Nintendo Switch", "Game Boy"),
    ventas_m = c(160, 155, 154, 118)
  )
  
  # Fijamos el orden del factor para que aparezcan de mayor a menor en el gráfico
  dt[, consola := factor(consola, levels = dt$consola)]
  
  # La magia del Waffle Chart en data.table
  escala <- 2 # 1 icono = 2 millones de consolas 
  columnas_max <- 30 # Cuántos iconos de ancho tendrá cada bloque
  
  dt[, iconos_totales := round(ventas_m / escala)]
  
  # Expandimos la tabla: de 4 filas pasamos a cientos de coordenadas exactas
  dt_grid <- dt[, .(id_icono = 1:iconos_totales), by = .(consola, ventas_m)]
  
  # Calculamos X e Y para hacer la cuadrícula
  dt_grid[, x := (id_icono - 1) %% columnas_max]
  dt_grid[, y := -((id_icono - 1) %/% columnas_max)] 
  
  log_success("Día 02 preparado. Cuadrícula de {nrow(dt_grid)} iconos generada.")
  
  dt_grid[]
}


# =============================================================================
# DÍA 03 — Mosaic (Comparisons)
# =============================================================================

prep_dia03_energia <- function() {
  
  url_owid <- "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv"
  # Diccionario ISO a Continente
  url_dict <- "https://raw.githubusercontent.com/lukes/ISO-3166-Countries-with-Regional-Codes/master/all/all.csv"
  
  log_info("Día 03: Descargando datos de energía OWID y diccionario ISO...")
  
  # Link datos
  dt_raw <- fread(url_owid)
  dt_dict <- fread(url_dict)
  
  # Limpierza
  dt_dict <- dt_dict[, .(`alpha-3`, region)]
  setnames(dt_dict, c("alpha-3", "region"), c("iso_code", "continente"))
  dt_year <- dt_raw[year == 2022]
  dt_paises <- dt_year[iso_code != ""]
  
  # Merge
  dt_cruce <- merge(dt_paises, dt_dict, by = "iso_code", all.x = TRUE)
  
  # Limpiamos NAs
  dt_cruce <- dt_cruce[!is.na(continente) & continente != ""]

  # Fusionar África y Oceanía y traducir
  dt_cruce[, continente := fcase(
    continente == "Americas", "América",
    continente == "Europe", "Europa",
    continente == "Asia", "Asia",
    continente  %in% c("Africa", "Oceania"), "Resto del Mundo"
  )]
  
  
  # Manejo de datos 
  # Calculamos Limpia vs Fósil por país
  dt_cruce[, Limpia := fcoalesce(low_carbon_electricity, 0)]
  dt_cruce[, Fósil := fcoalesce(fossil_electricity, 0)]
  
  # Sumamos los TWh agrupando por nuestro nuevo diccionario de continentes
  dt_agg <- dt_cruce[, .(
    Limpia = sum(Limpia, na.rm = TRUE),
    Fósil = sum(Fósil, na.rm = TRUE)
  ), by = continente]
  
  # Pasamos a formato largo para el plot
  dt_melt <- melt(dt_agg, 
                  id.vars = "continente", 
                  measure.vars = c("Fósil", "Limpia"), 
                  variable.name = "tipo", 
                  value.name = "consumo_twh")
  
  dt <- dt_melt[consumo_twh > 0]
  
  # 5. Geometría para el mosaico
  dt[, tipo := factor(tipo, levels = c("Fósil", "Limpia"))]
  
  # Eje X (Ancho: % del consumo mundial total)
  dt[, total_region := sum(consumo_twh), by = continente]
  dt_regiones <- unique(dt[, .(continente, total_region)])
  setorder(dt_regiones, -total_region) 
  
  dt_regiones[, pct_x := total_region / sum(total_region)]
  dt_regiones[, xmax := cumsum(pct_x)]
  dt_regiones[, xmin := shift(xmax, n = 1, fill = 0)]
  
  dt <- merge(dt, dt_regiones[, .(continente, xmin, xmax)], by = "continente")
  
  # Eje Y (Alto: % de Fósil vs Limpia dentro del continente)
  setorder(dt, -total_region, tipo)
  dt[, pct_y := consumo_twh / total_region]
  
  dt[, ymax := cumsum(pct_y), by = continente]
  dt[, ymin := shift(ymax, n = 1, fill = 0), by = continente]
  
  # Coordenadas centrales para inyectar los textos en ggplot2
  dt[, x_mid := xmin + (xmax - xmin) / 2]
  dt[, y_mid := ymin + (ymax - ymin) / 2]

  log_success("Día 03: Geometría calculada. Continentes mapeados por diccionario.")
  
  dt[]
}


# =============================================================================
# DÍA 04 — Slope (Comparisons)
# =============================================================================

prep_dia04_slope <- function() {
  
  log_info("Día 04: Extrayendo datos para el Slope Chart (2000 vs 2022).")
  
  url_owid <- "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv"
  dt_raw <- fread(url_owid)
  
  # Selección de interés
  paises_clave <- c("Spain", "Germany", "United States", "China", "India", "Russia")
  anios_analisis <- c(2000, 2022)
  
  dt_filtrado <- dt_raw[country %in% paises_clave & year %in% anios_analisis, 
                        .(country, year, renewables_share_elec)]
  # Traducir
  dt_filtrado[, country_es := fcase(
    country == "Spain", "España",
    country == "Germany", "Alemania",
    country == "United States", "Estados Unidos",
    country == "China", "China",
    country == "India", "India",
    country == "Russia", "Rusia",
    default = country
  )]
  
  # Long to wide
  dt_wide <- dcast(dt_filtrado, country + country_es ~ year, value.var = "renewables_share_elec")
  setnames(dt_wide, c("2000", "2022"), c("y2000", "y2022"))
  
  # Clasificar
  dt_wide[, trend := fcase(
    y2022 - y2000 > 20, "Aceleración Masiva",  # Suben más de 20 puntos
    y2022 - y2000 > 5, "Progreso Constante",   # Suben entre 5 y 20
    default = "Estancamiento o Caída"          # No mejoran o empeoran
  )]
  
  # Wide to long
  dt_long <- melt(dt_wide, id.vars = c("country", "country_es", "trend"), 
                  measure.vars = c("y2000", "y2022"), 
                  variable.name = "year_cat", value.name = "share")
  
  # Limpiar
  dt_long[, year := fifelse(year_cat == "y2000", 2000, 2022)]
  dt_long[, label := sprintf("%s (%s%%)", country_es, round(share))]
  
  log_success("Día 04: Datos del Slope Chart listos. {nrow(dt_long)} puntos de anclaje generados.")
  
  dt_long[]
}


# =============================================================================
# DÍA 05 — Experimental (Comparisons)
# =============================================================================


prep_dia05_experimental <- function() {
  
  log_info("Día 05: Extrayendo datos para el Dumbbell Polar (G20).")
  
  url_owid <- "https://raw.githubusercontent.com/owid/energy-data/master/owid-energy-data.csv"
  dt_raw <- fread(url_owid)
  
  # Países G20
  paises_g20 <- c("United States", "China", "Japan", "Germany", "India", "United Kingdom", 
                  "France", "Italy", "Brazil", "Canada", "South Korea", "Russia", "Australia",
                  "Mexico", "Indonesia", "Saudi Arabia", "Turkey", "Argentina", "South Africa")
  
  dt_filt <- dt_raw[country %in% paises_g20 & year %in% c(2000, 2022), 
                    .(country, year, energy_per_capita)]
  
  # Traducir
  dt_filt[, country := fcase(
    "United States" == country, "Estados Unidos",
    "China" == country, "China",
    "Japan" == country, "Japón",
    "Germany" == country, "Alemania",
    "India" == country, "India",
    "United Kingdom" == country, "Reino Unido",
    "France" == country, "Francia",
    "Italy" == country, "Italia",
    "Brazil" == country, "Brasil",
    "Canada" == country, "Canadá",
    "South Korea" == country, "Corea del Sur",
    "Russia" == country, "Rusia",
    "Australia" == country, "Australia",
    "Mexico" == country, "México",
    "Indonesia" == country, "Indonesia",
    "Saudi Arabia" == country, "Arabia Saudita",
    "Turkey" == country, "Turquía",
    "Argentina" == country, "Argentina",
    "South Africa" == country, "Sudáfrica",
    default = country
  )]
  
  # Long to wide
  dt_wide <- dcast(dt_filt, country ~ year, value.var = "energy_per_capita")
  setnames(dt_wide, c("2000", "2022"), c("y2000", "y2022"))
  
  # Limpiamos posibles NAs y ordenamos por el valor actual para que el círculo tenga un gradiente de tamaño
  dt_wide <- dt_wide[!is.na(y2000) & !is.na(y2022)]
  setorder(dt_wide, y2022)
  
  # Factorizar
  dt_wide[, country := factor(country, levels = dt_wide$country)]
  
  # Calcular los ángulos para las etiquetas polares
  dt_wide[, id := 1:.N]
  dt_wide[, angle := 90 - 360 * (id - 0.5) / .N]
  
  # Ajustar texto cambio en 180 grados
  dt_wide[, hjust := fifelse(angle < -90, 1, 0)]
  dt_wide[, angle := fifelse(angle < -90, angle + 180, angle)]
  
  log_success("Día 05: Datos polares listos. {nrow(dt_wide)} órbitas calculadas.")
  
  dt_wide[]
}


# =============================================================================
# DÍA 06 — Reporters Without Borders (Comparisons)
# =============================================================================

prep_dia06_rsf <- function() {
  
  log_info("Día 06: Leyendo y limpiando índice real de RSF...")
  
  archivo_local <- "R/30DayChartChallenge2026/data/rsf_index_2025.csv" 
  dt_raw <- fread(archivo_local, dec = ",", encoding = "Latin-1")
  
  # Renombrar
  setnames(dt_raw, 
           old = c("Country_ES", "Score 2025", "Zone"), 
           new = c("country", "score", "zona_original"))
  
  # Traducir zonas
  diccionario_zonas <- data.table(
    zona_original = c("Afrique", "Amériques", "Asie-Pacifique", "EEAC", "MENA", "UE Balkans"),
    continente = c("África", "América", "Asia-Pacífico", "Europa del Este\ny Asia Central", "Oriente Medio\ny Norte de África", "UE y Balcanes")
  )
  dt_raw <- merge(dt_raw, diccionario_zonas, by = "zona_original", all.x = TRUE)
  
  # Clasificar según RSF
  dt_raw[, categoria := fcase(
    score >= 85, "Buena",
    score >= 70 & score < 85, "Satisfactoria",
    score >= 55 & score < 70, "Problemática",
    score >= 40 & score < 55, "Difícil",
    score < 40, "Muy Grave"
  )]
  
  niveles_rsf <- c("Buena", "Satisfactoria", "Problemática", "Difícil", "Muy Grave")
  dt_raw[, categoria := factor(categoria, levels = niveles_rsf)]
  
  # Ordenar zonas por mediana
  dt_medianas <- dt_raw[, .(mediana = median(score, na.rm = TRUE)), by = continente]
  setorder(dt_medianas, mediana)
  dt_raw[, continente := factor(continente, levels = dt_medianas$continente)]
  
  # Labels
  setorder(dt_raw, continente, -score)
  dt_raw[, etiqueta := ""]
  dt_raw[, id_rango := 1:.N, by = continente]
  dt_raw[id_rango == 1 | id_rango == .N, etiqueta := country]
  
  log_success("Día 06: Datos RSF reales procesados. {nrow(dt_raw)} países listos para plotear.")
  
  dt_raw[]
}


# =============================================================================
# DÍA 07 — Multiscale (Distributions)
# =============================================================================

options(timeout = 600)
prep_dia07_renta_aeat <- function() {
  
  log_info("Día 07: Leyendo microdatos de Renta Municipal (AEAT)...")
  
  link_csv <- "https://www.ine.es/jaxiT3/files/t/es/csv_bdsc/30824.csv?nocab=1"
  dt_raw <- fread(link_csv, dec = ",")
  
  # Filtrar y limpiar
  dt_municipios <- dt_raw[
    Distritos == "" & 
    Secciones == "" & 
    Periodo == 2023 & 
    `Indicadores de renta media` == "Renta neta media por persona"
  ]

  # Extraer los 2 primeros dígitos  Provincia
  dt_municipios[, prov_code := str_extract(Municipios, "^\\d{2}")]

  dic_ccaa <- data.table(
    prov_code = sprintf("%02d", 1:52),
    ccaa = c(
      "País Vasco", "Castilla-La Mancha", "C. Valenciana", "Andalucía", "Castilla y León",
      "Extremadura", "Islas Baleares", "Cataluña", "Castilla y León", "Extremadura",
      "Andalucía", "C. Valenciana", "Castilla-La Mancha", "Andalucía", "Galicia",
      "Castilla-La Mancha", "Cataluña", "Andalucía", "Castilla-La Mancha", "País Vasco",
      "Andalucía", "Aragón", "Andalucía", "Castilla y León", "Cataluña",
      "La Rioja", "Galicia", "Madrid", "Andalucía", "Murcia",
      "Navarra", "Galicia", "Asturias", "Castilla y León", "Canarias",
      "Galicia", "Castilla y León", "Canarias", "Cantabria", "Castilla y León",
      "Andalucía", "Castilla y León", "Cataluña", "Aragón", "Castilla-La Mancha",
      "C. Valenciana", "Castilla y León", "País Vasco", "Castilla y León", "Aragón",
      "Ceuta", "Melilla"
    )
  )
  dic_nuts1 <- data.table(
    ccaa = c("Galicia", "Asturias", "Cantabria", 
             "País Vasco", "Navarra", "La Rioja", "Aragón", 
             "Madrid", 
             "Castilla y León", "Castilla-La Mancha", "Extremadura", 
             "Cataluña", "C. Valenciana", "Islas Baleares", 
             "Andalucía", "Murcia", "Ceuta", "Melilla", 
             "Canarias"),
    nuts1 = c(rep("Noroeste", 3), 
              rep("Noreste", 4), 
              "Comunidad de Madrid", 
              rep("Centro", 3), 
              rep("Este", 3), 
              rep("Sur", 4), 
              "Canarias")
  )
  
  dt_municipios <- merge(dt_municipios, dic_ccaa, by = "prov_code", all.x = TRUE)
  dt_municipios <- merge(dt_municipios, dic_nuts1, by = "ccaa", all.x = TRUE)
  
  setnames(dt_municipios, "Total", "renta", skip_absent = TRUE)
  dt_municipios[, renta :=as.numeric(gsub("\\.", "", renta))]
  dt_municipios <- dt_municipios[!is.na(renta) & renta > 0]

  # Total
  dt_macro <- copy(dt_municipios)
  dt_macro[, nuts1 := "TOTAL NACIONAL"]
  
  dt_final <- rbindlist(list(dt_macro, dt_municipios))
  
  # Ordenar de mayor a menor renta (aprox) para que visualmente el gráfico tenga sentido
  orden <- c("TOTAL NACIONAL", "Comunidad de Madrid", "Noreste", "Este", "Noroeste", "Centro", "Canarias", "Sur")
  dt_final[, nuts1 := factor(nuts1, levels = rev(orden))]
  
  log_success("Día 07: Agrupación NUTS 1 completada. {nrow(dt_final)} registros listos.")
  
  dt_final[]
}


# =============================================================================
# DÍA 08 — Circular (Distributions)
# =============================================================================

prep_dia08_circular <- function() {
  
  log_info("Día 08: Procesando microdatos de nacimientos INE (2024)...")

  archivo_microdatos <- "R/30DayChartChallenge2026/data/MNPnacim_2024.RData" 
  
  if (!file.exists(archivo_microdatos)) {
    log_error("FALTA EL ARCHIVO: Guarda los microdatos del INE en {archivo_microdatos}")
    stop("Pipeline detenido.")
  }
  load(archivo_microdatos)
  dt_micro <- as.data.table(df_micro)
  dt_micro[, MESPAR := as.integer(MESPAR)]
  dt_clean <- dt_micro[MESPAR >= 1 & MESPAR <= 12]

  dt_clean[, is_cesarea := fifelse(as.character(CESAREA) %in% c("1", "S", "s"), 1, 0)]
  
  # Agragar por mes
  dt_mes <- dt_clean[, .(
    total_nacimientos = .N,
    total_cesareas = sum(is_cesarea, na.rm = TRUE)
  ), by = MESPAR]

  # Calculamos el % de cesáreas
  dt_mes[, pct_cesarea := total_cesareas / total_nacimientos]
  
  setorder(dt_mes, MESPAR)
  
  # Etiquetar meses
  meses_labels <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", 
                    "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  dt_mes[, mes_nombre := meses_labels[MESPAR]]
  
  # Identificar el pico de nacimientos
  mes_pico <- dt_mes[which.max(pct_cesarea), MESPAR]
  dt_mes[, color_flag := fifelse(MESPAR == mes_pico, "Pico", "Normal")]
  
  log_success("Día 08 preparado. {sum(dt_mes$nacimientos)} nacimientos agrupados por mes.")
  
  dt_mes[]
}


# =============================================================================
# DÍA 09 — Wealth (Distributions)
# =============================================================================

prep_dia09_wealth <- function() {
  
  log_info("Día 09: Procesando microdatos de Riqueza (Forbes Billionaires)...")
  
  archivo_local <- "R/30DayChartChallenge2026/data/billionaires_2024.csv"
  
  if (!file.exists(archivo_local)) {
    log_error("FALTA EL ARCHIVO: Descarga el CSV de Forbes Billionaires y guárdalo en {archivo_local}")
    stop("Pipeline detenido: Necesitamos datos reales.")
  }
  
  dt_raw <- fread(archivo_local)

  setnames(dt_raw, 
           old = c("2024 Net Worth", "Industry"), 
           new = c("net_worth_raw", "sector"), 
           skip_absent = TRUE)
  
  # Regex
  # Eliminar el "$" y la "B", y convertimos a numérico
  dt_raw[, riqueza_b := as.numeric(str_remove_all(net_worth_raw, "[\\$B]"))]
  
  # Filtrar NAs
  dt_clean <- dt_raw[!is.na(riqueza_b) & !is.na(sector)]

  # Filtrar
  sectores_top <- dt_clean[, .N, by = sector][order(-N)][1:6, sector]
  dt_top <- dt_clean[sector %in% sectores_top]
  
  dt_medianas <- dt_top[, .(mediana = median(riqueza_b)), by = sector]
  setorder(dt_medianas, mediana)
  
  dt_top[, sector := factor(sector, levels = dt_medianas$sector)]
  
  log_success("Día 09 preparado. Extracción limpia de {nrow(dt_top)} milmillonarios del top 6 sectores.")
  
  dt_top[]
}


# =============================================================================
# DÍA 10 — Pop Culture (Distributions)
# =============================================================================


prep_dia10_popculture <- function() {
  
  log_info("Día 10: Procesando los volcados masivos de IMDb para Family Guy...")
  
  archivo_episodios <- "R/30DayChartChallenge2026/data/title.episode.tsv.gz"
  archivo_notas <- "R/30DayChartChallenge2026/data//title.ratings.tsv.gz"
  
  if (!file.exists(archivo_episodios) | !file.exists(archivo_notas)) {
    log_error("FALTAN LOS ARCHIVOS DE IMDB (https://datasets.imdbws.com). Descarga 'title.episode.tsv.gz' y 'title.ratings.tsv.gz' en la carpeta data/")
    stop("Pipeline detenido.")
  }
  
  # Leer parcialmente
  dt_episodes <- fread(archivo_episodios, select = c("tconst", "parentTconst", "seasonNumber"))
  
  # Filtramos solo los episodios cuyo padre es "Family Guy" (tt0182576)
  dt_fg_episodes <- dt_episodes[parentTconst == "tt0182576"]
  
  # Limpiamos temporadas inválidas (IMDb a veces pone "\\N" para especiales o nulos)
  dt_fg_episodes <- dt_fg_episodes[seasonNumber != "\\N"]
  dt_fg_episodes[, seasonNumber := as.integer(seasonNumber)]
  
  # Leer parcialmente rankings
  dt_ratings <- fread(archivo_notas, select = c("tconst", "averageRating", "numVotes"))
  
  # Merge
  dt_final <- merge(dt_fg_episodes, dt_ratings, by = "tconst")
  
  # Limpiar
  dt_final <- dt_final[numVotes > 100 & seasonNumber <= 23]
  
  niveles_temporadas <- sort(unique(dt_final$seasonNumber))
  dt_final[, season_factor := factor(seasonNumber, levels = rev(niveles_temporadas))]
  
  # Agrupamos en "Eras" para darle color al gráfico
  dt_final[, era := fcase(
    seasonNumber <= 3, "Fundación y Estabilidad (T1-T3)",
    seasonNumber >= 4 & seasonNumber <= 6, "Cénit Creativo (T4-T6)",
    seasonNumber >= 7 & seasonNumber <= 11, "Erosión del Formato (T7-T11)",
    seasonNumber >= 12, "La Era de la Fatiga (T12-T23)"
  )]
  
  # Reordenamos el factor para el gráfico
  orden_eras <- c(
    "Fundación y Estabilidad (T1-T3)", 
    "Cénit Creativo (T4-T6)", 
    "Erosión del Formato (T7-T11)", 
    "La Era de la Fatiga (T12-T23)"
  )
  dt_final[, era := factor(era, levels = orden_eras)]
  
  log_success("Día 10: Extraídos {nrow(dt_final)} episodios de Family Guy. Listos para graficar.")
  
  dt_final[]

}


# =============================================================================
# DÍA 11 — Physical (Distributions)
# =============================================================================

prep_dia11_physical <- function() {
  
  log_info("Día 11: Procesando microdatos físicos de atletas olímpicos...")
  
  archivo_local <- "R/30DayChartChallenge2026/data/dataset_olympics.csv"
  
  if (!file.exists(archivo_local)) {
    log_error("FALTA EL ARCHIVO: Descarga (https://www.kaggle.com/datasets/bhanupratapbiswas/olympic-data) 'dataset_olympics.csv' de Kaggle y guárdalo en data/")
    stop("Pipeline detenido.")
  }
  
  dt_raw <- fread(archivo_local)
  
  # Filtrar
  dt_clean <- dt_raw[Year >= 2000 & !is.na(Height)]
  dt_clean <- dt_clean[Sex == "M"] # Atletas masculinos 
  
  # Selección 
  deportes_clave <- c("Basketball", "Gymnastics", "Weightlifting", "Athletics", "Swimming")
  dt_extremos <- dt_clean[Sport %in% deportes_clave]
  
  # Traducir
  dt_extremos[, deporte_es := fcase(
    Sport == "Basketball", "Baloncesto",
    Sport == "Swimming", "Natación",
    Sport == "Athletics", "Atletismo (General)",
    Sport == "Weightlifting", "Halterofilia",
    Sport == "Gymnastics", "Gimnasia Artística"
  )]
  
  dt_medianas <- dt_extremos[, .(mediana_h = median(Height)), by = deporte_es]
  setorder(dt_medianas, mediana_h)
  
  # Factorizamos forzando el orden de más bajitos a más altos
  dt_extremos[, deporte_es := factor(deporte_es, levels = dt_medianas$deporte_es)]
  
  dt_final <- unique(dt_extremos, by = "ID")
  
  log_success("Día 11 preparado: {nrow(dt_final)} atletas físicos procesados.")
  
  dt_final[]
}


# =============================================================================
# DÍA 12 — FlowingData (Distributions)
# =============================================================================

prep_dia12_flowingdata <- function() {
  
  log_info("Día 12: Procesando la serie histórica de AEMET (Retiro)...")
  
  archivo_local <- "R/30DayChartChallenge2026/data/aemet_retiro.csv"
  
  if (!file.exists(archivo_local)) {
    log_error("FALTA EL ARCHIVO: Guarda tus datos de AEMET en {archivo_local}")
    stop("Pipeline detenido.")
  }
  
  dt_raw <- fread(archivo_local)
  
  # Limpieza
  dt_raw[, mes_num := month(fecha)]
  dt_clean <- dt_raw[!is.na(tmed), .(fecha, mes_num, tmed)]
  
  dic_meses <- data.table(
    mes_num = 1:12,
    mes_es = c("Enero", "Febrero", "Marzo", "Abril", "Mayo", "Junio", 
               "Julio", "Agosto", "Septiembre", "Octubre", "Noviembre", "Diciembre")
  )
  
  dt_clean <- merge(dt_clean, dic_meses, by = "mes_num")
  
  # Estadciones para paleta
  dt_clean[, estacion := fcase(
    mes_num %in% c(12, 1, 2), "Invierno",
    mes_num %in% c(3, 4, 5), "Primavera",
    mes_num %in% c(6, 7, 8), "Verano",
    mes_num %in% c(9, 10, 11), "Otoño"
  )]
  
  dt_clean[, mes_factor := factor(mes_es, levels = rev(dic_meses$mes_es))]
  dt_clean[, estacion := factor(estacion, levels = c("Invierno", "Primavera", "Verano", "Otoño"))]
  
  log_success("Día 12: {nrow(dt_clean)} días del Retiro procesados y listos para fluir.")
  
  dt_clean[]
}


# =============================================================================
# DÍA 13 — Ecosystems (Relationships)
# =============================================================================

prep_dia13_ecosystems <- function() {
  
  log_info("Día 13: Extrayendo la red trófica real de Chesapeake Bay...")
  
  data("foodwebs", package = "igraphdata")
  chesapeake <- foodwebs$Chesapeake
  
  dfs <- igraph::as_data_frame(chesapeake, what = "both")
  dt_edges <- as.data.table(dfs$edges)
  dt_nodes <- as.data.table(dfs$vertices)
  
  # Eliminamos sumideros químicos, entradas/salidas de energía y materia muerta
  nodos_no_vivos <- c(
    "Input", "Output", "Respiration", 
    "sediment particulate orga", "suspended particulate org", "dissolved organic carbon"
  )
  dt_nodes <- dt_nodes[!name %in% nodos_no_vivos]
  dt_edges <- dt_edges[!(from %in% nodos_no_vivos | to %in% nodos_no_vivos)]
  
  # Red
  in_degrees <- dt_edges[, .(presas_consumidas = .N), by = to]
  dt_nodes <- merge(dt_nodes, in_degrees, by.x = "name", by.y = "to", all.x = TRUE)
  dt_nodes[is.na(presas_consumidas), presas_consumidas := 0]
  
  # Superdepredador
  apex_name <- dt_nodes[which.max(presas_consumidas), name]
  dt_nodes[, es_apex := fifelse(name == apex_name, "Superdepredador", "Resto del Ecosistema")]
  
  # Aristas
  dt_edges[, is_apex_prey := fifelse(to == apex_name, "Alerta", "Base")]
  
  # GGRAPH
  ecosistema_graph <- tbl_graph(nodes = dt_nodes, edges = dt_edges, directed = TRUE)
  
  log_success("Día 13: Ecosistema purgado. Apex predator real: {apex_name}.")
  
  return(ecosistema_graph)
}


# =============================================================================
# DÍA 14 — Trade (Relationships)
# =============================================================================

prep_dia14_trade <- function(
    path_xls = NULL,  # si NULL, intenta descargarlo
    url_xls  = "https://img1.wsimg.com/blobby/go/e5e77e0b-59d1-44d9-ab25-4763ac982e53/downloads/7fd201b2-28ad-476c-bc67-7a2cab5304a3/ie_data.xls?ver=1775144929611"
) {
  
  # Descargar
  if (is.null(path_xls)) {
    path_xls <- file.path(tempdir(), "ie_data.xls")
    if (!file.exists(path_xls)) {
      log_info("Descargando ie_data.xls de Shiller...")
      download.file(url_xls, destfile = path_xls, mode = "wb", quiet = TRUE)
    } else {
      log_info("Usando caché local: {path_xls}")
    }
  }
  
  # Leer EXCEL
  raw <- readxl::read_excel(
    path      = path_xls,
    sheet     = "Data",
    skip      = 7,          # salta las 7 filas de notas
    col_names = TRUE,       # fila 8 = headers
    col_types = "text",     # leer todo como texto para no perder nada
    na        = c("", "NA", "N/A", ".")
  )
  
  dt <- as.data.table(raw)
  
  log_info("Columnas encontradas: {paste(names(dt)[1:15], collapse = ', ')}...")
  
  # Sacar columnas de interés
  # Función para encontrar columna por patrón
  find_col <- function(patterns) {
    for (p in patterns) {
      m <- grep(p, names(dt), ignore.case = TRUE, value = TRUE)
      if (length(m) > 0) return(m[1])
    }
    return(NULL)
  }
  
  col_date <- find_col(c("^Date$", "date", "Date"))
  col_gs10 <- find_col(c("GS10", "Rate GS10", "gs10", "10.*year", "yield"))
  col_cape <- find_col(c("^CAPE$", "P/E10", "cape", "CAPE"))
  col_price <- find_col(c("^P$", "^Price$"))
  col_earn  <- find_col(c("^E$", "Earnings"))
  
  log_info("Columnas mapeadas → Date:{col_date} | GS10:{col_gs10} | CAPE:{col_cape}")
  
  # Extraer datos y limpiar
  dt_clean <- dt[, .(
    fecha_dec = suppressWarnings(as.numeric(get(col_date))),
    price     = suppressWarnings(as.numeric(get(col_price))),
    earnings  = suppressWarnings(as.numeric(get(col_earn))),
    yield_10y = suppressWarnings(as.numeric(get(col_gs10))),
    cape      = suppressWarnings(as.numeric(get(col_cape)))
  )]
  
  # Eliminar filas vacías o con fecha inválida
  dt_clean <- dt_clean[!is.na(fecha_dec) & fecha_dec > 1800]
  
  # Limpiar fechas
  dt_clean[, anio := floor(fecha_dec)]
  dt_clean[, mes  := round((fecha_dec - anio) * 100)]
  dt_clean[, mes_ano := as.Date(paste(anio, mes, "01", sep = "-"))]
  dt_clean[, c("fecha_dec", "anio", "mes") := NULL]
  
  # Filtrar datos incompletos y períodos sin CAPE
  dt_clean <- dt_clean[complete.cases(yield_10y, cape, mes_ano)]
  
  # P/E simple: precio / earnings
  dt_clean[!is.na(price) & !is.na(earnings) & earnings > 0,
           pe_simple := price / earnings]
  
  # Clasificación por era
  eras <- c(
    "Pre-WWII (1881–1945)",
    "Posguerra / Expansión (1946–1999)",
    "Post-GFC / QE (2008–2019)",
    "COVID & Policrisis (2020–)"
  )
  
  dt_clean[, era_macro := factor(fcase(
    year(mes_ano) <= 1945,               eras[1],
    between(year(mes_ano), 1946, 2007),  eras[2],
    between(year(mes_ano), 2008, 2019),  eras[3],
    year(mes_ano) >= 2020,               eras[4]
  ), levels = eras)]
  
  # Diagnóstico
  r_total <- cor(dt_clean$yield_10y, dt_clean$cape, use = "complete.obs")
  
  log_success(glue(
    "CAPE listo: {nrow(dt_clean)} obs | ",
    "{min(dt_clean$mes_ano)} → {max(dt_clean$mes_ano)} | ",
    "r(CAPE, GS10) = {round(r_total, 3)}"
  ))
  
  dt_clean[]
}


# =============================================================================
# DÍA 15 — Correlation (Relationships)
# =============================================================================

prep_dia15_correlation <- function() {
  
  log_info("Día 15: Descargando cotizaciones para matriz de correlación...")
  
  env <- new.env()
  
  activos <- c(
    "SPY" = "S&P500", 
    "TLT" = "Bonos 20Y", 
    "GLD" = "Oro", 
    "BZ=F" = "Brent", 
    "BTC-USD" = "BTC"
  )
  
  # Descargar datos
  getSymbols(names(activos), src = "yahoo", from = Sys.Date() - 365, env = env, warnings = FALSE)
  
  # Limpieza
  dt_list <- lapply(names(activos), function(ticker) {
    xts_data <- env[[ticker]][, paste0(ticker, ".Adjusted")]
    # Log-returns
    xts_retornos <- Return.calculate(xts_data, method = "log")
    # Limpiamos los outliers (winsorización de Boudt)
    xts_limpios <- Return.clean(xts_retornos, method = "boudt")
    # Pasar a data.table
    dt <- data.table(index = index(xts_limpios), coredata(xts_limpios))
    setnames(dt, c("index", names(xts_data)), c("fecha", "retorno"))
    dt[, activo := activos[ticker]]
    
    dt[!is.na(retorno), .(fecha, activo, retorno)][]
  })

  dt_long <- rbindlist(dt_list)
  dt_wide <- dcast(dt_long, fecha ~ activo, value.var = "retorno")
  
  # Correlación de Pearson
  matriz_cor <- cor(dt_wide[, -1, with = FALSE], use = "pairwise.complete.obs")
  matriz_cor[upper.tri(matriz_cor, diag = FALSE)] <- NA
  
  # Adaptar para ggplot
  dt_cor <- as.data.table(as.table(matriz_cor))
  setnames(dt_cor, c("V1", "V2", "N"), c("activo_1", "activo_2", "correlacion"))
  dt_cor <- dt_cor[!is.na(correlacion)]

  dt_cor[, activo_1 := factor(activo_1, levels = rev(unname(activos)))]
  dt_cor[, activo_2 := factor(activo_2, levels = unname(activos))]
  
  log_success("Día 15 preparado: Matriz de correlación de {length(activos)} activos calculada.")
  
  dt_cor[]
}


# =============================================================================
# DÍA 16 — Causation (Relationships)
# =============================================================================

prep_dia16_causation <- function() {
  
  log_info("Día 16: Aislando el efecto causal del 'FED Pivot' (Event Study)...")
  
  env <- new.env()
  
  # S&P 500
  getSymbols("^GSPC", src = "yahoo", from = "1999-01-01", env = env, warnings = FALSE)
  dt_sp500 <- as.data.table(env$GSPC)
  setnames(dt_sp500, c("index", "GSPC.Adjusted"), c("fecha", "precio"), skip_absent = TRUE)
  dt_sp500 <- dt_sp500[!is.na(precio), .(fecha, precio)]
  
  # Definir evento causal (T = 0)
  eventos_pivot <- list(
    "Burbuja Dot-Com" = as.Date("2001-01-03"),
    "Gran Crisis Financiera" = as.Date("2007-09-18"),
    "Pánico COVID-19" = as.Date("2020-03-03"),
    "Policrisis (Ciclo Actual)" = as.Date("2024-09-18") 
  )
  
  # Ventanas temporales de referencia (T-50 a T+250 días de mercado)
  lista_eventos <- lapply(names(eventos_pivot), function(nombre) {
    fecha_cero <- eventos_pivot[[nombre]]
    idx_cero <- which.min(abs(dt_sp500$fecha - fecha_cero))
    
    # Extraer la ventana
    idx_inicio <- max(1, idx_cero - 50)
    idx_fin <- min(nrow(dt_sp500), idx_cero + 250)
    
    dt_ventana <- dt_sp500[idx_inicio:idx_fin]
    
    # Creamos el eje X relativo (Días desde el evento)
    dt_ventana[, dia_relativo := .I - (idx_cero - idx_inicio + 1)]
    
    # Normalización
    precio_base <- dt_ventana[dia_relativo == 0, precio]
    dt_ventana[, precio_norm := (precio / precio_base) * 100]
    
    dt_ventana[, ciclo := nombre]
    
    return(dt_ventana[, .(ciclo, dia_relativo, precio_norm)])
  })
  
  dt_causation <- rbindlist(lista_eventos)
  
  # Ordenamos los factores para la paleta
  dt_causation[, ciclo := factor(ciclo, levels = c(
    "Burbuja Dot-Com", "Gran Crisis Financiera", "Pánico COVID-19", "Policrisis (Ciclo Actual)"
  ))]
  
  log_success("Día 16 preparado: Inferencia causal alineada a T=0 para 4 ciclos históricos.")
  
  dt_causation[]
}


# =============================================================================
# DÍA 17 — Remake (Relationships)
# =============================================================================

prep_dia17_remake <- function() {
  
  log_info("Día 17: Extrayendo componentes cíclicos con MacroFilters (MBH)...")
  
  env <- new.env()
  
  # S&P 500 y M2
  getSymbols("M2SL", src = "FRED", env = env, warnings = FALSE)
  getSymbols("^GSPC", src = "yahoo", from = "2000-01-01", env = env, warnings = FALSE)
  
  dt_m2 <- as.data.table(env$M2SL)
  setnames(dt_m2, c("index", "M2SL"), c("fecha", "m2"), skip_absent = TRUE)
  dt_m2[, mes_ano := floor_date(as.Date(fecha), "month")]
  
  dt_sp <- as.data.table(env$GSPC)
  setnames(dt_sp, "index", "fecha", skip_absent = TRUE)
  dt_sp[, mes_ano := floor_date(as.Date(fecha), "month")]
  dt_sp_m <- dt_sp[, .(sp500 = mean(GSPC.Adjusted, na.rm = TRUE)), by = mes_ano]
  
  dt <- merge(dt_sp_m, dt_m2[, .(mes_ano, m2)], by = "mes_ano")
  dt <- na.omit(dt)
  
  # Transformación Logarítmica
  dt[, log_sp := log(sp500)]
  dt[, log_m2 := log(m2)]
  
  # 3. FILTRADO MACROECONÓMICO ROBUSTO (La técnica de la viñeta)
  log_info("Calibrando la delta de Huber y aplicando MBH...")

  d_sp <- mad(hp_filter(dt$log_sp, freq = 12)$cycle)
  d_m2 <- mad(hp_filter(dt$log_m2, freq = 12)$cycle)
  
  # Aplicamos el filtro MBH robusto a shocks estructurales
  filtro_sp <- mbh_filter(dt$log_sp, d = d_sp)
  filtro_m2 <- mbh_filter(dt$log_m2, d = d_m2)
  
  # Extraemos el ciclo puro
  dt[, ciclo_sp := filtro_sp$cycle]
  dt[, ciclo_m2 := filtro_m2$cycle]
  
  log_success("Día 17 preparado: Ciclos limpios y sin distorsión de shocks extraídos.")
  
  dt[]
}


# =============================================================================
# DÍA 18 — UNICEF (Relationships)
# =============================================================================

prep_dia18_unicef <- function(ruta_xlsx = "R/30DayChartChallenge2026/data/UNICEF_Expanded_Global_Databases_child_food_poverty_2024_2.xlsx") {
  
  log_info("Día 18: Procesando reporte de Pobreza Alimentaria Infantil de UNICEF...")
  
  if (!file.exists(ruta_xlsx)) {
    log_error("Descarga el fichero UNICEF_Expanded_Global_Databases_child_food_poverty_2024_2.xlsx")
    stop("Pipeline detenido.")
  }

  raw_data <- suppressMessages(
    readxl::read_excel(ruta_xlsx, sheet = "Latest Regional Global", col_names = FALSE)
  )
  
  dt_raw <- as.data.table(raw_data)
  
  # Basado en la estructura de UNICEF:
  # Columna 1 = Región, Columna 4 = Severa, Columna 5 = Moderada
  dt_clean <- dt_raw[4:.N, .(
    region = `...1`, 
    severe = `...4`, 
    moderate = `...5`
  )]
  
  # Limpieza estricta: Eliminar NAs, filas vacías, y guiones ("-")
  dt_clean <- dt_clean[!is.na(region) & !is.na(severe) & !is.na(moderate)]
  dt_clean <- dt_clean[severe != "-" & moderate != "-"]
  dt_clean <- dt_clean[severe != "Severe child food poverty"] # Quitamos la fila de cabecera de texto
  
  # Convertimos a numérico de forma segura
  dt_clean[, severe := as.numeric(severe)]
  dt_clean[, moderate := as.numeric(moderate)]
  
  # Filtramos solo el bloque de regiones principales de UNICEF y el Global
  regiones_target <- c(
    "Global",
    "East Asia and the Pacific",
    "Eastern and Southern Africa",
    "Middle East and North Africa",
    "South Asia",
    "West and Central Africa"
  )
  
  dt_final <- dt_clean[region %in% regiones_target]
  
  # Calculamos el total de pobreza alimentaria para ordenar
  dt_final[, total_poverty := severe + moderate]
  
  # Traducimos las regiones para el lienzo
  traducciones <- c(
    "Global" = "Promedio Global",
    "East Asia and the Pacific" = "Este de Asia y\nPacífico",
    "Eastern and Southern Africa" = "África Oriental y\nMeridional",
    "Middle East and North Africa" = "Medio Oriente y\nNorte de África",
    "South Asia" = "Sur de Asia",
    "West and Central Africa" = "África Occidental y\nCentral"
  )
  
  dt_final[, region_es := traducciones[region]]
  
  # Orden factorizado de peor a mejor
  dt_final[, region_es := factor(region_es, levels = dt_final[order(total_poverty)]$region_es)]
  
  log_success("Día 18 preparado: {nrow(dt_final)} regiones procesadas nativamente desde el .xlsx.")
  
  dt_final[]
}


# =============================================================================
# DÍA 19 — Evolution (Timeseries)
# =============================================================================

prep_dia19_evolution <- function() {
  
  log_info("Día 19: Descargando la estructura temporal de los tipos de interés (FRED)...")
  
  env <- new.env()
  
  # Tickers de FRED
  tickers <- c(
    "1m" = "DGS1MO", "3m" = "DGS3MO", "6m" = "DGS6MO", 
    "1y" = "DGS1", "2y" = "DGS2", "3y" = "DGS3", 
    "5y" = "DGS5", "7y" = "DGS7", "10y" = "DGS10", 
    "20y" = "DGS20", "30y" = "DGS30"
  )
  
  # Mapeo a meses numéricos para el eje X continuo
  madurez_meses <- c("1m"=1, "3m"=3, "6m"=6, "1y"=12, "2y"=24, "3y"=36, "5y"=60, "7y"=84, "10y"=120, "20y"=240, "30y"=360)
  
  # Descargar
  getSymbols(unname(tickers), src = "FRED", env = env, warnings = FALSE)
  
  # Unir
  lista_dt <- lapply(names(tickers), function(nombre) {
    ticker_sym <- tickers[nombre]
    dt <- as.data.table(env[[ticker_sym]])
    setnames(dt, c("index", ticker_sym), c("fecha", "yield"), skip_absent = TRUE)
    dt[, tramo := nombre]
    dt[, meses := madurez_meses[nombre]]
    return(dt[!is.na(yield)])
  })
  
  dt_full <- rbindlist(lista_dt)
  dt_full[, ano := year(fecha)]
  
  dt_full <- dt_full[ano >= 2000]
  
  dt_cierre <- dt_full[order(fecha), .SD[.N], by = .(ano, tramo)]
  
  # Detectar Inversión (10y - 3m). 
  dt_wide <- dcast(dt_cierre, ano ~ tramo, value.var = "yield")
  dt_wide[, spread := `10y` - `3m`]
  dt_wide[, estado := fcase(
    spread < 0, "Invertida (Recesión inminente)",
    default = "Normal (Expansión)"
  )]
  
  dt_final <- merge(dt_cierre, dt_wide[, .(ano, estado)], by = "ano")
  
  # Ordenamos para el plot
  dt_final <- dt_final[order(ano, meses)]
  
  log_success("Día 19 preparado: Curvas anuales desde el año 2000 extraídas. Listos para el Ridgeline.")
  
  dt_final[]
}


# =============================================================================
# DÍA 20 — Global Change (Timeseries)
# =============================================================================

prep_dia20_global_change <- function() {
  
  log_info("Día 20: Descargando anomalías térmicas globales desde NASA GISS...")
  
  # GISTEMP v4
  url <- "https://data.giss.nasa.gov/gistemp/tabledata_v4/GLB.Ts+dSST.csv"
  
  # La NASA incluye una fila de título descriptiva, así que la saltamos (skip = 1)
  # Además, los NAs vienen marcados como "***"
  dt_raw <- fread(url, skip = 1, na.strings = "***")
  
  # Seleccionamos explícitamente la columna del Año y los 12 meses
  meses <- c("Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec")
  meses_es <- c("Ene", "Feb", "Mar", "Abr", "May", "Jun", "Jul", "Ago", "Sep", "Oct", "Nov", "Dic")
  
  dt_clean <- dt_raw[, .SD, .SDcols = c("Year", meses)]
  
  dt_long <- melt(
    dt_clean, 
    id.vars = "Year", 
    measure.vars = meses,
    variable.name = "mes_en", 
    value.name = "anomalia"
  )
  
  # Limpiar
  dt_long <- dt_long[!is.na(anomalia)]
  dt_long[, anomalia := as.numeric(anomalia)]
  dt_long[, mes := factor(meses_es[match(mes_en, meses)], levels = meses_es)]
  
  log_success("Día 20 preparado: {nrow(dt_long)} registros mensuales de temperatura (1880-presente) listos.")
  
  dt_long[]
}


# =============================================================================
# DÍA 21 — Historical (Timeseries)
# =============================================================================

prep_dia21_historical <- function(ruta_csv = "R/30DayChartChallenge2026/data/28182.csv") {
  
  log_info("Día 21: Procesando la compresión salarial histórica (SMI)...")

  if (!file.exists(ruta_csv)) {
    log_error("FALTA EL ARCHIVO: Descarga 28182.csv del INE")
    stop("Pipeline detenido.")
  }

  dt_raw <- fread(ruta_csv, dec = ",", encoding = "Latin-1")
  dt_raw <- dt_raw[`Intervalos de salario` %notin% c("De 0 a 1 SMI", "Total de trabajadores")]
  dt_raw[, grupo := fcase(
    `Intervalos de salario` == "De 1 a 2 SMI", "1 a 2 SMI",
    `Intervalos de salario` == "De 2 a 3 SMI", "2 a 3 SMI",
    default =  "Más de 3 SMI"
  )]

  dt_long <- dt_raw[, .(porcentaje = sum(Total)), by = .(Periodo, grupo)]

  dt_wide <- dcast(dt_long, Periodo ~ grupo, value.var = "porcentaje")
  dt_wide <- dt_wide[, .(Periodo, smi_1_2 = `1 a 2 SMI`, smi_2_3 = `2 a 3 SMI`, smi_mas_3 = `Más de 3 SMI`)]
  
  log_success("Día 21 preparado: Datos inquebrantables de compresión salarial.")
  
  return(list(long = dt_long, wide = dt_wide))
}


# =============================================================================
# DÍA 22 — New Tool (Timeseries)
# =============================================================================

prep_dia22_new_tool <- function() {
  
  log_info("Día 22: Descargando el Nikkei 225 y calculando el sistema Ichimoku...")
  
  # Descarga del Nikkei 225 usando Yahoo Finance
  env <- new.env()
  getSymbols("^N225", src = "yahoo", env = env, warnings = FALSE)
  
  # Extraemos el objeto xts
  dt_xts <- env$N225
  
  # Limpiamos NAs que a veces deja Yahoo Finance en festivos locales
  dt_xts <- na.omit(dt_xts)
  
  # Filtramos el último año y medio para tener una vista detallada de la ruptura histórica
  dt_xts_recent <- dt_xts["2023-01-01/"]
  
  # NUEVA HERRAMIENTA: Generamos el objeto ichimoku
  # Esto calcula las 5 líneas (Tenkan, Kijun, Senkou A, Senkou B, Chikou)
  nube <- ichimoku(dt_xts_recent)
  
  log_success("Día 22 preparado: Objeto Ichimoku Kinko Hyo listo para el renderizado.")
  
  return(nube)
}


# =============================================================================
# DÍA 23 — Seasons (Timeseries)
# =============================================================================

prep_dia23_seasons <- function(archivo_local = "R/30DayChartChallenge2026/data/aemet_retiro.csv") {
  
  log_info("Día 23: Procesando Precipitación Acumulada (AEMET - Retiro)...")
  
  if (!file.exists(archivo_local)) stop(paste("FALTA EL ARCHIVO:", archivo_local))
  
  dt_raw <- fread(archivo_local)
  
  # Seleccionamos variables
  dt <- dt_raw[, .(fecha, prec)]
  
  # Limpiar
  dt[, prec_clean := gsub("Ip", "0.0", prec)]
  dt[, prec_clean := gsub(",", ".", prec_clean)]
  dt[, prec_num := as.numeric(prec_clean)]
  dt[is.na(prec_num), prec_num := 0] 
  
  # Estacional
  dt[, anio := year(fecha)]
  dt[, mes := month(fecha)]
  dt[, dia := mday(fecha)]
  
  # Suma acumulada de lluvia a lo largo del año
  setorder(dt, fecha)
  dt[, prec_acumulada := cumsum(prec_num), by = anio]
  dt[, fecha_ficticia := as.Date(sprintf("2024-%02d-%02d", mes, dia))]
  
  max_anio <- max(dt$anio)
  dt_historico <- dt[anio < max_anio]
  
  totales_anuales <- dt_historico[, .(total = max(prec_acumulada)), by = anio]
  anio_mas_seco <- totales_anuales[which.min(total), anio]
  anio_mas_humedo <- totales_anuales[which.max(total), anio]
  
  # Clasificar
  dt[, highlight := fcase(
    anio == anio_mas_seco, sprintf("%s (Más Seco)", anio),
    anio == anio_mas_humedo, sprintf("%s (Más Lluvioso)", anio),
    anio == max_anio, sprintf("%s (Año Actual)", anio),
    default = "Histórico"
  )]
  
  niveles_factor <- c("Histórico", sprintf("%s (Año Actual)", max_anio), sprintf("%s (Más Seco)", anio_mas_seco), sprintf("%s (Más Lluvioso)", anio_mas_humedo))
  dt[, highlight := factor(highlight, levels = niveles_factor)]
  setorder(dt, highlight)
  
  log_success(sprintf("Día 23 preparado. Seco: %s | Húmedo: %s", anio_mas_seco, anio_mas_humedo))
  
  dt[]
}


# =============================================================================
# DÍA 24 — South China Morning Post (Timeseries)
# =============================================================================

prep_dia24_scmp <- function() {
  
  log_info("Día 24: Descargando datos del Banco Mundial (Exportaciones Asia)...")
  
  # Descargamos Mercancías Exportadas (US$ actuales) para East Asia & Pacific (EAS)
  # desde 1970 hasta la actualidad.
  dt_raw <- WDI(indicator = "TX.VAL.MRCH.CD.WT", country = "EAS", start = 1970, end = 2023)
  setDT(dt_raw)
  
  # Limpieza y conversión a "Billones de dólares" (Trillions en inglés) 
  dt_raw <- dt_raw[!is.na(TX.VAL.MRCH.CD.WT)]
  dt_raw[, exports_trill := TX.VAL.MRCH.CD.WT / 1e12] # Dividimos entre un billón
  
  setorder(dt_raw, year)
  
  log_success("Día 24 preparado: Datos de Exportaciones Asiáticas listos.")
  
  dt_raw[]
}


# =============================================================================
# DÍA 25 — Space (Uncertainties)
# =============================================================================

prep_dia25_space <- function() {
  
  log_info("Día 25: Reconstruyendo el registro histórico del Asteroide Apophis (NASA JPL)...")
  
  # Estos son los datos REALES de los reportes del sistema Sentry de la NASA a finales de 2004
  # sobre la probabilidad de que el asteroide impactara en el año 2029.
  hitos_apophis <- data.table(
    fecha = as.Date(c(
      "2004-06-19", # Descubrimiento (datos insuficientes, riesgo cero aparente)
      "2004-12-20", # Se retoman observaciones, entra en la zona de riesgo
      "2004-12-23", # Anuncio oficial de riesgo: 1.1% (1 en 90)
      "2004-12-24", # El riesgo sube: 1.6% (1 en 62)
      "2004-12-26", # PÁNICO MÁXIMO: Nivel 4 en la Escala de Turín. Riesgo de 2.7% (1 en 37)
      "2004-12-27", # Salvación: Encuentran imágenes antiguas de 1994 ("precovery"). El cono se encoge.
      "2004-12-28", # Riesgo de impacto en 2029 descartado casi por completo
      "2005-01-10"  # Confirmación definitiva
    )),
    prob_real_estimada = c(0.0, 0.4, 1.1, 1.6, 2.7, 0.004, 0.0, 0.0),
    incertidumbre_sup =  c(0.1, 1.5, 3.5, 5.0, 7.5, 0.1,   0.0, 0.0) # Tamaño del error (Cono orbital)
  )
  
  dt <- data.table(fecha = seq(as.Date("2004-12-15"), as.Date("2005-01-15"), by = "1 day"))
  
  # Rellenar
  dt <- merge(dt, hitos_apophis, by = "fecha", all.x = TRUE)
  dt[, prob_mean := na.approx(prob_real_estimada, na.rm = FALSE)]
  dt[, prob_high := na.approx(incertidumbre_sup, na.rm = FALSE)]
  
  # Limpiamos los NA iniciales/finales
  dt[is.na(prob_mean), prob_mean := 0]
  dt[is.na(prob_high), prob_high := 0]
  dt[, prob_low := pmax(0, prob_mean - (prob_high - prob_mean))]
  
  # Identificamos el punto de máximo pánico para anotarlo en el gráfico
  dt[, es_pico := ifelse(fecha == as.Date("2004-12-26"), TRUE, FALSE)]
  
  log_success("Día 25 preparado: Datos históricos de Apophis reconstruidos.")
  
  dt[]
}


# =============================================================================
# DÍA 26 — Trend (Uncertainties)
# =============================================================================

prep_dia26_trend <- function(ruta_csv = "R/30DayChartChallenge2026/data/ecb_yield_curve.csv") {
  
  log_info("Día 26: Leyendo dataset masivo del BCE y calculando curvas Svensson...")
  
  if (!file.exists(ruta_csv)) {
    log_error("FALTA EL ARCHIVO: Descarga ecb_yield_curve.csv del BCE")
    stop("Pipeline detenido.")
    }

  dt_raw <- fread(ruta_csv)

  dt_params <- dt_raw[DATA_TYPE_FM %in% c("BETA0", "BETA1", "BETA2", "BETA3", "TAU1", "TAU2")]
  
  # Una fila por fecha, y los parámetros en columnas
  curvas <- dcast(dt_params, TIME_PERIOD ~ DATA_TYPE_FM, value.var = "OBS_VALUE")
  setnames(curvas, "TIME_PERIOD", "fecha")
  curvas[, fecha := as.Date(fecha)]
  
  # Función: Calcular Forward Rate (SVENSSON)
  calc_forward <- function(t, b0, b1, b2, b3, tau1, tau2) {
    term1 <- b1 * exp(-t / tau1)
    term2 <- b2 * (t / tau1) * exp(-t / tau1)
    term3 <- b3 * (t / tau2) * exp(-t / tau2)
    return(b0 + term1 + term2 + term3)
  }
  
  # El tipo a corto plazo real (usamos t = 1 año como proxy de la realidad macro)
  curvas[, tasa_real := calc_forward(1, BETA0, BETA1, BETA2, BETA3, TAU1, TAU2)]
  dt_real <- curvas[fecha >= "2013-01-01" & fecha <= "2019-01-01", .(fecha, tasa_real)]
  
  # Proyecciones / Expectativas
  # Elegimos fechas exactas (vintages) donde el BCE hizo anuncios importantes
  fechas_vintages <- as.Date(c(
    "2013-06-03", # Antes de medidas
    "2014-01-02", # Inicios del debate
    "2014-06-05", # ANUNCIO: Tipos negativos
    "2015-01-22", # ANUNCIO: Expansión cuantitativa (QE) masiva
    "2016-03-10"  # Capitulación del mercado
  ))
  
  lista_proyecciones <- lapply(fechas_vintages, function(f_vintage) {
    
    # Extraemos los parámetros de ese día exacto
    params_dia <- curvas[fecha == f_vintage]
    if(nrow(params_dia) == 0) return(NULL) # Por si cae en fin de semana
    
    # Proyectamos las expectativas mes a mes desde el día del anuncio hasta 3 años al futuro
    t_meses <- seq(0, 3, by = 1/12) 
    fechas_futuras <- f_vintage %m+% months(0:36)
    
    tasas_esperadas <- calc_forward(
      t_meses, 
      params_dia$BETA0, params_dia$BETA1, params_dia$BETA2, 
      params_dia$BETA3, params_dia$TAU1, params_dia$TAU2
    )
    
    data.table(
      vintage = format(f_vintage, "%b %Y"),
      fecha = fechas_futuras,
      tasa_proj = tasas_esperadas
    )
  })
  
  dt_proyecciones <- rbindlist(lista_proyecciones)
  
  log_success("Día 26 preparado: Hedgehog Chart generado con matemática de Svensson.")
  
  return(list(real = dt_real, proyecciones = dt_proyecciones))
}


# =============================================================================
# DÍA 27 — Animation (Uncertainties)
# =============================================================================

prep_dia27_animation <- function() {
  
  log_info("Día 27: Generando Bootstrapping para Hypothetical Outcome Plots (HOPs)...")
  
  # Datos base
  set.seed(2026)
  n_puntos <- 150
  x <- runif(n_puntos, 0, 10)
  # Relación débil con mucho ruido para que la línea "baile"
  y <- -0.5 * x + 10 + rnorm(n_puntos, mean = 0, sd = 4) 
  
  dt_puntos <- data.table(x = x, y = y)
  
  # Bootstrapping
  n_frames <- 50
  
  lista_lineas <- lapply(1:n_frames, function(frame_actual) {
    # Tomamos una muestra aleatoria con reemplazo
    muestra <- dt_puntos[sample(1:.N, replace = TRUE)]    
    modelo <- lm(y ~ x, data = muestra)
    
    # Guardar
    data.table(
      frame = frame_actual,
      x = c(0, 10),
      y = predict(modelo, newdata = data.frame(x = c(0, 10)))
    )
  })
  
  dt_lineas <- rbindlist(lista_lineas)
  
  log_success("Día 27 preparado: 50 regresiones hipotéticas calculadas.")
  
  return(list(puntos = dt_puntos, lineas = dt_lineas))
}


# =============================================================================
# DÍA 28 — Modeling (Uncertainties)
# =============================================================================

prep_dia28_modeling <- function() {
  
  log_info("Día 28: Descargando datos del S&P 500...")
  
  # Datos
  getSymbols("SPY", from = Sys.Date() - (365 * 10), to = Sys.Date(), auto.assign = TRUE)
  retornos_reales <- na.omit(ROC(Cl(SPY), type = "continuous"))
  S0_real         <- as.numeric(last(Cl(SPY)))
  
  # GBM clásico
  mu_gbm    <- mean(retornos_reales) * 252
  sigma_gbm <- sd(retornos_reales) * sqrt(252)
  
  # GARCH(1,1) con distribución t-Student
  # La t-Student captura fat tails (crashes/rallies inesperados)
  spec <- ugarchspec(
    variance.model = list(model = "sGARCH", garchOrder = c(1, 1)),
    mean.model     = list(armaOrder = c(0, 0), include.mean = TRUE),
    distribution.model = "std"   # Student-t estandarizada
  )
  
  fit <- ugarchfit(spec = spec, data = retornos_reales, solver = "hybrid")
  
  cf <- coef(fit)
  log_info(sprintf(
    "GARCH calibrado → ω=%.2e | α=%.4f | β=%.4f | ν=%.2f | persistencia=%.4f",
    cf["omega"], cf["alpha1"], cf["beta1"], cf["shape"],
    cf["alpha1"] + cf["beta1"]   # cuánto "recuerda" la volatilidad
  ))
  log_info(sprintf(
    "GBM baseline    → Drift: %.2f%% | σ: %.2f%% | Precio actual: $%.2f",
    mu_gbm * 100, sigma_gbm * 100, S0_real
  ))
  
  # Simular con Monte Carlo
  set.seed(2026)
  n_sims <- 1000
  n_days <- 252
  dt     <- 1 / 252
  
  # GBM clásico (Geometric Brownian Motion)
  Z_gbm <- matrix(rnorm(n_sims * n_days), nrow = n_sims)
  S_gbm <- matrix(0, nrow = n_sims, ncol = n_days + 1)
  S_gbm[, 1] <- S0_real
  for (t in 1:n_days) {
    S_gbm[, t+1] <- S_gbm[, t] * exp(
      (mu_gbm - sigma_gbm^2 / 2) * dt + sigma_gbm * sqrt(dt) * Z_gbm[, t]
    )
  }
  
  # GARCH(1,1)-t — simulación con ugarchsim ───────────────────────────
  # ugarchsim propaga correctamente ω, α, β, ν desde el punto actual del proceso
  # (sigma condicional del último día observado como estado inicial)
  sim_garch <- ugarchsim(
    fit,
    n.sim  = n_days,
    m.sim  = n_sims,
    rseed  = 2026
  )
  
  # Retornos simulados: matriz [n_days × n_sims]
  ret_sim   <- sim_garch@simulation$seriesSim
  sigma_sim <- sim_garch@simulation$sigmaSim  # sigma diaria [n_days × n_sims]
  
  # Reconstruimos precios desde retornos log
  S_garch        <- matrix(0, nrow = n_sims, ncol = n_days + 1)
  S_garch[, 1]   <- S0_real
  for (t in 1:n_days) {
    S_garch[, t+1] <- S_garch[, t] * exp(ret_sim[t, ])
  }
  
  # Recojn data.table
  preprocess_sims <- function(S_mat, modelo_label) {
    dt_s   <- as.data.table(S_mat)
    dt_s[, sim_id := 1:.N]
    dt_long <- melt(dt_s, id.vars = "sim_id", variable.name = "dia", value.name = "precio")
    dt_long[, dia    := as.integer(gsub("V", "", dia)) - 1L]
    dt_long[, modelo := modelo_label]
    dt_long
  }
  
  dt_gbm_long   <- preprocess_sims(S_gbm,   "GBM clásico")
  dt_garch_long <- preprocess_sims(S_garch, "GARCH(1,1)-t")
  dt_all_long   <- rbindlist(list(dt_gbm_long, dt_garch_long))
  
  # Cuantiles por modelo y día
  dt_quantiles <- dt_all_long[, .(
    p05 = quantile(precio, 0.05),
    p25 = quantile(precio, 0.25),
    p50 = quantile(precio, 0.50),
    p75 = quantile(precio, 0.75),
    p95 = quantile(precio, 0.95)
  ), by = .(dia, modelo)]
  
  # Spaghetti: 60 paths por modelo, GARCH los más interesantes visualmente
  dt_spaghetti <- rbindlist(list(
    dt_gbm_long[sim_id   <= 60],
    dt_garch_long[sim_id <= 60]
  ))
  
  # Volatilidad condicional proyectada (anualizada) — solo GARCH
  dt_vol_sim <- as.data.table(t(sigma_sim * sqrt(252) * 100))  # % anualizado
  dt_vol_sim[, sim_id := 1:.N]
  dt_vol_long <- melt(dt_vol_sim, id.vars = "sim_id", variable.name = "dia", value.name = "vol_anual_pct")
  dt_vol_long[, dia := as.integer(gsub("V", "", dia))]
  
  dt_vol_proj <- dt_vol_long[, .(
    vol_media = mean(vol_anual_pct),
    vol_p10   = quantile(vol_anual_pct, 0.10),
    vol_p90   = quantile(vol_anual_pct, 0.90)
  ), by = dia]
  
  # Volatilidad histórica realizada para contexto (ventana 21d)
  vol_hist_actual <- sqrt(252) * sd(tail(retornos_reales, 21)) * 100
  
  log_success(sprintf(
    "Día 28 listo → σ implícita GARCH hoy: %.1f%% | σ histórica 21d: %.1f%%",
    as.numeric(tail(sigma(fit), 1)) * sqrt(252) * 100,
    vol_hist_actual
  ))
  
  return(list(
    spaghetti    = dt_spaghetti,
    quantiles    = dt_quantiles,       # ambos modelos, para superponer cones
    vol_proj     = dt_vol_proj,        # subpanel de volatilidad GARCH
    vol_hist_pct = vol_hist_actual,    # referencia visual
    s0           = S0_real,
    # Parámetros para el subtítulo del plot
    params = list(
      mu_gbm    = mu_gbm,
      sigma_gbm = sigma_gbm,
      omega     = cf["omega"],
      alpha1    = cf["alpha1"],
      beta1     = cf["beta1"],
      nu        = cf["shape"],
      persist   = cf["alpha1"] + cf["beta1"]
    ),
    garch_fit = fit
  ))
}
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
library(dplyr)


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
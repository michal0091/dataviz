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
    continente = c("África", "América", "Asia-Pacífico", "Europa del Este y Asia Central", "Oriente Medio y Norte de África", "UE y Balcanes")
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
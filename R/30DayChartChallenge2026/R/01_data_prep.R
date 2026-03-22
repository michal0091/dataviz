# =============================================================================
# 01_data_prep.R — Plantillas de preparación de datos
# #30DayChartChallenge 2026
#
# Convención de nombres:
#   prep_raw_diaXX()   → lee el CSV crudo y devuelve un data.table sin tocar
#   prep_datos_diaXX() → limpia y transforma; recibe el raw, devuelve el clean
# =============================================================================


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

#' Lee el CSV crudo del Día 01.
#' @param path Ruta al archivo CSV.
#' @return data.table crudo.
prep_raw_dia01 <- function(path) {
  logger::log_info("[Día 01] Leyendo datos desde: {path}")
  dt <- data.table::fread(path, encoding = "UTF-8")
  logger::log_info("[Día 01] Filas leídas: {nrow(dt)} | Columnas: {ncol(dt)}")
  dt
}

#' Limpia y transforma los datos del Día 01 (Part-to-Whole).
#' @param dt_raw data.table crudo.
#' @return data.table con columnas `categoria`, `valor`, `pct`.
prep_datos_dia01 <- function(dt_raw) {
  logger::log_info("[Día 01] Iniciando transformación ({nrow(dt_raw)} filas)")

  dt <- data.table::copy(dt_raw)

  # TODO: adaptar nombres de columnas reales
  # data.table::setnames(dt, old = c("..."), new = c("categoria", "valor"))
  # dt[, pct := valor / sum(valor) * 100]
  # dt[, etiqueta := stringr::str_glue("{categoria}\n{round(pct, 1)}%")]
  # data.table::setorder(dt, -valor)

  logger::log_success("[Día 01] Transformación completada ({nrow(dt)} filas)")
  dt
}

# =============================================================================
# 02_plots.R — Plantillas de funciones de graficado
# #30DayChartChallenge 2026
#
# Convención:
#   plot_diaXX(dt, tema, config, output) → construye, muestra y guarda el plot
# =============================================================================


# =============================================================================
# PLANTILLA DÍA XX — sustituir XX por el número de día
# =============================================================================

#' Construye el gráfico del Día XX y lo guarda en disco.
#'
#' @param dt      data.table limpio devuelto por prep_datos_diaXX().
#' @param tema    Objeto theme() de ggplot2 (ej: theme_30dcc() o theme_week1()).
#' @param config  Lista de configuración cargada desde el YAML.
#' @param output  Ruta completa del archivo .png de salida.
#' @param width   Ancho en píxeles (default: 1200).
#' @param height  Alto en píxeles (default: 1200).
#' @param dpi     Resolución (default: 150).
#'
#' @return Ruta al archivo guardado (invisible).
#'
plot_diaXX <- function(dt,
                       tema   = theme_30dcc(),
                       config,
                       output,
                       width  = 1200,
                       height = 1200,
                       dpi    = 150) {

  logger::log_info("[Día XX] Construyendo gráfico")

  # Generar caption
  caption <- generate_caption(
    day         = XX,   # <-- sustituir por número
    source_text = "TODO: fuente de datos",
    config      = config
  )

  # Construir plot
  gg <- ggplot2::ggplot(dt) +
    # TODO: añadir capas geom_*() según el tipo de gráfico
    # ggplot2::geom_col(ggplot2::aes(x = categoria, y = pct)) +
    ggplot2::labs(
      title    = "TODO: título",
      subtitle = "TODO: subtítulo",
      caption  = caption,
      x        = NULL,
      y        = NULL
    ) +
    tema

  # Guardar
  ggplot2::ggsave(
    filename = output,
    plot     = gg,
    width    = width,
    height   = height,
    units    = "px",
    dpi      = dpi,
    bg       = "white"
  )

  logger::log_success("[Día XX] Gráfico guardado en: {output}")
  invisible(output)
}


# =============================================================================
# DÍA 01 — Part-to-Whole (Comparisons)
# =============================================================================

#' Gráfico de parte-a-todo para el Día 01.
#'
#' Tipo sugerido: waffle / treemap / donut / barras apiladas al 100%.
#'
#' @param dt      data.table con columnas `categoria`, `valor`, `pct`.
#' @param tema    Objeto theme() de ggplot2.
#' @param config  Lista de configuración del YAML.
#' @param output  Ruta del .png de salida.
#'
#' @return Ruta al archivo guardado (invisible).
#'
plot_dia01 <- function(dt,
                       tema   = theme_30dcc(),
                       config,
                       output = "R/30DayChartChallenge2026/outputs/day_01_part_to_whole.png") {

  logger::log_info("[Día 01] Construyendo gráfico Part-to-Whole")

  caption <- generate_caption(
    day         = 1,
    source_text = "TODO: fuente de datos",
    config      = config
  )

  gg <- ggplot2::ggplot(dt, ggplot2::aes(x = "", y = pct, fill = categoria)) +
    # Ejemplo base: barras apiladas al 100% — sustituir por el geom final
    ggplot2::geom_col(width = 0.6, color = "white", linewidth = 0.4) +
    ggplot2::coord_flip() +
    ggplot2::scale_y_continuous(labels = scales::percent_format(scale = 1)) +
    ggplot2::labs(
      title    = "TODO: título Día 01",
      subtitle = "TODO: subtítulo",
      caption  = caption,
      x        = NULL,
      y        = NULL,
      fill     = NULL
    ) +
    tema

  ggplot2::ggsave(
    filename = output,
    plot     = gg,
    width    = 1200,
    height   = 900,
    units    = "px",
    dpi      = 150,
    bg       = "white"
  )

  logger::log_success("[Día 01] Gráfico guardado en: {output}")
  invisible(output)
}

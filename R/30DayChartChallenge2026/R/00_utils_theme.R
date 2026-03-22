# =============================================================================
# 00_utils_theme.R — Tema base para el #30DayChartChallenge 2026
# =============================================================================

#' Tema ggplot2 minimalista para storytelling limpio.
#'
#' Fondo blanco, sin panel gris, tipografía clara. Diseñado como base
#' genérica que puede combinarse con cualquier tema semanal.
#'
#' @param base_size   Tamaño de fuente base (default: 11).
#' @param base_family Familia tipográfica (default: "Lato").
#' @param bg_col      Color de fondo (default: "#FFFFFF").
#' @param text_col    Color principal del texto (default: "#222222").
#' @param grid_col    Color de la cuadrícula (default: "#E8E8E8").
#'
#' @return Un objeto theme() de ggplot2.
#'
theme_30dcc <- function(base_size   = 11,
                        base_family = "Lato",
                        bg_col      = "#FFFFFF",
                        text_col    = "#222222",
                        grid_col    = "#E8E8E8") {

  ggplot2::theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    ggplot2::theme(
      # Fondos —  sin panel gris
      plot.background  = ggplot2::element_rect(fill = bg_col, color = NA),
      panel.background = ggplot2::element_rect(fill = bg_col, color = NA),

      # Cuadrícula: solo líneas mayores, muy sutiles
      panel.grid.major = ggplot2::element_line(color = grid_col, linewidth = 0.3),
      panel.grid.minor = ggplot2::element_blank(),

      # Tipografía general
      text = ggplot2::element_text(color = text_col, family = base_family),

      # Título: negrita, alineado a la izquierda
      plot.title = ggplot2::element_text(
        size   = ggplot2::rel(2.0),
        face   = "bold",
        hjust  = 0,
        color  = text_col,
        margin = ggplot2::margin(b = 6)
      ),

      # Subtítulo: más ligero, mayor interlineado
      plot.subtitle = ggplot2::element_text(
        size       = ggplot2::rel(1.3),
        hjust      = 0,
        color      = text_col,
        lineheight = 1.3,
        margin     = ggplot2::margin(b = 12)
      ),

      # Ejes: sin marcas, texto discreto
      axis.text  = ggplot2::element_text(color = text_col, size = ggplot2::rel(1.1)),
      axis.title = ggplot2::element_text(color = text_col, size = ggplot2::rel(1.2), hjust = 0.5),
      axis.ticks = ggplot2::element_blank(),

      # Leyenda: arriba, sin caja
      legend.position      = "top",
      legend.justification = "left",
      legend.background    = ggplot2::element_rect(fill = bg_col, color = NA),
      legend.key           = ggplot2::element_rect(fill = bg_col, color = NA),
      legend.text          = ggplot2::element_text(color = text_col, size = ggplot2::rel(1.1)),
      legend.title         = ggplot2::element_text(color = text_col, size = ggplot2::rel(1.2), face = "bold"),

      # Facetas: sin recuadro gris
      strip.background = ggplot2::element_blank(),
      strip.text       = ggplot2::element_text(face = "bold", color = text_col, size = ggplot2::rel(1.1), hjust = 0),

      # Caption: HTML/Markdown, alineado a la izquierda
      plot.caption.position = "plot",
      plot.caption = ggtext::element_markdown(
        color      = text_col,
        size       = ggplot2::rel(1.0),
        hjust      = 0,
        halign     = 0,
        lineheight = 1.2,
        margin     = ggplot2::margin(t = 12, b = 2)
      ),

      # Márgenes del plot
      plot.margin = ggplot2::margin(15, 15, 10, 15),

      complete = TRUE
    )
}

# --- themes_30DCC2026.R ---
# Temas y paletas de colores para el #30DayChartChallenge 2026

library(ggplot2)
library(ggtext)


# =============================================================================
# 1. PALETAS DE COLORES
# =============================================================================

# Semana 1 - Comparisons
paleta_week1 <- c(
  "#2C3E50",  # Azul medianoche
  "#E74C3C",  # Rojo
  "#3498DB",  # Azul claro
  "#F39C12",  # Naranja
  "#27AE60"   # Verde
)

# Semana 2 - Distributions
paleta_week2 <- c(
  "#6C3483",  # Púrpura oscuro
  "#A569BD",  # Lavanda
  "#D7BDE2",  # Lavanda claro
  "#1ABC9C",  # Turquesa
  "#A3E4D7"   # Turquesa claro
)

# Semana 3 - Relationships
paleta_week3 <- c(
  "#1B4F72",  # Azul marino
  "#2980B9",  # Azul
  "#AED6F1",  # Azul hielo
  "#E67E22",  # Naranja oscuro
  "#FAD7A0"   # Melocotón
)

# Semana 4 - Timeseries
paleta_week4 <- c(
  "#145A32",  # Verde oscuro
  "#1E8449",  # Verde
  "#82E0AA",  # Verde claro
  "#D35400",  # Naranja quemado
  "#F0B27A"   # Salmón
)

# Semana 5 - Uncertainties
paleta_week5 <- c(
  "#212F3D",  # Gris muy oscuro
  "#5D6D7E",  # Gris azulado
  "#AEB6BF",  # Gris claro
  "#E74C3C",  # Rojo acento
  "#F9E79F"   # Amarillo pálido
)

# Gradiente genérico
generic_gradient <- c("#f7fbff", "#deebf7", "#c6dbef", "#9ecae1",
                      "#6baed6", "#4292c6", "#2171b5", "#084594")

# Lista maestra de paletas
challenge_palettes <- list(
  week1    = paleta_week1,
  week2    = paleta_week2,
  week3    = paleta_week3,
  week4    = paleta_week4,
  week5    = paleta_week5,
  gradient = generic_gradient
)


# =============================================================================
# 2. FUNCIÓN GENERADORA DE PALETAS
# =============================================================================

#' Genera una función interpoladora a partir de una paleta del challenge.
#'
#' @param palette Nombre de la paleta (clave en challenge_palettes).
#' @param reverse Invertir la paleta?
#' @param ...     Argumentos adicionales para colorRampPalette().
#' @return Función que devuelve n colores interpolados.
#'
challenge_pal <- function(palette = "week1", reverse = FALSE, ...) {
  if (!palette %in% names(challenge_palettes)) {
    stop(paste(
      "Paleta no encontrada:", palette,
      "\nPaletas disponibles:", paste(names(challenge_palettes), collapse = ", ")
    ))
  }
  pal <- challenge_palettes[[palette]]
  if (reverse) pal <- rev(pal)
  grDevices::colorRampPalette(pal, ...)
}


# =============================================================================
# 3. ESCALAS ggplot2
# =============================================================================

#' Escala de COLOR para ggplot2 usando paletas del challenge 2026.
scale_color_challenge <- function(palette = "week1", discrete = TRUE, reverse = FALSE, ...) {
  pal <- challenge_pal(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale(aesthetics = "colour", palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...)
  }
}

#' Escala de RELLENO para ggplot2 usando paletas del challenge 2026.
scale_fill_challenge <- function(palette = "week1", discrete = TRUE, reverse = FALSE, ...) {
  pal <- challenge_pal(palette = palette, reverse = reverse)
  if (discrete) {
    ggplot2::discrete_scale(aesthetics = "fill", palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...)
  }
}


# =============================================================================
# 4. TEMAS ggplot2
# =============================================================================

# --- Semana 1: Comparisons ---
#' Tema limpio y editorial para gráficos de comparación.
theme_week1 <- function(base_size = 11, base_family = "Roboto") {
  bg_col   <- "#F5F0EB"
  text_col <- "#2C3E50"
  grid_col <- "#D5CFC8"

  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background  = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = bg_col, color = NA),
      panel.grid.major = element_line(color = grid_col, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      text             = element_text(color = text_col, family = base_family),
      plot.title       = element_text(family = "Baumans", size = rel(3.2), hjust = 0,
                                      margin = margin(b = 8), color = text_col),
      plot.subtitle    = element_text(size = rel(2.0), hjust = 0,
                                      margin = margin(b = 15), lineheight = 1.2, color = text_col),
      axis.text        = element_text(color = text_col, size = rel(1.4)),
      axis.title       = element_text(color = text_col, size = rel(1.6), hjust = 0.5),
      legend.position  = "top",
      legend.justification = "left",
      legend.background    = element_rect(fill = bg_col, color = NA),
      legend.key           = element_rect(fill = bg_col, color = NA),
      legend.text          = element_text(color = text_col, size = rel(1.4)),
      legend.title         = element_text(color = text_col, size = rel(1.6), face = "bold"),
      plot.caption.position = "plot",
      plot.caption     = element_markdown(color = text_col, size = rel(1.4), hjust = 0,
                                          halign = 0, margin = margin(t = 12, b = 2),
                                          lineheight = 1.2),
      plot.margin      = margin(15, 15, 10, 15),
      axis.ticks       = element_blank(),
      complete         = TRUE
    )
}

# --- Semana 2: Distributions ---
#' Tema oscuro y técnico para gráficos de distribuciones.
theme_week2 <- function(base_size = 11, base_family = "Roboto Mono") {
  bg_col    <- "#1A1A2E"
  panel_col <- "#16213E"
  text_col  <- "#E0E0E0"
  grid_col  <- "#3A3A5C"

  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background  = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = panel_col, color = NA),
      panel.grid.major = element_line(color = grid_col, linewidth = 0.25),
      panel.grid.minor = element_blank(),
      text             = element_text(color = text_col, family = base_family),
      plot.title       = element_text(family = "Exo 2", size = rel(2.2), hjust = 0.5,
                                      margin = margin(b = 10), face = "bold", color = text_col),
      plot.subtitle    = element_text(size = rel(1.4), hjust = 0.5,
                                      margin = margin(b = 15), color = text_col),
      axis.text        = element_text(color = text_col, size = rel(1.2)),
      axis.title       = element_text(color = text_col, size = rel(1.4)),
      axis.line        = element_line(color = grid_col, linewidth = 0.5),
      legend.position  = "top",
      legend.background    = element_rect(fill = bg_col, color = NA),
      legend.key           = element_rect(fill = panel_col, color = NA),
      legend.text          = element_text(color = text_col, size = rel(1.2)),
      legend.title         = element_text(color = text_col, size = rel(1.4), face = "bold"),
      plot.caption.position = "plot",
      plot.caption     = element_markdown(color = text_col, size = rel(1.0), hjust = 0,
                                          halign = 0, margin = margin(t = 15, b = 5),
                                          lineheight = 1.0),
      plot.margin      = margin(15, 15, 10, 15),
      axis.ticks       = element_blank(),
      complete         = TRUE
    )
}

# --- Semana 3: Relationships ---
#' Tema claro y académico para gráficos de relaciones.
theme_week3 <- function(base_size = 11, base_family = "Lato") {
  bg_col   <- "#FAFAFA"
  text_col <- "#1B2631"
  grid_col <- "#D5D8DC"

  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background  = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = bg_col, color = NA),
      panel.grid.major = element_line(color = grid_col, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      text             = element_text(color = text_col, family = base_family),
      plot.title       = element_text(size = rel(2.2), hjust = 0, face = "bold",
                                      margin = margin(b = 8), color = text_col),
      plot.subtitle    = element_text(size = rel(1.6), hjust = 0,
                                      margin = margin(b = 15), lineheight = 1.2, color = text_col),
      axis.text        = element_text(color = text_col, size = rel(1.2)),
      axis.title       = element_text(color = text_col, size = rel(1.4)),
      axis.line        = element_line(color = grid_col, linewidth = 0.4),
      legend.position  = "top",
      legend.justification = "left",
      legend.background    = element_rect(fill = bg_col, color = NA),
      legend.key           = element_rect(fill = bg_col, color = NA),
      legend.text          = element_text(color = text_col, size = rel(1.2)),
      legend.title         = element_text(color = text_col, size = rel(1.4), face = "bold"),
      plot.caption.position = "plot",
      plot.caption     = element_markdown(color = text_col, size = rel(1.2), hjust = 0,
                                          halign = 0, margin = margin(t = 15, b = 5),
                                          lineheight = 1.1),
      plot.margin      = margin(15, 15, 10, 15),
      axis.ticks       = element_line(color = grid_col, linewidth = 0.3),
      complete         = TRUE
    )
}

# --- Semana 4: Timeseries ---
#' Tema editorial para series temporales (inspirado en prensa financiera).
theme_week4 <- function(base_size = 11, base_family = "Lato") {
  bg_col    <- "#FFFFFF"
  text_col  <- "#222222"
  text2_col <- "#666666"
  grid_col  <- "#E8E8E8"
  accent_col <- "#145A32"

  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background  = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = bg_col, color = NA),
      panel.grid.major = element_line(color = grid_col, linewidth = 0.35),
      panel.grid.minor = element_blank(),
      text             = element_text(color = text_col, family = base_family),
      plot.title       = element_text(size = rel(2.2), hjust = 0, face = "bold",
                                      margin = margin(b = 6), color = accent_col),
      plot.subtitle    = element_text(size = rel(1.6), hjust = 0,
                                      margin = margin(b = 15), color = text2_col),
      axis.text        = element_text(color = text2_col, size = rel(1.3)),
      axis.title       = element_text(color = text_col, size = rel(1.5)),
      axis.line        = element_line(color = text_col, linewidth = 0.5),
      legend.position  = "top",
      legend.justification = "left",
      legend.background    = element_rect(fill = bg_col, color = NA),
      legend.key           = element_rect(fill = bg_col, color = NA),
      legend.text          = element_text(color = text_col, size = rel(1.3)),
      legend.title         = element_text(color = text_col, size = rel(1.5), face = "bold"),
      strip.background = element_blank(),
      strip.text       = element_text(face = "bold", color = text_col, size = rel(1.2), hjust = 0),
      plot.caption.position = "plot",
      plot.caption     = element_markdown(color = text2_col, size = rel(1.3), hjust = 0,
                                          halign = 0, margin = margin(t = 15, b = 5),
                                          lineheight = 1.1),
      plot.margin      = margin(15, 15, 10, 15),
      axis.ticks       = element_line(color = grid_col, linewidth = 0.3),
      complete         = TRUE
    )
}

# --- Semana 5: Uncertainties ---
#' Tema limpio y científico para gráficos de incertidumbre.
theme_week5 <- function(base_size = 11, base_family = "Lato") {
  bg_col    <- "#FFFFFF"
  text_col  <- "#333333"
  grid_col  <- "#E5E5E5"
  title_col <- "#212F3D"

  theme_light(base_size = base_size, base_family = base_family) %+replace%
    theme(
      plot.background  = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = bg_col, color = NA),
      panel.grid.major = element_line(color = grid_col, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      panel.border     = element_rect(color = grid_col, fill = NA, linewidth = 0.5),
      text             = element_text(color = text_col, family = base_family),
      plot.title       = element_text(size = rel(2.2), hjust = 0, face = "bold",
                                      margin = margin(b = 8), color = title_col),
      plot.subtitle    = element_text(size = rel(1.6), hjust = 0,
                                      margin = margin(b = 15), lineheight = 1.2),
      axis.text        = element_text(color = text_col, size = rel(1.2)),
      axis.title       = element_text(color = text_col, size = rel(1.4)),
      axis.line        = element_blank(),
      legend.position  = "bottom",
      legend.background = element_rect(fill = bg_col, color = NA),
      legend.key        = element_rect(fill = bg_col, color = NA),
      legend.text       = element_text(color = text_col, size = rel(1.2)),
      legend.title      = element_text(color = text_col, size = rel(1.4), face = "bold"),
      strip.background  = element_rect(fill = alpha(grid_col, 0.3), color = NA),
      strip.text        = element_text(face = "bold", color = text_col, size = rel(1.0)),
      plot.caption.position = "plot",
      plot.caption      = element_markdown(color = text_col, size = rel(1.2), hjust = 0,
                                           halign = 0, margin = margin(t = 15, b = 5),
                                           lineheight = 1.1),
      plot.margin       = margin(15, 15, 10, 15),
      axis.ticks        = element_line(color = grid_col, linewidth = 0.3),
      complete          = TRUE
    )
}


# --- Fin themes_30DCC2026.R ---

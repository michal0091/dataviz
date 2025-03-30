# --- themes_30DCC2025.R ---

# Cargar paquetes necesarios para definir los temas
# install.packages("ggplot2")
# install.packages("ggtext")
library(ggplot2)
library(ggtext)


# --- Paletas de Colores ---

# Paleta para la Semana 1 (Categoría: Comparisons)
# Colores: azul oscuro, gris oscuro, ocre, granate, azul medio
paleta_semana1 <- c("#324b64", "#373737", "#b4783c", "#963c3c", "#286e8c")


# --- 1. Definición de Vectores de Paletas ---

# Paleta para la Semana 1 (Categoría: Comparisons)
paleta_week1 <- c("#324b64", "#373737", "#b4783c", "#963c3c", "#286e8c")

# Paleta de gradiente genérico  
generic_gradient <- c("#f7fcfd", "#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#6e016b")
gradient_blue <- c("#FFFFFF", "#324b64")


# Paleta para la Semana 2 (Categoría: Comparisons)
# paleta_week2 <- c(...)


# --- 2. Lista Nombrada de Paletas ---
#    Agrupa todas las paletas definidas arriba con un nombre clave.
challenge_palettes <- list(
  `week1`    = paleta_week1,
  `gradient` = generic_gradient,
  `gradient_blue` = gradient_blue
  # `week2` = paleta_week2
)


# --- 3. Función Generadora de Paletas (Adaptada de my_pal) ---
#    Toma el nombre de la paleta, la recupera de la lista,
#    opcionalmente la invierte, y devuelve una función interpoladora.

#' Generador de funciones de paleta de colores para el challenge
#' @param palette Nombre de la paleta (debe ser una clave en challenge_palettes).
#' @param reverse Booleano, invertir la paleta?
#' @param ... Argumentos adicionales para colorRampPalette().
#' @return Una función que toma un entero (n) y devuelve n colores.
challenge_pal <- function(palette = "week1", # Cambiado default a week1
                          reverse = FALSE,
                          ...) {
  # Asegurarse que el nombre de la paleta existe
  if (!palette %in% names(challenge_palettes)) {
    stop(paste("Paleta no encontrada:", palette,
               "\nPaletas disponibles:", paste(names(challenge_palettes), collapse = ", ")))
  }

  pal <- challenge_palettes[[palette]] # Obtener el vector de colores

  if (reverse) {
    pal <- rev(pal) # Invertir si se pide
  }

  # Devolver la función interpoladora
  grDevices::colorRampPalette(pal, ...)
}


# --- 4. Funciones Constructoras de Escalas ggplot2 (Adaptadas de scale_color/fill_my) ---
#    Usan challenge_pal() para obtener la paleta y crean la escala apropiada.

#' Constructor de escalas de COLOR para ggplot2 usando paletas del challenge
#' @param palette Nombre de la paleta en challenge_palettes.
#' @param discrete Booleano, es escala discreta (TRUE) o continua (FALSE)?
#' @param reverse Booleano, invertir paleta?
#' @param ... Argumentos adicionales para discrete_scale() o scale_color_gradientn().
#' @return Una escala de ggplot2.
scale_color_challenge <- function(palette = "week1", # Cambiado default
                                  discrete = TRUE,
                                  reverse = FALSE,
                                  ...) {
  pal <- challenge_pal(palette = palette, reverse = reverse) # Obtener la función interpoladora

  if (discrete) {
    ggplot2::discrete_scale(aesthetics = "colour", scale_name = palette, palette = pal, ...)
  } else {
    ggplot2::scale_color_gradientn(colours = pal(256), ...) # Usar paleta continua
  }
}

#' Constructor de escalas de RELLENO para ggplot2 usando paletas del challenge
#' @param palette Nombre de la paleta en challenge_palettes.
#' @param discrete Booleano, es escala discreta (TRUE) o continua (FALSE)?
#' @param reverse Booleano, invertir paleta?
#' @param ... Argumentos adicionales para discrete_scale() o scale_fill_gradientn().
#' @return Una escala de ggplot2.
scale_fill_challenge <- function(palette = "week1", # Cambiado default
                                 discrete = TRUE,
                                 reverse = FALSE,
                                 ...) {
  pal <- challenge_pal(palette = palette, reverse = reverse) # Obtener la función interpoladora

  if (discrete) {
    ggplot2::discrete_scale(aesthetics = "fill", scale_name = palette, palette = pal, ...)
  } else {
    ggplot2::scale_fill_gradientn(colours = pal(256), ...) # Usar paleta continua
  }
}


# --- 5. Funciones de Tema ---

#' Tema de ggplot2 para la Semana 1 (Categoría: Comparisons) del #30DayChartChallenge 2025
#'
#' Utiliza la paleta de colores y fuentes definida por el usuario para la primera semana.
#' Asume que las fuentes 'Roboto' y 'Baumans' han sido cargadas con showtext.
#' Configura el caption para usar element_markdown.
#'
#' @param base_size Tamaño base de la fuente (default: 11).
#' @param base_family Familia de fuente base (default: 'Roboto').
#'
#' @return Un objeto de tema de ggplot2.
#'
theme_week1 <- function(base_size = 11, base_family = "Roboto") {

  # Colores definidos para la semana 1
  background_col <- "#b4a08c"
  text_col_main <- "#000000" # Fuente principal negra
  grid_col <- "#9e8a7a" # Color sutil para la rejilla, derivado del fondo

  # Empezar con theme_minimal y la fuente base Roboto
  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # --- Fondos ---
      plot.background = element_rect(fill = background_col, color = NA),
      panel.background = element_rect(fill = background_col, color = NA),

      # --- Rejilla (Grid) ---
      panel.grid.major = element_line(color = grid_col, linewidth = 0.3), # Rejilla principal sutil
      panel.grid.minor = element_blank(), # Sin rejilla menor

      # --- Textos ---
      text = element_text(color = text_col_main, family = base_family), # Texto general
      plot.title = element_text(family = "Baumans", # Fuente específica para título
                                size = rel(1.6), # Tamaño relativo al base_size
                                hjust = 0.5, # Centrado horizontal
                                margin = margin(b = 10), # Margen inferior
                                color = text_col_main),
      plot.subtitle = element_text(family = base_family, # Fuente Roboto
                                   size = rel(1.1),
                                   hjust = 0.5,
                                   margin = margin(b = 15), # Margen inferior
                                   color = text_col_main),
      axis.text = element_text(color = text_col_main, size = rel(0.8)), # Texto de ejes
      axis.title = element_text(color = text_col_main, size = rel(1.0), hjust = 0.5), # Títulos de ejes

      # --- Leyenda ---
      legend.position = "top", # Posición de la leyenda
      legend.background = element_rect(fill = background_col, color = NA), # Fondo transparente
      legend.box.background = element_rect(fill = background_col, color = NA), # Caja transparente
      legend.key = element_rect(fill = background_col, color = NA), # Claves transparentes
      legend.text = element_text(color = text_col_main, size = rel(0.8)),
      legend.title = element_text(color = text_col_main, size = rel(0.9), face = "bold"),

      # --- Caption ---
      plot.caption.position = "plot", # Posición relativa al gráfico completo
      plot.caption = element_markdown( # ¡IMPORTANTE! Usar element_markdown
                                       color = text_col_main, # Color base (puede ser sobreescrito por HTML)
                                       size = rel(0.75),
                                       hjust = 0.5, # Centrado horizontal (0=izq, 1=der)
                                       halign = 0.5, # Alineación horizontal del bloque de texto
                                       margin = margin(t = 15, b = 5) # Margen superior e inferior
                                      ),

      # --- Márgenes y Bordes ---
      plot.margin = margin(15, 15, 10, 15), # Márgenes generales del gráfico (arriba, der, abj, izq)
      panel.border = element_blank(), # Sin borde del panel
      axis.ticks = element_blank(), # Sin marcas de los ejes

      # Añadir aquí cualquier otra personalización que desees...
      complete = TRUE # Indica que es un tema completo
    )
}



ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point(size = 2) +
  labs(
    title = "Title of the plot\nTitle of the plot",
    subtitle = "Subtitle of the plot\nSubtitle of the plot",
    caption = "social_caption",
    tag = "Tag tex",
  ) +
    theme_week1() 

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
paleta_week2_tech <- c(
  "#4FC3F7", # Cyan claro
  "#81C784", # Verde claro
  "#F06292", # Rosa
  "#FFD54F", # Amarillo/Ámbar
  "#BA68C8"  # Púrpura claro
)
paleta_week3_animals <- c(
  "#6B8E23", # Verde Oliva Intenso
  "#BDB76B", # Caqui Oscuro / Musgo
  "#CD853F", # Ocre / Peru
  "#A0522D", # Sienna / Marrón Tierra
  "#8FBC8F", # Verde Mar Oscuro (más apagado)
  "#668096"  # Azul Pizarra Apagado
)
paleta_el_pais <- c(
  `text` = "#1f1f1f",
  `text2` = "#9c9c9c",
  `accent1` = "#0e6290", # Azul El País aprox.
  `accent2` = "#DE1D1A", # Rojo El País aprox.
  `grid` = "#E0E0E0",   # Gris claro para rejilla
  `bg` = "#FFFFFF"       # Fondo blanco
)

paleta_week4_social <- c(
  `indigo`   = "#3F51B5",
  `pink`     = "#FF4081",
  `teal`     = "#009688", 
  `orange`   = "#FF9800", 
  `purpureus` = "#9f499b", 
  `satin_gold` = "#c09822"   
)


paleta_week5_uncertainty <- c(
  `dark_grey`  = "#455A64",  # Azul Grisáceo 700
  `medium_grey`= "#78909C",  # Azul Grisáceo 500
  `light_grey` = "#B0BEC5",  # Azul Grisáceo 200
  `teal_accent`= "#4DB6AC",  # Teal 300
  `orange_accent`= "#FF8A65",  # Naranja Intenso 300
  `indigo_accent`= "#3F51B5"   # Índigo 500 (de Wk4)
)


# Colores base
colores_base_social <- list(
  bg = "#F2F2F2",
  text = "#333333",
  grid = "#DCDCDC", 
  primary = "#3F51B5",
  accent = "#FF4081" 
)

# Paleta de gradiente genérico  
generic_gradient <- c("#f7fcfd", "#e0ecf4", "#bfd3e6", "#9ebcda", "#8c96c6", "#8c6bb1", "#88419d", "#6e016b")
gradient_blue <- c("#FFFFFF", "#324b64")


# Paleta para la Semana 2 (Categoría: Comparisons)
# paleta_week2 <- c(...)


# --- 2. Lista Nombrada de Paletas ---
#    Agrupa todas las paletas definidas arriba con un nombre clave.
challenge_palettes <- list(
  `week1`    = paleta_week1,
  `week2_tech` = paleta_week2_tech, 
  `week3_animals` = paleta_week3_animals,
  `el_pais` = paleta_el_pais,
  `week4_social`    = paleta_week4_social,
  `week5_uncertainty`= paleta_week5_uncertainty,
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
    ggplot2::discrete_scale(aesthetics = "colour", palette = pal, ...)
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
    ggplot2::discrete_scale(aesthetics = "fill", palette = pal, ...)
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
  background_col <- "#c8baab"
  text_col_main <- "#000000" # Fuente principal negra
  grid_col <- "#b4a08c" # Color sutil para la rejilla, derivado del fondo

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
                                size = rel(3.2), # Tamaño relativo al base_size
                                hjust = 0, # Centrado horizontal
                                margin = margin(5, 10, 10, 0), # Margen inferior
                                lineheight = 0.3, # Ajustal altura de linea
                                color = text_col_main),
      plot.subtitle = element_text(family = base_family, # Fuente Roboto
                                   size = rel(2),
                                   hjust =0,
                                   margin = margin(0, 15, 15, 0), # Margen inferior
                                   lineheight = 0.3, # Ajustal altura de linea
                                   color = text_col_main),
      axis.text = element_text(color = text_col_main, size = rel(1.4)), # Texto de ejes
      axis.title = element_text(color = text_col_main, size = rel(1.6), hjust = 0.5), # Títulos de ejes

      # --- Leyenda ---
      legend.position = "top", # Posición de la leyenda
      legend.background = element_rect(fill = background_col, color = NA), # Fondo transparente
      legend.box.background = element_rect(fill = background_col, color = NA), # Caja transparente
      legend.key = element_rect(fill = background_col, color = NA), # Claves transparentes
      legend.key.size = unit(0.8, 'lines'), # Controla el tamaño de la clave 
      legend.text = element_text(color = text_col_main, size = rel(1.4)),
      legend.title = element_text(color = text_col_main, size = rel(1.6), face = "bold"),

      # --- Caption ---
      plot.caption.position = "plot", # Posición relativa al gráfico completo
      plot.caption = element_markdown( # ¡IMPORTANTE! Usar element_markdown
                                       color = text_col_main, # Color base (puede ser sobreescrito por HTML)
                                       size = rel(1.6),
                                       hjust = 0.5, # Centrado horizontal 
                                       halign = 0.5, # Alineación horizontal del bloque de texto
                                       lineheight = 0.5, # Ajustal altura de lines
                                       margin = margin(t = 10, b = 2) # Margen superior e inferior
                                      ),

      # --- Márgenes y Bordes ---
      plot.margin = margin(15, 15, 15, 15), # Márgenes generales del gráfico (arriba, der, abj, izq)
      panel.border = element_blank(), # Sin borde del panel
      axis.ticks = element_blank(), # Sin marcas de los ejes

      # Añadir aquí cualquier otra personalización que desees...
      complete = TRUE # Indica que es un tema completo
    )
}

# --- Función de Tema para Semana 2 (Tech Oscuro) ---

#' Tema ggplot2 estilo "Tech" (oscuro) para Semana 2 (Distributions)
#'
#' @param base_size Tamaño base fuente (default: 11).
#' @param base_family Familia fuente base (default: 'Roboto Mono').
#'
theme_week2_tech <- function(base_size = 11, base_family = "Roboto Mono") {

  # Colores base del tema oscuro
  bg_col <- "#202124"        # Fondo plot (gris muy oscuro Google)
  panel_col <- "#303134"     # Fondo panel (ligeramente más claro)
  text_col <- "#E8EAED"      # Texto principal (gris claro Google)
  grid_col <- "#5F6368"      # Rejillas (gris medio Google)
  title_font_family <- "Exo 2" # Fuente para títulos

  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # --- Aspect Ratio Cuadrado ---
      aspect.ratio = 1,

      # --- Fondos ---
      plot.background = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = panel_col, color = NA),

      # --- Rejilla ---
      panel.grid.major = element_line(color = grid_col, linewidth = 0.25), # Más fina
      panel.grid.minor = element_blank(),

      # --- Textos ---
      text = element_text(color = text_col, family = base_family),
      plot.title = element_text(family = title_font_family, size = rel(2.2), hjust = 0.5,
                                margin = margin(b = 10), color = text_col, face="bold"),
      plot.subtitle = element_text(family = base_family, size = rel(1.4), hjust = 0.5,
                                   margin = margin(b = 15), color = text_col),
      axis.text = element_text(color = text_col, size = rel(1.2)),
      axis.title = element_text(color = text_col, size = rel(1.4), hjust = 0.5),

      # --- Leyenda ---
      legend.position = "top",
      legend.background = element_rect(fill = bg_col, color = NA),
      legend.box.background = element_rect(fill = bg_col, color = NA),
      legend.key = element_rect(fill = panel_col, color = NA), # Fondo clave como panel
      legend.text = element_text(color = text_col, size = rel(1.2)),
      legend.title = element_text(color = text_col, size = rel(1.4), face = "bold"),
      legend.key.size = unit(0.5, 'cm'), # Mantenemos ajuste anterior

      # --- Caption ---
      plot.caption.position = "plot",
      plot.caption = element_markdown(color = text_col, size = rel(1.0), hjust = 0, # Alineado izquierda
                                      halign = 0, margin = margin(t = 15, b = 5),
                                      lineheight = 1.0),

      # --- Márgenes y Bordes ---
      plot.margin = margin(15, 15, 10, 10),
      panel.border = element_blank(),
      axis.line = element_line(color = grid_col, linewidth = 0.6), # Línea de ejes sutil
      axis.ticks = element_blank(),

      complete = TRUE
    )
}

#' Tema ggplot2 estilo "Naturaleza/Animales" para Semana 3 (Relationships)
#'
#' @param base_size Tamaño base fuente (default: 11).
#' @param base_family
#' 
theme_week3_animals <- function(base_size = 11, base_family = "Cabin") {

  # Colores base del tema "Animales"
  bg_col <- "#FAF0E6"      # Fondo Lino/Beige muy claro
  panel_col <- alpha(bg_col, 0.7) # Panel ligeramente transparente o igual
  text_col <- "#5D4037"      # Texto Marrón Oscuro
  grid_col <- "#D2B48C"      # Rejillas Tan/Arena claro (más sutil)
  line_col <- "#A0522D"      # Color para líneas importantes (Sienna)
  title_font_family <- "Cabin" # Podría ser "Cabin Condensed" si la cargas

  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # --- Aspect Ratio Cuadrado ---
      aspect.ratio = 1,

      # --- Fondos ---
      plot.background = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = panel_col, color = NA), # Puede ser igual a bg_col

      # --- Rejilla ---
      panel.grid.major = element_line(color = alpha(grid_col, 0.5), linewidth = 0.3), # Rejilla suave
      panel.grid.minor = element_blank(),

      # --- Textos ---
      text = element_text(color = text_col, family = base_family),
      plot.title = element_text(family = title_font_family, size = rel(2.2), hjust = 0.5,
                                  margin = margin(b = 10), color = text_col, face="bold"),
      plot.subtitle = element_text(family = base_family, size = rel(1.4), hjust = 0.5,
                                     margin = margin(b = 15), color = text_col),
      axis.text = element_text(color = text_col, size = rel(1.2)),
      axis.title = element_text(color = text_col, size = rel(1.4), hjust = 0.5),

      # --- Leyenda ---
      legend.position = "top",
      legend.background = element_rect(fill = bg_col, color = NA),
      legend.box.background = element_rect(fill = bg_col, color = NA),
      legend.key = element_rect(fill = panel_col, color = NA), # Fondo clave como panel
      legend.text = element_text(color = text_col, size = rel(1.2)),
      legend.title = element_text(color = text_col, size = rel(1.4), face = "bold"),
      legend.key.size = unit(0.5, 'cm'),

      # --- Caption ---
      plot.caption.position = "plot",
      plot.caption = element_markdown(color = text_col, size = rel(1.0), hjust = 0,
                                      halign = 0, margin = margin(t = 15, b = 5),
                                      lineheight = 1.0),

      # --- Márgenes y Bordes ---
      plot.margin = margin(15, 15, 10, 10),
      panel.border = element_blank(),
      axis.line = element_line(color = line_col, linewidth = 0.6), # Línea de ejes visible
      axis.ticks = element_blank(), # Sin ticks

      complete = TRUE
    )
}

#' Tema ggplot2 inspirado en El País
#'
#' @param base_size Tamaño base fuente (default: 11).
#' @param base_family Familia fuente base (default: 'Lato').
#'
theme_el_pais <- function(base_size = 11, base_family = "Lato") {

  # Colores de la paleta El País
  bg_col <- challenge_palettes$el_pais['bg']
  text_col <- challenge_palettes$el_pais['text']
  text2_col <- challenge_palettes$el_pais['text2']
  grid_col <- challenge_palettes$el_pais['grid']
  line_col <- challenge_palettes$el_pais['text'] # Líneas de eje oscuras

  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # --- Fondos ---
      plot.background = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = bg_col, color = NA),

      # --- Rejilla ---
      panel.grid.major = element_line(color = grid_col, linewidth = 0.3), # Solo rejilla principal suave
      panel.grid.minor = element_blank(),

      # --- Textos ---
      text = element_text(color = text_col, family = base_family),
      plot.title = element_text(family = base_family, size = rel(2.2), hjust = 0, # Título a la izquierda
                                  margin = margin(b = 8), face="bold"),
      plot.subtitle = element_text(family = base_family, size = rel(1.8), hjust = 0,
                                     margin = margin(b = 15), color = text2_col),
      axis.text = element_text(color = text2_col, size = rel(1.2)),
      axis.title = element_text(color = text_col, size = rel(1.4), hjust = 0.5),

      # --- Leyenda ---
      legend.position = "top",
      legend.justification = "left", # Leyenda alineada a la izquierda
      legend.background = element_rect(fill = bg_col, color = NA),
      legend.box.background = element_rect(fill = bg_col, color = NA),
      legend.key = element_rect(fill = bg_col, color = NA),
      legend.text = element_text(color = text2_col, size = rel(1.4)),
      legend.title = element_text(color = text_col, size = rel(1.6), face = "bold", hjust = 0.5),

      # --- Caption ---
      plot.caption.position = "plot",
      plot.caption = element_markdown(color = text2_col, size = rel(1.2), hjust = 0, # Alineado izquierda
                                      halign = 0, margin = margin(t = 15, b = 5),
                                      lineheight = 1.1),

      # --- Márgenes y Bordes ---
      plot.margin = margin(15, 15, 15, 15),
      panel.border = element_blank(),
      axis.line = element_line(color = line_col, linewidth = 0.5), # Línea de ejes visible
      axis.ticks = element_line(color = grid_col, linewidth = 0.35), # Ticks sutiles

      # --- Títulos Facetas (si se usan) ---
       strip.background = element_blank(), # Sin fondo
       strip.text = element_text(face = "bold", color = text_col, size = rel(1.6), hjust = 0, vjust = 0.5), # Texto strip a la izq.

      complete = TRUE
    )
}


#' Tema ggplot2 para Semana 4 (Timeseries - Social Themes)
#'
#' @param base_size Tamaño base fuente (default: 11).
#' @param base_family Familia fuente base (default: 'Lato').
#'
theme_week4_social <- function(base_size = 11, base_family = "Lato") {

  # Usar los colores base definidos previamente
  bg_col <- "#F2F2F2"
  text_col <- "#5c5c5c"
  title_col <- "#757de8"
  grid_col <- "#bfbfbf"
  line_col <- "#333333" 

  theme_minimal(base_size = base_size, base_family = base_family) %+replace%
    theme(
    
      # --- Fondos ---
      plot.background = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = bg_col, color = NA), # Fondo panel igual

      # --- Rejilla ---
      panel.grid.major = element_line(color = grid_col, linewidth = 0.35), # Rejilla principal clara
      panel.grid.minor = element_blank(), # Sin rejilla menor

      # --- Textos ---
      text = element_text(color = text_col, family = base_family),
      plot.title = element_text(family = base_family, size = rel(2.2), hjust = 0, # Título a la izquierda
                                  margin = margin(b = 8), face = "bold", color = title_col),
      plot.subtitle = element_text(family = base_family, size = rel(1.8), hjust = 0,
                                     margin = margin(b = 15)),
      axis.text = element_text(color = text_col, size = rel(1.4)),
      axis.title = element_text(color = text_col, size = rel(1.6), hjust = 0.5),

      # --- Leyenda ---
      legend.position = "top", # Arriba o 'bottom' suele ir bien en timeseries
      legend.justification = "left",
      legend.background = element_rect(fill = bg_col, color = NA),
      legend.box.background = element_rect(fill = bg_col, color = NA),
      legend.key = element_rect(fill = bg_col, color = NA), # Clave transparente
      legend.text = element_text(color = text_col, size = rel(1.4)),
      legend.title = element_text(color = text_col, size = rel(1.6), face = "bold"),

      # --- Caption ---
      plot.caption.position = "plot",
      plot.caption = element_markdown(color = text_col, size = rel(1.4), hjust = 0,
                                      halign = 0, margin = margin(t = 15, b = 5),
                                      lineheight = 1.1),

      # --- Márgenes y Bordes ---
      plot.margin = margin(15, 15, 10, 10),
      panel.border = element_blank(), # Sin borde de panel
      axis.line = element_line(color = line_col, linewidth = 0.5), # Línea de ejes visible
      axis.ticks = element_line(color = grid_col, linewidth = 0.3), # Ticks visibles pero claros

     # --- Títulos Facetas (si se usan) ---
     strip.background = element_rect(fill = alpha(grid_col, 0.5), color=NA), # Fondo suave para strips
     strip.text = element_text(face = "bold", color = text_col, size = rel(1.1), hjust = 0.5), # Texto strip centrado

      complete = TRUE
    )
}

#' Tema ggplot2 para Semana 5 (Uncertainties)
#' Base clara, limpia, enfocada en legibilidad y visualización de incertidumbre.
#'
#' @param base_size Tamaño base fuente (default: 11).
#' @param base_family Familia fuente base (default: 'Lato').
#'
theme_week5_uncertainty <- function(base_size = 11, base_family = "Lato") {

  # Colores base (se pueden sobreescribir en cada gráfico si es necesario)
  text_col <- "#333333"  # Texto gris oscuro
  grid_col <- "#E5E5E5"  # Rejilla gris claro
  bg_col <- "#FFFFFF"    # Fondo blanco (o F7F7F7 si prefieres off-white)
  text_col_title <-  "#455A64" # Fuente para títulos

  # Usar theme_light como base por sus ejes y panel definidos
  theme_light(base_size = base_size, base_family = base_family) %+replace%
    theme(
      # --- Sin Aspect Ratio Fijo ---
       aspect.ratio = NULL,

      # --- Fondos ---
      plot.background = element_rect(fill = bg_col, color = NA),
      panel.background = element_rect(fill = bg_col, color = NA),

      # --- Rejilla ---
      panel.grid.major = element_line(color = grid_col, linewidth = 0.3),
      panel.grid.minor = element_blank(),

      # --- Textos ---
      text = element_text(color = text_col, family = base_family),
      plot.title = element_text(size = rel(2.2), hjust = 0, # Título a la izquierda
                                  margin = margin(b = 8), face = "bold", color =  text_col_title),
      plot.subtitle = element_text(size = rel(1.8), hjust = 0,
                                     margin = margin(b = 15)),
      axis.text = element_text(color = text_col, size = rel(1.2)),
      axis.title = element_text(color = text_col, size = rel(1.4), hjust = 0.5),

      # --- Leyenda ---
      legend.position = "bottom", # Abajo para dejar espacio al gráfico
      legend.background = element_rect(fill = bg_col, color = NA),
      legend.key = element_rect(fill = bg_col, color = NA),
      legend.text = element_text(color = text_col, size = rel(1.2)),
      legend.title = element_text(color = text_col, size = rel(1.4), face = "bold"),

      # --- Caption ---
      plot.caption.position = "plot",
      plot.caption = element_markdown(color = text_col, size = rel(1.2), hjust = 0,
                                      halign = 0, margin = margin(t = 15, b = 5),
                                      lineheight = 1.1),

      # --- Márgenes y Bordes ---
      plot.margin = margin(15, 15, 10, 15), # Un poco más de margen izquierdo/derecho
      panel.border = element_rect(color = grid_col, fill=NA, linewidth=0.5), # Borde panel suave
      axis.line = element_blank(), # theme_light ya tiene panel.border
      axis.ticks = element_line(color = grid_col, linewidth = 0.3), # Ticks visibles

     # --- Títulos Facetas (si se usan) ---
     strip.background = element_rect(fill = alpha(grid_col, 0.3), color=NA), # Fondo suave para strips
     strip.text = element_text(face = "bold", color = text_col, size = rel(1.0), hjust = 0.5),

      complete = TRUE
    )
}


# --- Fin themes_30DCC2025.R ---
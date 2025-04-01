# --- utils.R ---

library(glue)
library(ggtext)
library(sysfonts)
library(showtext)
library(ggplot2)

font_add(family = "fa-brands", regular = "fonts/fa-brands-400.ttf")
font_add(family = "fa-solid", regular = "fonts/fa-solid-900.ttf")

showtext_auto()

#' Genera el texto del pie de foto (caption) para los gráficos del #30DayChartChallenge
#'
#' Utiliza la información del archivo de configuración YAML y los detalles específicos del día.
#' Requiere que ggtext esté instalado y una fuente de iconos (ej. Font Awesome) configurada con showtext.
#'
#' @param day El número del día (1-30).
#' @param source_text El texto que describe la fuente de los datos para ese día.
#' @param config El objeto de configuración cargado desde el archivo YAML.
#' @param color_text_author Color para el autor y hashtags principales (predeterminado: azul).
#' @param color_text_source Color para la fuente y nombres de usuario (predeterminado: negro/gris).
#' @param icon_font_family Nombre de la familia de la fuente de iconos (predeterminado: 'Font Awesome 6 Brands').
#'
#' @return Una cadena de texto formateada en HTML/Markdown lista para usar con ggtext::element_markdown().
#'
generate_caption <- function(day,
  source_text,
  config,
  color_text_author = "#286e8c", # Azul de tu paleta semana 1
  color_text_source = "#373737", # Gris oscuro de tu paleta semana 1
  icon_font_family = "fa-brands") { # Asegúrate de que este nombre coincide con cómo lo carga showtext

# Extraer información por defecto del config
defaults <- config$defaults
author <- defaults$author_name
social_media <- defaults$social_media
main_hashtag <- defaults$main_hashtag

# Extraer información del día específico del config
daily_info <- config$daily_prompts[[day]]
if (is.null(daily_info)) {
stop(paste("No se encontró información para el día", day, "en el archivo de configuración."))
}
theme_name <- daily_info$theme
day_hashtag <- paste0("#Day", day) # Hashtag #DayN

# Construir la parte de redes sociales dinámicamente
social_html_parts <- character()
build_social_part <- function(icon_code_html, username) {
  glue(" <span style='font-family:\"{icon_font_family}\"; color: {color_text_author};'>{icon_code_html};</span><span style='color: {color_text_source}'> {username}</span>     ")
}
  
build_mastodon_part <- function(icon_code_html, username, server) {
  glue("<span style='font-family:\"{icon_font_family}\"; color: {color_text_author};'>{icon_code_html};</span><span style='color: {color_text_source}'> {username}@</span><span style='color: {color_text_source}'>{server}</span>     ")
  }

if (!is.null(social_media$github_username) && !is.null(social_media$github_icon)) {
  social_html_parts <- c(social_html_parts, build_social_part(social_media$github_icon, social_media$github_username))
}

if (!is.null(social_media$linkedin_username) && !is.null(social_media$linkedin_icon)) {
  social_html_parts <- c(social_html_parts, build_social_part(social_media$linkedin_icon, social_media$linkedin_username))
}

if (!is.null(social_media$mastodon_username) && !is.null(social_media$mastodon_icon) && !is.null(social_media$mastodon_server)) {
   social_html_parts <- c(social_html_parts, build_mastodon_part(social_media$mastodon_icon, social_media$mastodon_username, social_media$mastodon_server))
}


# Separador entre redes sociales: varios espacios sin ruptura
social_string <- paste(social_html_parts, collapse = "&nbsp;&nbsp;&nbsp;") 

  
# Construir el caption final usando glue
caption <- glue::glue(
"<span style='color: {color_text_author};'><strong>Viz:</strong></span> <span style='color: {color_text_source};'>{author}     </span> | ",
"<span style='color: {color_text_author};'><strong>     Source:</strong></span> <span style='color: {color_text_source};'>{source_text}</span><br>",
"{social_string}<br>",
"<span style='color: {color_text_author};'><strong>{main_hashtag}          </strong></span> | <span style='color: {color_text_author};'>     {day_hashtag}: {theme_name}</span>"
)

return(caption)
}

# --- Función para Configurar Fuentes ---

#' Carga las fuentes necesarias para el #30DayChartChallenge usando showtext
#'
#' Carga Roboto, Baumans desde Google Fonts y Font Awesome Brands desde un archivo local.
#' Activa showtext para su uso en gráficos.
#'
#' @param fa_brands_path Ruta al archivo .ttf o .otf de Font Awesome Brands.
#'                       Por defecto, busca en "./fonts/fa-brands-400.ttf" relativo
#'                       al directorio de trabajo actual. ¡AJUSTA SI ES NECESARIO!
#'
setup_fonts <- function(fa_brands_path = "fonts/fa-brands-400.ttf") {

  # Cargar fuentes de Google Fonts
  font_add_google("Roboto", "Roboto")
  font_add_google("Baumans", "Baumans")

  # Cargar fuente de iconos local (Font Awesome Brands)
  # ¡¡VERIFICA QUE LA RUTA 'fa_brands_path' ES CORRECTA!!
  # Puedes pasar una ruta absoluta si la relativa da problemas.
  if (file.exists(fa_brands_path)) {
    font_add(family = "fa-brands", regular = fa_brands_path)
  } else {
    warning(paste("Archivo de fuente Font Awesome Brands no encontrado en:",
                  normalizePath(fa_brands_path, mustWork = FALSE),
                  "\nLos iconos no se mostrarán correctamente."))
  }

  # Activar showtext para que ggplot use estas fuentes
  showtext_auto()

  # Opcional: Imprimir familias cargadas para verificar
  # print("Familias de fuentes disponibles para showtext:")
  # print(font_families())
}

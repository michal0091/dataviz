# --- utils.R ---
# Utilidades compartidas para el #30DayChartChallenge 2026

library(glue)
library(ggtext)
library(sysfonts)
library(showtext)
library(ggplot2)

font_add(family = "fa-brands", regular = "fonts/fa-brands-400.ttf")
font_add(family = "fa-solid",  regular = "fonts/fa-solid-900.ttf")
showtext_auto()


# --- Configuración de Fuentes ------------------------------------------------

#' Carga las fuentes necesarias para el #30DayChartChallenge usando showtext.
#' Carga fuentes de Google Fonts y Font Awesome desde archivo local.
#'
#' @param fa_brands_path Ruta al archivo fa-brands-400.ttf (default: "fonts/fa-brands-400.ttf").
#' @param extra_fonts Named list of extra Google Fonts to load, e.g. list("Oswald" = "Oswald").
#'
setup_fonts <- function(fa_brands_path = "fonts/fa-brands-400.ttf",
                        extra_fonts    = list()) {

  font_add_google("Roboto",      "Roboto")
  font_add_google("Roboto",      "Roboto Bold",  regular.wt = 700)
  font_add_google("Roboto Mono", "Roboto Mono")
  font_add_google("Lato",        "Lato")
  font_add_google("Baumans",     "Baumans")
  font_add_google("Exo 2",       "Exo 2")
  font_add_google("Cabin",       "Cabin")
  font_add_google("Gudea",       "Gudea")

  # Fuentes adicionales opcionales
  for (nm in names(extra_fonts)) {
    font_add_google(nm, extra_fonts[[nm]])
  }

  # Font Awesome Brands (iconos sociales)
  if (file.exists(fa_brands_path)) {
    font_add(family = "fa-brands", regular = fa_brands_path)
  } else {
    warning(paste(
      "Archivo Font Awesome Brands no encontrado en:",
      normalizePath(fa_brands_path, mustWork = FALSE),
      "\nLos iconos sociales no se mostrarán correctamente."
    ))
  }

  showtext_auto()
}


# --- Caption Generator -------------------------------------------------------

#' Genera el caption HTML/Markdown para los gráficos del #30DayChartChallenge.
#'
#' @param day           Número de día (1-30).
#' @param source_text   Texto de la fuente de datos.
#' @param config        Objeto de configuración cargado desde el YAML.
#' @param color_author  Color para autor y hashtags (default: azul).
#' @param color_source  Color para la fuente y usernames (default: gris oscuro).
#' @param icon_family   Nombre de la familia de fuente de iconos (default: "fa-brands").
#'
#' @return String con HTML/Markdown listo para ggtext::element_markdown().
#'
generate_caption <- function(day,
                             source_text,
                             config,
                             color_author = "#286e8c",
                             color_source = "#373737",
                             icon_family  = "fa-brands") {

  defaults     <- config$defaults
  author       <- defaults$author_name
  social       <- defaults$social_media
  main_hashtag <- defaults$main_hashtag

  daily_info <- config$daily_prompts[[day]]
  if (is.null(daily_info)) {
    stop(paste("No se encontró información para el día", day, "en el archivo de configuración."))
  }

  theme_name <- daily_info$theme
  day_type   <- daily_info$type   # "regular", "data_day", "theme_day"
  day_label  <- paste0("#Day", day)

  # Construir partes de redes sociales
  .social_span <- function(icon_html, username) {
    glue(
      "<span style='font-family:\"{icon_family}\"; color:{color_author};'>{icon_html};</span>",
      "<span style='color:{color_source}'> {username}</span>     "
    )
  }
  .mastodon_span <- function(icon_html, username, server) {
    glue(
      "<span style='font-family:\"{icon_family}\"; color:{color_author};'>{icon_html};</span>",
      "<span style='color:{color_source}'> {username}@{server}</span>     "
    )
  }

  social_parts <- character()
  if (!is.null(social$github_username))   social_parts <- c(social_parts, .social_span(social$github_icon,   social$github_username))
  if (!is.null(social$linkedin_username)) social_parts <- c(social_parts, .social_span(social$linkedin_icon, social$linkedin_username))
  if (!is.null(social$mastodon_username)) social_parts <- c(social_parts, .mastodon_span(social$mastodon_icon, social$mastodon_username, social$mastodon_server))

  social_string <- paste(social_parts, collapse = "&nbsp;&nbsp;&nbsp;")

  # Etiqueta de tipo de día
  type_label <- switch(day_type,
    data_day  = paste0(" <em>(Data Day)</em>"),
    theme_day = paste0(" <em>(Theme Day)</em>"),
    ""
  )

  glue::glue(
    "<span style='color:{color_author};'><strong>Viz:</strong></span> ",
    "<span style='color:{color_source};'>{author}     </span> | ",
    "<span style='color:{color_author};'><strong>Source:</strong></span> ",
    "<span style='color:{color_source};'>{source_text}</span><br>",
    "{social_string}<br>",
    "<span style='color:{color_author};'><strong>{main_hashtag}</strong></span>",
    " | <span style='color:{color_author};'>{day_label}: {theme_name}{type_label}</span>"
  )
}

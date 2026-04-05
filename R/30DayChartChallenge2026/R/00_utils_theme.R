# =============================================================================
# 00_utils_theme.R — Tema base para el #30DayChartChallenge 2026
# =============================================================================
library(ggplot2)
library(ggtext)
library(showtext)
library(sysfonts)
library(glue)

# --- 1. CONFIGURACIÓN DE FUENTES ---
#' @param extra_fonts Vector de caracteres con nombres exactos de Google Fonts
setup_fonts_2026 <- function(extra_fonts = NULL) {
  
  # Fuentes base Funk (Tech/Neón)
  font_add_google("Space Grotesk", "Space Grotesk")
  font_add_google("Outfit", "Outfit")
  
  # Fuentes base Sobrias (Editorial/Distribuciones)
  font_add_google("Lora", "Lora")
  font_add_google("Fira Sans", "Fira Sans")
  
  # Si el usuario pide más fuentes (ej. para un Theme Day), las iteramos
  if (!is.null(extra_fonts)) {
    for (font_name in extra_fonts) {
      # Usamos tryCatch por si hay un error tipográfico en el nombre
      tryCatch({
        font_add_google(font_name, font_name)
      }, error = function(e) {
        warning(glue::glue("No se pudo cargar la fuente: {font_name}"))
      })
    }
  }
  
  font_add(family = "fa-brands", regular = "fonts/fa-brands-400.ttf")
  font_add(family = "fa-solid", regular = "fonts/fa-solid-900.ttf")
  showtext_auto()
}

setup_fonts_cat3 <- function() {
  font_add_google("IBM Plex Sans")
  font_add_google("Inter", "Inter")
  showtext_auto()
}

showtext_auto()
# --- 2. PALETAS DE COLOR ---

# Paleta original (Días 1-6)
paleta_funk_2026 <- c(
  lima = "#c0d10e",
  azul_claro = "#4399ef",
  cyan = "#00c8ef",
  teja = "#c2562f",
  naranja = "#e58131",
  crema = "#e0c5ac",
  oliva = "#688337",
  mostaza = "#cfb423"
)

# Paleta nueva (Distribuciones / Editorial)
paleta_sobria_2026 <- c(
  fondo = "#faf5ed",       # Crema/Blanco roto para el lienzo
  magenta = "#cd2162",     # Color de acento brutal
  pino = "#4f766f",        # Verde oscuro apagado
  coral = "#e2928d",       # Rosa salmón
  malva = "#a88b93",       # Morado grisáceo
  pizarra = "#6d7172",     # Gris medio para textos/ejes
  dorado = "#e6c570",      # Amarillo arena
  nude = "#f4caab"         # Piel/Melocotón claro
)

# Tu nueva paleta "Relacional"
paleta_relaciones <- c(
  fondo    = "#e6e9f0", # Gris gélido
  marino   = "#04346a", # Texto y líneas base
  cian     = "#3bd0e4", # Destacado principal (Positivo)
  naranja  = "#e1913b", # Destacado secundario (Divergente)
  coral    = "#e13b3b", # Nodos/Categorías cálidas
  alerta   = "#e3111f"  # Rojo puro para causalidad/peligro
)

# --- 3. FUNCIÓN DE CAPTION ---
generar_caption_2026 <- function(dia, tema_dia, fuente_datos, color_autor, color_texto) {
  
  # Códigos Unicode de FontAwesome
  icon_github <- "&#xf09b;"
  icon_linkedin <- "&#xf08c;"
  icon_mastodon <- "&#xf4f6;"
  
  # Construimos el HTML exactamente con tu layout de NatGeo
  caption <- glue(
    # Línea 1: Viz y Source
    "<span style='color: {color_autor};'>**Viz:**</span> <span style='color: {color_texto};'>Michal Kinel</span> | ",
    "<span style='color: {color_autor};'>**Source:**</span> <span style='color: {color_texto};'>{fuente_datos}</span><br>",
    
    # Línea 2: Redes Sociales con Iconos
    "<span style='font-family:\"fa-brands\"; color: {color_texto};'>{icon_github}</span> <span style='color: {color_texto};'>michal0091</span> &nbsp;&nbsp;",
    "<span style='font-family:\"fa-brands\"; color: {color_texto};'>{icon_linkedin}</span> <span style='color: {color_texto};'>michal-kinel</span> &nbsp;&nbsp;<br>",
    
    # Línea 3: Hashtags
    "<span style='color: {color_autor};'>**#30DayChartChallenge2026**</span> | <span style='color: {color_texto};'>#Day{dia}: {tema_dia}</span>"
  )
  
  return(caption)
}

# --- 4. TEMA ESTRUCTURAL PARAMETRIZADO ---
#' @param bg_color Color del fondo del gráfico y panel
#' @param text_color Color principal para títulos, ejes y texto
#' @param title_font Familia tipográfica para títulos
#' @param text_font Familia tipográfica para el resto de textos
theme_30dcc_base <- function(base_size = 12,
                             bg_color = "#ffffff", 
                             text_color = "#1a1a1a",
                             title_font = "Space Grotesk",
                             text_font = "Outfit") {
  
  theme_minimal(base_size = base_size, base_family = text_font) %+replace%
    theme(
      # Fondos dinámicos
      plot.background = element_rect(fill = bg_color, color = NA),
      panel.background = element_rect(fill = bg_color, color = NA),
      
      # Textos generales
      text = element_text(color = text_color, family = text_font),
      
      # Títulos
      plot.title = element_text(family = title_font, face = "bold", 
                                size = rel(2.4), color = text_color, 
                                margin = margin(t = 10, b = 5), hjust = 0),
      plot.subtitle = element_text(family = text_font, size = rel(1.3), 
                                   color = text_color, margin = margin(b = 20), hjust = 0),
      
      # Ejes
      axis.text = element_text(color = text_color, size = rel(1.1)),
      axis.title = element_text(family = title_font, color = text_color, 
                                face = "bold", size = rel(1.2)),
      
      # Rejilla (Usa el color del texto pero con opacidad al 15%)
      panel.grid.major.y = element_line(color = alpha(text_color, 0.15), linewidth = 0.4),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Leyenda (Fondos transparentes para no romper el color de fondo)
      legend.position = "top",
      legend.justification = "left",
      legend.background = element_rect(fill = "transparent", color = NA),
      legend.key = element_rect(fill = "transparent", color = NA),
      legend.title = element_blank(),
      legend.text = element_text(size = rel(1.1), color = text_color),
      
      # Caption
      plot.caption = element_markdown(family = text_font, size = rel(1), 
                                      color = text_color, hjust = 0, 
                                      lineheight = 1.2, margin = margin(t = 20, b = 5)),
      
      # Márgenes para formato 4:5
      plot.margin = margin(t = 20, r = 25, b = 20, l = 25)
    )
}
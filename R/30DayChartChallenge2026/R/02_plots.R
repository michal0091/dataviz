# =============================================================================
# 02_plots.R — Plantillas de funciones de graficado
# #30DayChartChallenge 2026
#
# Convención:
#   plot_diaXX(dt, tema, config, output) → construye, muestra y guarda el plot
# =============================================================================
library(ggplot2)
library(stringr)
library(ggtext)
library(showtext)


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

plot_dia01_satelites <- function(dt_limpio, paleta) {
  
  setup_fonts_2026()
  showtext_opts(dpi = 300) 
  
  fondo_dia <- "#14141c" 
  texto_dia <- "#ffffff" 
  texto_secundario <- "#a0aab5" 
  color_acento <- unname(paleta["naranja"])
  
  colores_satelites <- c(
    "SpaceX (Starlink)" = unname(paleta["naranja"]),
    "OneWeb" = unname(paleta["lima"]),
    "Amazon Kuiper" = unname(paleta["cyan"]),
    "China (Gov & Constelaciones)" = unname(paleta["teja"]),
    "EE.UU. (NASA, Militar, Inteligencia)" = unname(paleta["oliva"]),
    "Otros Gigantes Comerciales" = unname(paleta["mostaza"]),
    "Resto del Mundo (Agencias y Univ.)" = "#4a4f55" 
  )
  
  dt_limpio$y_dummy <- "Orbita"
  
  p <- ggplot(dt_limpio, aes(x = pct, y = y_dummy, fill = categoria_limpia)) +
    geom_col(width = 0.8, color = fondo_dia, linewidth = 1.5, 
             position = position_stack(reverse = TRUE)) +
    
    scale_fill_manual(values = colores_satelites) +
    scale_x_continuous(labels = scales::percent_format(), expand = c(0, 0)) +
    
    guides(fill = guide_legend(ncol = 2, byrow = TRUE, override.aes = list(color = NA))) +
    
    labs(
      title = "El Monopolio de la Órbita Baja",
      subtitle = str_wrap(paste0("SpaceX controla actualmente más del ", 
      dt_limpio[categoria_limpia == "SpaceX (Starlink)", as.character(floor(100 * pct))],
      "% de todos los satélites activos frente a la infraestructura de gobiernos y otras corporaciones."), 65),
      caption = generar_caption_2026(
        dia = "01", 
        tema_dia = "Part-to-Whole", 
        fuente_datos = "GCAT (Jonathan C. McDowell)",
        color_autor = color_acento,
        color_texto = texto_secundario
      )
    ) +
    
    theme_minimal(base_size = 18, base_family = "Outfit") +
    theme(
      plot.background = element_rect(fill = fondo_dia, color = NA),
      panel.background = element_rect(fill = fondo_dia, color = NA),
      text = element_text(color = texto_dia),
      
      plot.title = element_text(family = "Space Grotesk", face = "bold", size = rel(1.8), margin = margin(b = 10), hjust = 0),
      plot.subtitle = element_text(size = rel(1), color = texto_secundario, margin = margin(b = 40), lineheight = 1.2, hjust = 0),
      
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(color = texto_secundario, size = rel(0.9)),
      panel.grid.major.x = element_line(color = "#2a2d34", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      panel.grid.major.y = element_blank(),
      
      legend.position = "top",
      legend.justification = "left",
      legend.margin = margin(b = 20),
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.7), color = texto_secundario),
      legend.key.size = unit(0.8, "cm"),
      legend.background = element_rect(fill = fondo_dia, color = NA),
      
      plot.caption = element_markdown(size = rel(0.7), color = texto_secundario, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.margin = margin(30, 30, 30, 30)
    )
    
  return(p)
}


# =============================================================================
# DÍA 02 — Pictogram (Comparisons)
# =============================================================================

plot_dia02_pictogram <- function(dt_grid, paleta) {
  
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  
  fondo_dia <- "#14141c" 
  texto_dia <- "#ffffff" 
  texto_secundario <- "#a0aab5" 
  
  colores_consolas <- c(
    "PlayStation 2" = unname(paleta["cyan"]),      
    "Nintendo DS" = unname(paleta["teja"]),        
    "Nintendo Switch" = unname(paleta["naranja"]), 
    "Game Boy" = unname(paleta["lima"])            
  )
  
  p <- ggplot(dt_grid, aes(x = x, y = y, color = consola)) +
    
    # LA MAGIA DE GGTEXT: Usamos richtext para forzar el HTML y la fuente por CSS
    geom_richtext(
      label = "<span style='font-family:\"fa-solid\";'>&#xf11b;</span>", 
      size = 3, 
      fill = NA,         # Quitamos el fondo de la caja de texto
      label.color = NA,  # Quitamos el borde de la caja
      label.padding = unit(rep(0, 4), "pt") # Quitamos los márgenes internos
    ) +
    scale_x_continuous(expand = expansion(add = 0.95)) +
    scale_y_continuous(expand = expansion(add = 0.95)) +
    facet_wrap(~consola, ncol = 1) +
    scale_color_manual(values = colores_consolas) +
    coord_fixed(ratio = 1, clip = "off") +
    
    labs(
      title = "La Guerra del Salón",
      subtitle = str_wrap("Las videoconsolas más vendidas de la historia. Cada mando representa 2 millones de unidades comercializadas a nivel global.", 50),
      caption = generar_caption_2026(
        dia = "02", 
        tema_dia = "Pictogram", 
        fuente_datos = "Sony & Nintendo",
        color_autor = unname(paleta["naranja"]),
        color_texto = texto_secundario
      )
    ) +
    
    theme_minimal(base_size = 18, base_family = "Outfit") +
    theme(
      plot.background = element_rect(fill = fondo_dia, color = NA),
      panel.background = element_rect(fill = fondo_dia, color = NA),
      text = element_text(color = texto_dia),
      
      plot.title = element_text(family = "Space Grotesk", face = "bold", size = rel(2.2), margin = margin(b = 10)),
      plot.subtitle = element_text(size = rel(1.1), color = texto_secundario, margin = margin(b = 30), lineheight = 1.2),
      
      strip.text = element_text(family = "Space Grotesk", face = "bold", color = texto_dia, size = rel(1.4), hjust = 0, margin = margin(t = 20, b = 10)),
      
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid = element_blank(),
      legend.position = "none",
      
      plot.caption = element_markdown(size = rel(0.7), color = texto_secundario, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.margin = margin(30, 30, 30, 30)
    )
    
  return(p)
}


# =============================================================================
# DÍA 03 — Mosaic (Comparisons)
# =============================================================================

plot_dia03_mosaico <- function(dt, paleta) {
  
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  
  fondo_dia <- "#14141c" 
  texto_dia <- "#ffffff" 
  texto_secundario <- "#a0aab5"
  colores_energia <- c(
    "Fósil" = unname(paleta["naranja"]),
    "Limpia" = unname(paleta["cyan"])
  )
  
  p <- ggplot(dt) +
    geom_rect(aes(xmin = xmin, xmax = xmax, ymin = ymin, ymax = ymax, fill = tipo),
              color = fondo_dia, linewidth = 2) + 
    geom_text(aes(x = x_mid, y = y_mid, 
                  label = fifelse(xmax - xmin > 0.05, paste0(round(pct_y * 100), "%"), "")),
              family = "Space Grotesk", fontface = "bold", size = 6, color = fondo_dia) +
    scale_fill_manual(values = colores_energia) +
    scale_x_continuous(breaks = unique(dt$x_mid), labels = unique(dt$continente), expand = c(0.05, 0.05)) +
    scale_y_continuous(labels = scales::percent_format(), expand = c(0.01, 0.01)) +
    
    labs(
      title = "El Gigante de Carbón",
      subtitle = str_wrap("Matriz de generación eléctrica global (2022). El ancho del bloque representa el volumen total generado por región; el alto, la proporción de fuentes limpias (Nuclear/Renovable) frente a combustibles fósiles.", 65),
      caption = generar_caption_2026("03", "Mosaic", "Our World in Data (Energy Institute)", unname(paleta["cyan"]), texto_secundario)
    ) +
    theme_minimal(base_size = 16, base_family = "Outfit") +
    theme(
      plot.background = element_rect(fill = fondo_dia, color = NA),
      panel.background = element_rect(fill = fondo_dia, color = NA),
      text = element_text(color = texto_dia),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Space Grotesk", face = "bold", size = rel(2.2), margin = margin(b = 10)),
      plot.subtitle = element_text(size = rel(1.1), color = texto_secundario, margin = margin(b = 30), lineheight = 1.2),
      
      # Eje X: Nombres de continentes inclinados para que los pequeños no colisionen
      axis.text.x = element_text(color = texto_dia, face = "bold", size = rel(1.2), angle = 45, hjust = 1, margin = margin(t = 10)),
      axis.text.y = element_text(color = texto_secundario, size = rel(1)),
      
      axis.title = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      # Leyenda elegante arriba
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(1.1), color = texto_dia, face = "bold"),
      legend.key.size = unit(1, "cm"),
      legend.margin = margin(b = 10),
      
      plot.caption = element_markdown(size = rel(0.7), color = texto_secundario, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.margin = margin(40, 40, 40, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 04 — Slope (Comparisons)
# =============================================================================

plot_dia04_slope <- function(dt, paleta) {
  
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  
  fondo_dia <- "#14141c" 
  texto_dia <- "#ffffff" 
  texto_secundario <- "#a0aab5" 
  
  # Mapeamos los colores a tu paleta Funk
  colores_trend <- c(
    "Aceleración Masiva" = unname(paleta["cyan"]),
    "Progreso Constante" = unname(paleta["lima"]),
    "Estancamiento o Caída" = unname(paleta["teja"])
  )
  
  # Dividimos los datos para anclar las etiquetas a la izquierda y a la derecha
  dt_2000 <- dt[year == 2000]
  dt_2022 <- dt[year == 2022]
  
  p <- ggplot(dt, aes(x = year, y = share, group = country_es, color = trend)) +
    geom_line(linewidth = 1.5, alpha = 0.85) +
    geom_point(size = 4) +
    geom_text(data = dt_2000, aes(label = label), show.legend = FALSE,
              hjust = 1, nudge_x = -0.8, family = "Space Grotesk", fontface = "bold", size = 4.5) +
    geom_text(data = dt_2022, aes(label = label), show.legend = FALSE, 
              hjust = 0, nudge_x = 0.8, family = "Space Grotesk", fontface = "bold", size = 4.5) +
    
    scale_color_manual(values = colores_trend) +
    scale_x_continuous(breaks = c(2000, 2022), limits = c(1990, 2032), expand = c(0.12, 0.12)) +
    coord_cartesian(clip = "off") +
    
    labs(
      title = "La Carrera Renovable",
      subtitle = str_wrap("Evolución de la cuota de generación eléctrica mediante fuentes renovables en el siglo XXI. Destacan los saltos masivos de Reino Unido y Alemania, frente al estancamiento de Japón y EE.UU.", 65),
      caption = generar_caption_2026("04", "Slope", "Our World in Data (Energy Institute)", unname(paleta["cyan"]), texto_secundario)
    ) +
    
    theme_minimal(base_size = 16, base_family = "Outfit") +
    theme(
      plot.background = element_rect(fill = fondo_dia, color = NA),
      panel.background = element_rect(fill = fondo_dia, color = NA),
      text = element_text(color = texto_dia),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Space Grotesk", face = "bold", size = rel(2), margin = margin(b = 10)),
      plot.subtitle = element_text(size = rel(1.1), color = texto_secundario, margin = margin(b = 40), lineheight = 1.2),
      
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      panel.grid.major.y = element_blank(),
      panel.grid.minor.y = element_blank(),
      
      # Mantenemos las líneas verticales del año 2000 y 2022 sutiles
      axis.text.x = element_text(color = texto_dia, face = "bold", size = rel(1.2)),
      panel.grid.major.x = element_line(color = "#2a2d34", linewidth = 0.8, linetype = "dashed"),
      panel.grid.minor.x = element_blank(),
      
      # Subimos la leyenda y la hacemos más elegante
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.8), color = texto_dia, face = "bold"),
      legend.key.width = unit(1, "cm"),
      legend.margin = margin(b = 20),
      
      plot.caption = element_markdown(size = rel(0.7), color = texto_secundario, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.margin = margin(20, 20, 20, 20)
    )
    
  return(p)
}
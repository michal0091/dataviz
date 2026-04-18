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
library(ggridges)
library(ggraph)
library(ggrepel)
library(ggtext)
library(ggfx)
library(tidygraph)
library(showtext)
library(logger)
library(glue)
library(ichimoku)
library(cowplot) 
library(magick)
library(gganimate)
library(gifski)

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
      subtitle = str_wrap("Evolución de la cuota de generación eléctrica mediante fuentes renovables en el siglo XXI. Destacan los saltos masivos de España y Alemania, frente al estancamiento de Japón y EE.UU.", 65),
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


# =============================================================================
# DÍA 05 — Experimental (Comparisons)
# =============================================================================

plot_dia05_experimental <- function(dt, paleta) {
  
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  
  fondo_dia <- "#14141c" 
  texto_dia <- "#ffffff" 
  texto_secundario <- "#a0aab5" 
  
  color_2000 <- unname(paleta["oliva"])
  color_2022 <- unname(paleta["cyan"]) 
  color_linea <- "#2a2d34"            
  
  max_val <- max(dt$y2022, na.rm = TRUE)
  
  p <- ggplot(dt, aes(x = country)) +
    geom_segment(aes(xend = country, y = y2000, yend = y2022), 
                 color = color_linea, linewidth = 2) +
    geom_point(aes(y = y2000), color = color_2000, size = 3, alpha = 0.6) +
    geom_point(aes(y = y2022), color = color_2022, size = 5) +

    geom_text(aes(y = max_val * 1.1, label = country, angle = angle, hjust = hjust), 
              color = texto_dia, family = "Space Grotesk", fontface = "bold", size = 5) +
    
    coord_polar(start = 0, clip = "off") +
    
    scale_y_continuous(limits = c(-max_val * 0.4, max_val * 1.3)) +
    
    labs(
      title = "¿La Expansión Energética?",
      subtitle = glue("Dumbbell Polar del consumo de energía per cápita (kWh) en el G20.<br><b><span style='color: {color_2000};'>El punto verde marca el año 2000</span>;<span style='color: {color_2022};'> el punto cyan, el año 2022.</span></b>"),
      caption = generar_caption_2026("05", "Experimental", "Our World in Data", color_2022, texto_secundario)
    ) +
    
    theme_void(base_size = 16, base_family = "Outfit") +
    theme(
      plot.background = element_rect(fill = fondo_dia, color = NA),
      panel.background = element_rect(fill = fondo_dia, color = NA),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      # Forzamos la alineación manual en theme_void
      plot.title = element_text(color = texto_dia, family = "Space Grotesk", face = "bold", size = rel(2.4), margin = margin(t = 10, b = 10, l = 0)),
      plot.subtitle = element_markdown(color = texto_secundario, margin = margin(b = 30, l = 0), lineheight = 1.2),
      
      plot.caption = element_markdown(color = texto_secundario, size = rel(0.7), hjust = 0, lineheight = 1.6, margin = margin(t = 20, l = 0, b = 40))
    )
    
  return(p)
}


# =============================================================================
# DÍA 06 — Reporters Without Borders (Comparisons)
# =============================================================================

plot_dia06_rsf_editorial <- function(dt) {
  
  # Añadir fuente Lato
  font_add_google("Lato", "lato")
  showtext_opts(dpi = 300)
  setup_fonts_2026()
  showtext_auto()
  
  # Estética de RSF.ORG
  fondo_rsf <- "#FFFFFF" 
  texto_rsf <- "#1B1B1B" 
  texto_secundario <- "#949494" 
  color_referencia <- "#E0E0E0" 
  color_acento_rsf <- "#EA0A38" # Lipstick Red RSF
  
  colores_score <- c(
    "Buena" = "#3E5FFF",           
    "Satisfactoria" = "#ADDAA8",   
    "Problemática" = "#FFFF66",     
    "Difícil" = "#FFA100",         
    "Muy Grave" = "#DE2717"         
  )
  
  p <- ggplot(dt, aes(x = score, y = continente, color = categoria)) +
    
    geom_vline(xintercept = 50, color = color_referencia, linewidth = 1, linetype = "dashed") +
    geom_jitter(height = 0.25, width = 0, size = 3.5, alpha = 0.85) +
    
    # Textos de los países usando Lato Bold
    geom_text(aes(label = etiqueta), vjust = -1.5, color = color_acento_rsf,
              family = "lato", fontface = "bold", size = 4.5, show.legend = FALSE) +
    
    scale_color_manual(values = colores_score) +
    scale_x_continuous(limits = c(0, 100), breaks = seq(0, 100, 20), expand = c(0.02, 0)) +
    
    labs(
      title = "La Geografía de la Censura",
      subtitle = str_wrap("Índice Mundial de Libertad de Prensa. Cada punto representa un país. Las puntuaciones más bajas (0) indican represión absoluta; las más altas (100), libertad total.", 65),
      caption = generar_caption_2026("06", "Reporters Without Borders", "RSF World Press Freedom Index", color_acento_rsf, texto_secundario)
    ) +
    
    # Tema editorial RSF
    theme_minimal(base_size = 16, base_family = "lato") +
    theme(
      plot.background = element_rect(fill = fondo_rsf, color = NA),
      panel.background = element_rect(fill = fondo_rsf, color = NA),
      text = element_text(color = texto_rsf, family = "lato"),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      # Títulos en Lato Bold con el color de RSF
      plot.title = element_text(family = "lato", face = "bold", size = rel(2.2), color = color_acento_rsf, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "lato", size = rel(1.1), color = texto_secundario, margin = margin(b = 40), lineheight = 1.2),
      
      axis.title = element_blank(),
      axis.text.y = element_text(color = texto_rsf, face = "bold", size = rel(1), margin = margin(r = 5)),
      axis.text.x = element_text(color = texto_secundario, size = rel(1.1)),
      
      panel.grid.major.y = element_blank(), 
      panel.grid.minor = element_blank(),
      panel.grid.major.x = element_line(color = color_referencia, linewidth = 0.5, linetype = "dotted"),
      
      legend.position = "top",
      legend.justification.top = "left",
      legend.location = "plot",
      legend.title = element_blank(),
      legend.text = element_text(size = rel(0.8), color = texto_rsf, face = "bold"),
      legend.key.size = unit(0.8, "cm"),
      legend.margin = margin(b = 20),
      
      # Forzamos que ggtext también respete Lato
      plot.caption = element_markdown(family = "lato", size = rel(0.7), color = texto_secundario, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.margin = margin(40, 40, 40, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 07 — Multiscale (Distributions)
# =============================================================================

plot_dia07_multiscale <- function(dt, paleta) {
  
  setup_fonts_2026()
  
  fondo_papel <- unname(paleta["fondo"])
  color_texto <- unname(paleta["pizarra"])
  color_macro <- unname(paleta["magenta"])
  
  # Mapeo de la paleta a las zonas NUTS 1
  colores_nuts1 <- c(
    "TOTAL NACIONAL"      = color_macro,
    "Comunidad de Madrid" = unname(paleta["coral"]),
    "Noreste"             = unname(paleta["pino"]),
    "Este"                = unname(paleta["malva"]),
    "Noroeste"            = unname(paleta["dorado"]),
    "Centro"              = unname(paleta["nude"]),
    "Sur"                 = "#c5aeb1", # Tono extra mezclado para que encaje
    "Canarias"            = "#8f9c99"  # Tono extra mezclado
  )
  
  p <- ggplot(dt, aes(x = renta, y = nuts1, fill = nuts1)) +
    
    geom_density_ridges(scale = 1.7, rel_min_height = 0.005, color = fondo_papel, 
                        linewidth = 0.8, alpha = 0.85) +
    
    scale_fill_manual(values = colores_nuts1) +
    
    scale_x_continuous(labels = scales::label_dollar(prefix = "", suffix = "€", big.mark = ".", decimal.mark = ","),
                       breaks = seq(10000, 30000, by = 5000), 
                       limits = c(7000, 32000), expand = c(0, 0)) +
    
    labs(
      title = "La Fractura Macro-Regional",
      subtitle = str_wrap("Distribución multiescala de la renta neta por persona a nivel NUTS 1 (2023). La perspectiva macro revela profundas brechas territoriales: mientras el Sur, Canarias y el Noroeste presentan distribuciones estrechas que indican gran homogeneidad en rentas medias y bajas, la Comunidad de Madrid destaca por su extrema dispersión y una pronunciada cola hacia las rentas más altas.", 60),
      caption = generar_caption_2026("07", "Multiscale (NUTS 1)", "INE (Atlas de Distribución de Renta)", color_macro, color_texto)
    ) +
    
    theme_minimal(base_size = 16, base_family = "Fira Sans") +
    theme(
      plot.background = element_rect(fill = fondo_papel, color = NA),
      panel.background = element_rect(fill = fondo_papel, color = NA),
      text = element_text(color = color_texto),
      
      plot.title = element_text(family = "Lora", face = "bold", size = rel(2.2), color = color_macro),
      plot.subtitle = element_text(family = "Lora", size = rel(1.05), color = color_texto, margin = margin(b = 40), lineheight = 1.2),
      
      plot.title.position = "plot",

      axis.title = element_blank(),
      axis.text.y = element_text(face = "bold", size = rel(1.1), color = color_texto),
      axis.text.x = element_text(size = rel(0.9), margin = margin(t = 10)),
      
      panel.grid.major.y = element_line(color = "#e6ded1", linewidth = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "none",
      plot.caption = element_markdown(family = "Lora", size = rel(0.7), color = color_macro, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.caption.position = "plot",
      plot.margin = margin(40, 20, 40, 40)
    )
    
    return(p)
}


# =============================================================================
# DÍA 08 — Circular (Distributions)
# =============================================================================

plot_dia08_circular <- function(dt, paleta) {
  
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  
  fondo_papel <- unname(paleta["fondo"])
  color_texto <- unname(paleta["pizarra"])
  
  # Colores para la doble capa
  color_base_total <- "#e6ded1"             
  color_cesareas_normal <- unname(paleta["pino"])  
  color_cesareas_pico <- unname(paleta["magenta"]) 
  
  max_val <- max(dt$total_nacimientos)
  
  p <- ggplot(dt, aes(x = MESPAR)) +
    
    geom_hline(yintercept = seq(0, max_val, length.out = 4), 
               color = color_base_total, linewidth = 0.75, linetype = "dotted") +
    geom_col(aes(y = total_nacimientos), fill = color_base_total, 
             width = 0.9, color = fondo_papel, linewidth = 0.5, alpha = 0.75) +
    geom_col(aes(y = total_cesareas, fill = color_flag), 
             width = 0.9, color = fondo_papel, linewidth = 0.5) +
    
    # Textos
    geom_text(aes(y = total_cesareas + (max_val * 0.2), 
                  label = paste0(round(pct_cesarea * 100, 1), "%"),
                  color = color_flag),
              family = "Fira Sans", fontface = "bold", size = 4.5) +
    
    scale_fill_manual(values = c("Normal" = color_cesareas_normal, "Pico" = color_cesareas_pico)) +
    scale_color_manual(values = c("Normal" = color_texto, "Pico" = color_cesareas_pico)) +
    
    # Configuración de los meses en el anillo exterior
    scale_x_continuous(
      breaks = 1:12,
      labels = dt$mes_nombre,
      limits = c(0.5, 12.5),
      expand = c(0, 0)
    ) +
    
    # El Agujero del Donut
    scale_y_continuous(
      limits = c(-max_val * 0.35, max_val * 1.05),
      expand = c(0, 0)
    ) +
    
    coord_polar(start = -pi/12, clip = "off") +
    
    labs(
      title = "El Calendario de Quirófano",
      subtitle = str_wrap("Distribución mensual de nacimientos en España (2024). El anillo gris exterior representa el volumen total de partos; el anillo interior resalta las intervenciones por cesárea. Analizamos si la estacionalidad médica (festivos, verano) altera la tasa natural de cirugías.", 60),
      caption = generar_caption_2026("08", "Circular (Distributions)", "INE (Microdatos Nacimientos 2024)", color_cesareas_pico, color_texto)
    ) +
    
    theme_minimal(base_size = 16, base_family = "Fira Sans") +
    theme(
      plot.background = element_rect(fill = fondo_papel, color = NA),
      panel.background = element_rect(fill = fondo_papel, color = NA),
      text = element_text(color = color_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Lora", face = "bold", size = rel(2.1), color = color_cesareas_pico, margin = margin(b = 10, l = 0)),
      plot.subtitle = element_text(family = "Lora", size = rel(1), color = color_texto, margin = margin(b = 20, l = 0), lineheight = 1.3),
      
      axis.title = element_blank(),
      axis.text.y = element_blank(),
      axis.text.x = element_text(family = "Fira Sans", size = rel(1.2), color = color_texto, face = "bold"),
      
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "none",
      plot.caption = element_markdown(family = "Fira Sans", size = rel(0.7), color = color_texto, hjust = 0, lineheight = 1.6, margin = margin(t = 20, l = 0)),
      plot.margin = margin(10, 0, 10, 0)
    )
    
  return(p)
}


# =============================================================================
# DÍA 09 — Wealth (Distributions)
# =============================================================================

plot_dia09_wealth <- function(dt, paleta) {
  
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  
  fondo_papel <- unname(paleta["fondo"])
  color_texto <- unname(paleta["pizarra"])
  color_base <- unname(paleta["malva"])       # Verde oscuro sobrio para el "montón"
  color_tech <- unname(paleta["magenta"])    # Magenta vibrante para destacar a la tecnología
  color_boxplot <- unname(paleta["pino"])
  
  # Flag para colorear a los "Tech Bros"
  dt[, color_flag := fifelse(sector == "Technology", "Tech", "Resto")]
  
  p <- ggplot(dt, aes(x = riqueza_b, y = sector, color = color_flag)) +
    
    geom_jitter(height = 0.2, size = 3, alpha = 0.5) +
    geom_boxplot(width = 0.5, fill = "transparent", color = color_boxplot, 
                 outlier.shape = NA, linewidth = 0.9) +
    
    scale_color_manual(values = c("Resto" = color_base, "Tech" = color_tech)) +
    
    # Escala Logarítmica base 10
    scale_x_log10(
      labels = scales::label_dollar(suffix = " B", accuracy = 1),
      breaks = c(1, 3, 10, 30, 100, 200) 
    ) +
    
    labs(
      title = "La Desigualdad de la Élite",
      subtitle = str_wrap("Patrimonio neto (Miles de millones $) de los multimillonarios de Forbes 2024. Al usar una escala logarítmica, la Ley de Potencias se revela: la inmensa mayoría forma una densa 'clase media' alrededor de 2B-3B$, mientras unos pocos titanes (especialmente en Tecnología) estiran la cola hacia la derecha.", 60),
      caption = generar_caption_2026("09", "Wealth (Distributions)", "Forbes World's Billionaires List 2024 (via Kaggle)", color_tech, color_texto)
    ) +
    
    theme_minimal(base_size = 16, base_family = "Fira Sans") +
    theme(
      plot.background = element_rect(fill = fondo_papel, color = NA),
      panel.background = element_rect(fill = fondo_papel, color = NA),
      text = element_text(color = color_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Lora", face = "bold", size = rel(2.1), color = color_tech, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Lora", size = rel(1.05), color = color_texto, margin = margin(b = 40), lineheight = 1.3),
      
      axis.title = element_blank(),
      axis.text.y = element_text(family = "Fira Sans", face = "bold", size = rel(1.05), color = color_texto),
      axis.text.x = element_text(family = "Fira Sans", size = rel(0.9), color = color_texto, margin = margin(t = 5)),
      
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = "#e6ded1", linewidth = 1, linetype = "dotted"),
      panel.grid.minor.x = element_blank(),
      
      legend.position = "none",
      plot.caption = element_markdown(family = "Fira Sans", size = rel(0.7), color = color_texto, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.margin = margin(40, 40, 40, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 10 — Pop Culture (Distributions)
# =============================================================================

plot_dia10_popculture <- function(dt, paleta_base) {
  
  setup_fonts_2026()
  showtext_opts(dpi = 300)

  fondo_papel <- unname(paleta_base["fondo"]) # #faf5ed
  color_texto <- unname(paleta_base["pizarra"]) # #6d7172
  
  # Paleta de colores de 'FAMILY GUY' (portada)
  fg_titulo <- "#0097D7"   # Blue-bell (Letra título)
  
  colores_eras <- c(
    "Fundación y Estabilidad (T1-T3)" = "#2F642F", # Peter (Pantalón)
    "Cénit Creativo (T4-T6)"          = "#FDDD7E", # Stewie (Camiseta)
    "Erosión del Formato (T7-T11)"    = "#6174B8", # Chris (Camiseta)
    "La Era de la Fatiga (T12-T23)"   = "#DE294C"  # Stewie (Pantalón - Alerta)
  )
  
  p <- ggplot(dt, aes(x = averageRating, y = season_factor, fill = era)) +
    
    geom_vline(xintercept = 7.5, color = color_texto, linetype = "dashed", alpha = 0.4, linewidth = 0.8) +
    geom_density_ridges(scale = 2.5, rel_min_height = 0.01, color = fondo_papel, linewidth = 0.6, alpha = 0.85) +
    
    scale_fill_manual(values = colores_eras) +
    scale_x_continuous(breaks = seq(4, 10, by = 1), limits = c(4.5, 9.5)) +
    
    labs(
      title = "La Decadencia de Quahog",
      subtitle = str_wrap("Distribución de notas de 'Family Guy' en IMDb. Las primeras temporadas (Peter/Stewie) mantenían una consistencia férrea por encima del 7.5. A partir de la temporada 9, la campana se desploma, arrastrando la media hacia la mediocridad moderna.", 60),
      caption = generar_caption_2026("10", "Pop Culture (Distributions)", "IMDb Non-Commercial Datasets", fg_titulo, color_texto)
    ) +
    
    guides(fill = guide_legend(nrow = 2, byrow = TRUE)) +
    
    theme_minimal(base_size = 16, base_family = "Fira Sans") +
    theme(
      plot.background = element_rect(fill = fondo_papel, color = NA),
      panel.background = element_rect(fill = fondo_papel, color = NA),
      text = element_text(color = color_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      # Título principal con el azul oficial de la serie
      plot.title = element_text(family = "Lora", face = "bold", size = rel(2.2), color = fg_titulo, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Lora", size = rel(1.1), color = color_texto, margin = margin(b = 30), lineheight = 1.3),
      
      axis.title = element_blank(),
      axis.text.y = element_text(family = "Fira Sans", face = "bold", size = rel(1.1), color = color_texto, vjust = 0),
      axis.text.x = element_text(family = "Fira Sans", size = rel(1.2), color = color_texto, margin = margin(t = 5)),
      
      panel.grid.major.y = element_line(color = "#e6ded1", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "top",
      legend.justification = "left",
      legend.location = "plot",
      legend.title = element_blank(),
      legend.text = element_text(family = "Fira Sans", size = rel(0.8), face = "bold"),
      legend.key.size = unit(0.8, "cm"),
      
      plot.caption = element_markdown(family = "Fira Sans", size = rel(0.7), color = color_texto, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 11 — Physical (Distributions)
# =============================================================================


plot_dia11_physical <- function(dt, paleta) {
  
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  
  fondo_papel <- unname(paleta["fondo"])
  color_texto <- unname(paleta["pizarra"])
  color_acento <- unname(paleta["magenta"])
  
  colores_deporte <- c(
    "Baloncesto"          = unname(paleta["magenta"]),
    "Natación"            = unname(paleta["pino"]),
    "Atletismo (General)" = unname(paleta["pizarra"]),
    "Halterofilia"        = unname(paleta["dorado"]),
    "Gimnasia Artística"  = unname(paleta["coral"])
  )
  
  p <- ggplot(dt, aes(x = Height, y = deporte_es, fill = deporte_es)) +
    
    geom_vline(xintercept = 175, color = color_texto, linetype = "dashed", alpha = 0.5, linewidth = 0.8) +

    geom_density_ridges(scale = 1.8, rel_min_height = 0.01, color = fondo_papel, 
                        linewidth = 0.8, alpha = 0.85) +
    annotate("text", x = 173, y = 5.8, label = "Media Humana (173cm)", 
             family = "Fira Sans", color = color_texto, angle = 90, size = 3.5, fontface = "italic") +
    
    scale_fill_manual(values = colores_deporte) +
    
    # Configuramos el eje X (Altura en cm)
    scale_x_continuous(breaks = seq(150, 220, by = 10), 
                       labels = function(x) paste0(x, " cm"),
                       limits = c(150, 225)) +
    
    labs(
      title = "La Morfología del Éxito",
      subtitle = str_wrap("Distribución de la altura física (masculina) en los Juegos Olímpicos del siglo XXI. El deporte de élite exige biotipos tan extremos que fracturan la distribución normal humana. Los gimnastas necesitan centros de gravedad bajos, mientras que el baloncesto filtra biológicamente a individuos en el percentil 99 de altura.", 60),
      caption = generar_caption_2026("11", "Physical (Distributions)", "Kaggle (120 years of Olympic history)", color_acento, color_texto)
    ) +
    
    theme_minimal(base_size = 16, base_family = "Fira Sans") +
    theme(
      plot.background = element_rect(fill = fondo_papel, color = NA),
      panel.background = element_rect(fill = fondo_papel, color = NA),
      text = element_text(color = color_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Lora", face = "bold", size = rel(2.2), color = color_acento, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Lora", size = rel(1.1), color = color_texto, margin = margin(b = 40), lineheight = 1.3),
      
      axis.title = element_blank(),
      axis.text.y = element_text(family = "Fira Sans", face = "bold", size = rel(1.05), color = color_texto, vjust = 0),
      axis.text.x = element_text(family = "Fira Sans", size = rel(0.95), color = color_texto, margin = margin(t = 5)),
      
      panel.grid.major.y = element_line(color = "#e6ded1", linewidth = 1),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "none",
      
      plot.caption = element_markdown(family = "Fira Sans", size = rel(0.7), color = color_texto, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.margin = margin(40, 40, 40, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 12 — FlowingData (Distributions)
# =============================================================================

plot_dia12_flowingdata <- function(dt) {
  
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  # Paleta FollowingData
  fondo_fd <- "#ffffff"
  texto_titulos <- "#000000"
  texto_cuerpo <- "#333333"
  texto_meta <- "#888888"
  lineas_grid <- "#cccccc"
  acento_rojo <- "#821122" 
  
  colores_estaciones <- c(
    "Invierno"  = "#8AC2BE", 
    "Primavera" = "#7EAD5B", 
    "Verano"    = "#DD8B75", 
    "Otoño"     = "#E7AE6E"  
  )
  
  p <- ggplot(dt, aes(x = tmed, y = mes_factor, fill = estacion)) +
    
    geom_vline(xintercept = 0, color = texto_meta, linetype = "dashed", linewidth = 0.5) +
    geom_vline(xintercept = 25, color = acento_rojo, linetype = "dotted", linewidth = 0.5) +
    
    # Ridgeline
    geom_density_ridges(scale = 2.5, rel_min_height = 0.01, color = fondo_fd, linewidth = 0.8, alpha = 0.9) +
    
    scale_fill_manual(values = colores_estaciones) +
  
    scale_x_continuous(breaks = seq(-5, 35, by = 5), 
                       labels = function(x) paste0(x, "°C")) +
    
    labs(
      title = "La Huella Térmica de Madrid",
      subtitle = str_wrap("Distribución de las temperaturas medias diarias en el Parque del Retiro (2000 - 2026). Al agrupar más de dos décadas de registros, las crestas de verano revelan colas largas y pesadas hacia la derecha, marcando el rastro de las olas de calor que superan recurrentemente los 25°C de media diaria.", 65),
      caption = generar_caption_2026("12", "FlowingData Theme", "AEMET (Estación 3195: Madrid, Retiro)", acento_rojo, texto_meta)
    ) +
    
    # Tema FollowingData
    theme_minimal(base_size = 16) +
    theme(
      plot.background = element_rect(fill = fondo_fd, color = NA),
      panel.background = element_rect(fill = fondo_fd, color = NA),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Montserrat", face = "bold", size = rel(2.2), color = texto_titulos, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Lora", face = "italic", size = rel(1.1), color = texto_cuerpo, margin = margin(b = 30), lineheight = 1.4),
      
      axis.title = element_blank(),
      
      axis.text.y = element_text(family = "Montserrat", face = "bold", size = rel(0.9), color = texto_meta, vjust = 0),
      axis.text.x = element_text(family = "Inconsolata", size = rel(1.2), color = texto_cuerpo, margin = margin(t = 10)),
      
      panel.grid.major.y = element_blank(),
      panel.grid.major.x = element_line(color = lineas_grid, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(family = "Montserrat", size = rel(0.9), color = texto_cuerpo),
      legend.key.size = unit(0.6, "cm"),
      legend.margin = margin(b = 15),
      
      plot.caption = element_markdown(family = "Montserrat", size = rel(0.7), color = texto_meta, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.margin = margin(30, 30, 30, 30)
    )
    
  return(p)
}

# =============================================================================
# DÍA 13 — Ecosystems (Relationships)
# =============================================================================

plot_dia13_ecosystems <- function(graph_data, paleta) {
  
  setup_fonts_cat3()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()


  c_fondo   <- unname(paleta["fondo"])
  c_marino  <- unname(paleta["marino"])
  c_cian    <- unname(paleta["cian"])
  c_alerta  <- unname(paleta["alerta"]) 
  
  # Construcción del gráfico de red en modo arco
  p <- ggraph(graph_data, layout = "linear", sort.by = presas_consumidas) +
    
    # Arcos
    geom_edge_arc(
      aes(color = is_apex_prey, alpha = is_apex_prey, edge_width = is_apex_prey),
      fold = TRUE, strength = 0.4
    ) +
    
    scale_edge_color_manual(values = c("Base" = c_marino, "Alerta" = c_alerta)) +
    scale_edge_alpha_manual(values = c("Base" = 0.15, "Alerta" = 0.85)) +
    scale_edge_width_manual(values = c("Base" = 0.5, "Alerta" = 1.5)) +
    
    # Nodos
    geom_node_point(
      aes(color = es_apex, size = presas_consumidas)
    ) +
    
    scale_color_manual(values = c("Resto del Ecosistema" = c_cian, "Superdepredador" = c_alerta)) +
    scale_size_continuous(range = c(1, 4.5)) + 
    
    # Textos de los nodos
    geom_node_text(
      aes(label = name, color = es_apex),
      angle = 0, hjust = 1, nudge_y = -0.35, 
      family = "IBM Plex Sans", fontface = "bold", size = 3.5
    ) +
    
    # Rotar
    coord_flip() +
    
    scale_y_continuous(expand = expansion(mult = c(0.4, 0.1))) +
    scale_x_continuous(expand = expansion(mult = c(0.05, 0.05))) +
    
    labs(
      title = "El Hambre del Superdepredador",
      subtitle = str_wrap("Red trófica real de la Bahía de Chesapeake (33 taxones, 71 conexiones). Ordenados por la complejidad de su dieta. El rastro en rojo sangre revela las conexiones directas del superdepredador dominante, demostrando visualmente cómo su extinción impactaría en cascada a través de toda la bahía.", 70),
      caption = generar_caption_2026("13", "Ecosystems (Relationships)", "Baird & Ulanowicz (1989) via {igraphdata}", c_alerta, c_marino)
    ) +
    
    theme_minimal(base_size = 16, base_family = "Inter") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_marino),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "IBM Plex Sans", face = "bold", size = rel(2.05), color = c_marino, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Inter", size = rel(0.95), color = c_marino, margin = margin(b = 40), lineheight = 1.3),
      
      # Apagamos todos los elementos de los ejes y el grid porque es una red
      axis.title = element_blank(),
      axis.text = element_blank(),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "none",
      
      plot.caption = element_markdown(family = "Inter", size = rel(0.7), color = c_marino, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.margin = margin(40, 40, 40, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 14 — Trade (Relationships)
# =============================================================================

plot_dia14_trade <- function(dt, paleta) {
  
  setup_fonts_cat3()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  # Paleta
  c_fondo   <- unname(paleta["fondo"])
  c_marino  <- unname(paleta["marino"])
  c_cian    <- unname(paleta["cian"])
  c_naranja <- unname(paleta["naranja"])
  c_alerta  <- unname(paleta["alerta"]) 
  
  # Mapeamos los nombres 
  colores_era <- c(
    "Pre-WWII (1881–1945)"              = c_naranja,
    "Posguerra / Expansión (1946–1999)" = c_marino,
    "Post-GFC / QE (2008–2019)"         = c_cian,
    "COVID & Policrisis (2020–)"        = c_alerta
  )
  
  # Hitos
  dt_labels <- dt[
    mes_ano %in% as.Date(c("1929-09-01", "1999-12-01", "2021-11-01", "2026-03-01"))
  ]
  
  dt_labels[, etiqueta := fcase(
    year(mes_ano) == 1929, "Crash de 1929",
    year(mes_ano) == 1999, "Pico Dot-Com\n(Dic 1999)",
    year(mes_ano) == 2021, "Euforia COVID\n(Nov 2021)",
    year(mes_ano) == 2026, "Tensión Irán\n(Mar 2026)"
  )]
  
  p <- ggplot(dt, aes(x = yield_10y, y = cape)) +
    
    # Puntos por Era Macroeconómica
    geom_point(aes(fill = era_macro), shape = 21, color = "white", 
               size = 3.5, stroke = 0.5, alpha = 0.65) +
    
        # La "Curva de Gravedad" no lineal (loess) calculada sobre 145 años de historia
    geom_smooth(method = "glm",formula = "y ~ x", color = "black", 
                linetype = "dashed", se = FALSE, linewidth = 1.2) +
    
    # Etiquetas Repelidas para los hitos
    geom_text_repel(
      data = dt_labels,
      aes(label = etiqueta, x = yield_10y, y = cape),
      family = "IBM Plex Sans", color = "black", fontface = "bold", size = 4.5,
      # Forzamos los nudges manuales que calculamos arriba
      nudge_x = dt_labels$nudge_x, nudge_y = dt_labels$nudge_y,
      # Aumentamos agresivamente el padding para dispersarlas
      box.padding = 1.8, point.padding = 0.8, force = 5,
      # Estilo de la línea de conexión
      segment.color = "black", segment.alpha = 0.6,
      bg.color = "white", bg.r = 0.15
    ) +
    
    scale_fill_manual(values = colores_era) +
    
    scale_x_continuous(
      labels = function(x) paste0(x, "%"),
      breaks = seq(0, 16, by = 2) # Ampliamos hasta el 16% por los años 80
    ) +
    
    scale_y_continuous(
      breaks = seq(0, 50, by = 10)
    ) +
    
    labs(
      title = "El 'Macro Trade':\nUn Siglo de Gravedad Financiera",
      subtitle = str_wrap("Relación entre la rentabilidad del Bono a 10 Años y el ratio CAPE de Shiller (1881-2026). La curva punteada revela la física del mercado: los tipos actúan como un ancla gravitacional sobre los múltiplos. La era COVID y la actual policrisis (rojo) sitúan las valoraciones en cotas solo vistas en 1929 y 1999, desafiando la media histórica en un entorno de restricción monetaria.", 80),
      caption = generar_caption_2026("14", "Trade (Relationships)", "Robert Shiller Data (Yale University)", c_alerta, c_marino),
      x = "Tasa Libre de Riesgo (Rendimiento Bono 10 Años EE.UU.)",
      y = "Múltiplo de Valoración (CAPE Ratio de Shiller)"
    ) +
    
    theme_minimal(base_size = 16, base_family = "Inter") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_marino),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "IBM Plex Sans", face = "bold", size = rel(1.8), color = c_marino, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Inter", size = rel(0.8), color = "#4a5b6e", margin = margin(b = 30), lineheight = 1.3),
      
      axis.title.x = element_text(family = "IBM Plex Sans", size = rel(0.85), face = "bold", margin = margin(t = 15)),
      axis.title.y = element_text(family = "IBM Plex Sans", size = rel(0.85), face = "bold", margin = margin(r = 15)),
      
      axis.text = element_text(family = "IBM Plex Sans", size = rel(0.95), color = c_marino),
      
      # Grid elegante estilo informe JPM
      panel.grid.major.y = element_line(color = "#d1d5e0", linewidth = 0.5),
      panel.grid.major.x = element_line(color = "#d1d5e0", linewidth = 0.5, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      
      legend.position = "top",
      legend.justification = "center",
      legend.title = element_blank(),
      legend.text = element_text(family = "IBM Plex Sans", face = "bold", size = rel(0.9)),
      legend.key.size = unit(0.4, "cm"),
      legend.margin = margin(b = 20),
      
      plot.caption = element_markdown(family = "Inter", size = rel(0.75), color = c_marino, hjust = 0, lineheight = 1.6, margin = margin(t = 30)),
      plot.margin = margin(30, 40, 30, 40)
    )
    
  p <- p + guides(fill = guide_legend(nrow = 2, byrow = TRUE))
  
  return(p)
}


# =============================================================================
# DÍA 15 — Correlation (Relationships)
# =============================================================================

plot_dia15_correlation <- function(dt, paleta) {
  
  setup_fonts_cat3()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  # Extraemos tu paleta
  c_fondo   <- unname(paleta["fondo"])
  c_marino  <- unname(paleta["marino"])
  c_cian    <- unname(paleta["cian"])
  c_alerta  <- unname(paleta["alerta"]) 
  
  p <- ggplot(dt, aes(x = activo_2, y = activo_1, fill = correlacion)) +
    
    # Heatmap
    geom_tile(color = c_fondo, linewidth = 1.5) +
    geom_text(
      aes(
        label = sprintf("%.2f", correlacion),
        color = abs(correlacion) > 0.4
      ),
      family = "IBM Plex Sans", fontface = "bold", size = 4
    ) +
    
    # Escalas color
    scale_fill_gradient2(
      low = c_alerta,      # Negativa = Rojo
      mid = c_fondo,       # Cero = Color de fondo del lienzo
      high = c_cian,       # Positiva = Cian
      midpoint = 0,
      limits = c(-1, 1),
      breaks = c(-1, -0.5, 0, 0.5, 1),
      guide = guide_colorbar(
        title = "← Opuestos | Se mueven juntos →",
        title.position = "top",
        barwidth = unit(12, "cm"),
        barheight = unit(0.5, "cm")
      )
    ) +
    
    scale_color_manual(values = c("TRUE" = c_fondo, "FALSE" = c_marino), guide = "none") +
    
    # Fortmat
labs(
      title = "La Muerte de la Cartera 60/40",
      subtitle = str_wrap("Correlación diaria en el último año. La policrisis aniquila el 60/40: bonos y bolsa ya no se compensan, caen a la par (+0.14). Mientras Bitcoin (+0.44) se comporta como un activo de riesgo puro, el Oro (0.00) resiste como el único refugio verdaderamente descorrelacionado.", 65),
      caption = generar_caption_2026("15", "Correlation (Relationships)", "Yahoo Finance vía {PerformanceAnalytics}", c_alerta, c_marino),
      x = NULL, y = NULL
    ) +
    
    theme_minimal(base_size = 16, base_family = "Inter") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_marino),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "IBM Plex Sans", face = "bold", size = rel(1.8), color = c_marino, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Inter", size = rel(0.8), color = "#4a5b6e", margin = margin(b = 30), lineheight = 1.3),
      
      # Estilizamos los ejes de la matriz
      axis.text.x = element_text(family = "IBM Plex Sans", face = "bold", size = rel(0.9), color = c_marino, angle = 45, hjust = 1),
      axis.text.y = element_text(family = "IBM Plex Sans", face = "bold", size = rel(0.9), color = c_marino),
      
      # Eliminamos la cuadrícula porque geom_tile ya hace de matriz
      panel.grid = element_blank(),
      
      legend.position = "top",
      legend.justification = "left",
      legend.location = "plot",
      legend.title = element_text(family = "IBM Plex Sans", size = rel(0.75), face = "italic", hjust = 0.5),
      legend.text = element_text(family = "IBM Plex Sans", face = "bold", size = rel(0.7)),
      legend.margin = margin(b = 10),
      
      plot.caption = element_markdown(family = "Inter", size = rel(0.75), color = c_marino, hjust = 0, lineheight = 1.6, margin = margin(t = 40)),
      plot.margin = margin(t = 40, r = 20, b = 40, l =  0)
    )
    
  # Forzamos que los azulejos sean cuadrados perfectos
  p <- p + coord_fixed(ratio = 8/10)
  
  return(p)
}


# =============================================================================
# DÍA 16 — Causation (Relationships)
# =============================================================================

plot_dia16_causation <- function(dt, paleta) {
  
  setup_fonts_cat3()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  c_fondo   <- unname(paleta["fondo"])
  c_marino  <- unname(paleta["marino"])
  c_cian    <- unname(paleta["cian"])
  c_naranja <- unname(paleta["naranja"])
  c_alerta  <- unname(paleta["alerta"]) 
  
  colores_ciclo <- c(
    "Burbuja Dot-Com" = c_naranja,
    "Gran Crisis Financiera" = c_marino,
    "Pánico COVID-19" = c_cian,
    "Policrisis (Ciclo Actual)" = c_alerta
  )
  
  # Extraemos solo los puntos finales de cada línea para poner las etiquetas
  dt_labels <- dt[, .SD[.N], by = ciclo]
  
  p <- ggplot(dt, aes(x = dia_relativo, y = precio_norm, color = ciclo)) +
    
    geom_vline(xintercept = 0, color = c_marino, linetype = "dotted", linewidth = 1) +
    annotate(
      "text", x = 5, y = 125, label = "La FED ejecuta\nel primer recorte\n(T = 0)",
      family = "IBM Plex Sans", fontface = "italic", color = c_marino, size = 3.5, hjust = 0
    ) +
    
    # Línea base de 100 (Break-even)
    geom_hline(yintercept = 100, color = c_marino, linetype = "solid", linewidth = 0.3, alpha = 0.5) +
    
    # Las trayectorias
    geom_line(linewidth = 1.2, alpha = 0.85) +
  
    geom_text_repel(
      data = dt_labels,
      aes(label = ciclo),
      family = "IBM Plex Sans", fontface = "bold", size = 4,
      nudge_x = 10, direction = "y", hjust = 0,
      segment.color = NA # Sin línea de conexión, pegado al final del trazo
    ) +
    
    scale_color_manual(values = colores_ciclo, guide = "none") +
    
    scale_x_continuous(
      breaks = seq(-50, 250, by = 50),
      expand = expansion(mult = c(0, 0.3)) # Espacio a la derecha para las etiquetas
    ) +
    
    scale_y_continuous(
      breaks = seq(40, 140, by = 10),
      labels = function(x) paste0(x, " pts")
    ) +
    
    labs(
      title = "Inferencia Causal: La Trampa y el Rebote",
      subtitle = str_wrap("Estudio de eventos: Impacto del primer recorte de tipos de la FED. La historia revela dos regímenes. En 2000 y 2008 (naranja/marino), el 'Pivot' precedió a mercados bajistas prolongados. Sin embargo, las crisis modernas muestran una resiliencia antinatural: en el ciclo actual (rojo), tras una euforia inicial y una dura capitulación hacia los 88 puntos en el día 138, el mercado protagonizó una violenta recuperación en V (+18%), emulando la inyección del COVID y desafiando la gravedad macroeconómica.", 80),
      caption = generar_caption_2026("16", "Causation (Relationships)", "Análisis propio sobre datos Yahoo Finance", c_alerta, c_marino),
      x = "Días de cotización desde la intervención de la FED (El Evento)",
      y = "S&P 500 Normalizado (Día 0 = 100)"
    ) +
    
    theme_minimal(base_size = 16, base_family = "Inter") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_marino),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "IBM Plex Sans", face = "bold", size = rel(1.6), color = c_marino, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Inter", size = rel(0.8), color = "#4a5b6e", margin = margin(b = 30), lineheight = 1.3),
      
      axis.title.x = element_text(family = "IBM Plex Sans", size = rel(0.95), face = "bold", margin = margin(t = 15)),
      axis.title.y = element_text(family = "IBM Plex Sans", size = rel(0.95), face = "bold", margin = margin(r = 15)),
      
      axis.text = element_text(family = "IBM Plex Sans", size = rel(0.9), color = c_marino),
      
      panel.grid.major = element_line(color = "#d1d5e0", linewidth = 0.5),
      panel.grid.minor = element_blank(),
      
      plot.caption = element_markdown(family = "Inter", size = rel(0.75), color = c_marino, hjust = 0, lineheight = 1.6, margin = margin(t = 30)),
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 17 — Remake (Relationships)
# =============================================================================

plot_dia17_remake <- function(dt, paleta) {
  
  setup_fonts_cat3()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  c_fondo   <- unname(paleta["fondo"])
  c_marino  <- unname(paleta["marino"])
  c_alerta  <- unname(paleta["alerta"]) 
  c_cian    <- unname(paleta["cian"])
  
  # Calculamos la verdadera R2 cíclica
  modelo <- lm(ciclo_sp ~ ciclo_m2, data = dt)
  r_squared <- summary(modelo)$r.squared
  
  p <- ggplot(dt, aes(x = ciclo_m2, y = ciclo_sp)) +
    
    geom_hline(yintercept = 0, color = c_marino, linetype = "solid", linewidth = 0.5, alpha = 0.2) +
    geom_vline(xintercept = 0, color = c_marino, linetype = "solid", linewidth = 0.5, alpha = 0.2) +
    
    geom_smooth(method = "lm", color = c_alerta, linetype = "dashed", se = FALSE, linewidth = 1.2) +
    
    geom_point(shape = 21, fill = c_cian, color = c_fondo, size = 3.5, stroke = 0.6, alpha = 0.85) +
    
    annotate(
      "label", 
      x = min(dt$ciclo_m2), 
      y = max(dt$ciclo_sp), 
      label = sprintf("R² = %.3f\nEl ciclo del M2 no explica a la Bolsa", r_squared),
      family = "IBM Plex Sans", fontface = "bold", color = c_alerta, size = 4.5, 
      hjust = 0, vjust = 1, fill = c_fondo, label.size = NA
    ) +
    
    scale_x_continuous(labels = scales::percent_format(accuracy = 1)) +
    scale_y_continuous(labels = scales::percent_format(accuracy = 1)) +
    
    labs(
      title = "El Fraude del Doble Eje Y (Remake)",
      subtitle = str_wrap("Apollo Academy sugiere una correlación casi perfecta entre la Masa Monetaria y el S&P 500 alineando a la fuerza dos ejes con tendencia infinita. Al aplicar el algoritmo MBH {MacroFilters} para extirpar la inercia temporal y aislar los ciclos reales, la ilusión se desploma. La dispersión demuestra empíricamente que la impresión de dinero (M2) no dicta las fluctuaciones del mercado.", 70),
      caption = generar_caption_2026("17", "Remake (Relationships)", "FRED & Yahoo. Filtro: {MacroFilters} MBH", c_alerta, c_marino),
      x = "Ciclo de Masa Monetaria M2\n(Desviación % respecto a su tendencia base)",
      y = "Ciclo del S&P 500\n(Desviación % respecto a su tendencia base)"
    ) +
    
    theme_minimal(base_size = 16, base_family = "Inter") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_marino),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "IBM Plex Sans", face = "bold", size = rel(1.8), color = c_marino, margin = margin(b = 10)),
      plot.subtitle = element_text(family = "Inter", size = rel(0.9), color = "#4a5b6e", margin = margin(b = 30), lineheight = 1.3),
      
      axis.title.x = element_text(family = "IBM Plex Sans", size = rel(0.8), face = "bold", margin = margin(t = 15)),
      axis.title.y = element_text(family = "IBM Plex Sans", size = rel(0.8), face = "bold", margin = margin(r = 15)),
      
      axis.text = element_text(family = "IBM Plex Sans", size = rel(1.1), color = c_marino),
      
      panel.grid.major = element_line(color = "#d1d5e0", linewidth = 0.5, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      
      plot.caption = element_markdown(family = "Inter", size = rel(0.75), color = c_marino, hjust = 0, lineheight = 1.6, margin = margin(t = 30)),
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 18 — UNICEF (Relationships)
# =============================================================================

plot_dia18_unicef <- function(dt, paleta) {
  
  setup_fonts_cat3()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  c_fondo   <- unname(paleta["fondo"])
  c_marino  <- unname(paleta["marino"])
  c_alerta  <- unname(paleta["alerta"])   # Severa
  c_naranja <- unname(paleta["naranja"])  # Moderada
  
  p <- ggplot(dt, aes(y = region_es)) +
    
    # La brecha entre ambos estados
    geom_segment(
      aes(x = moderate, xend = severe, y = region_es, yend = region_es),
      color = "#a8b2c1", linewidth = 1.5
    ) +
    
    # Pobreza Severa vs Moderada
    geom_point(aes(x = moderate), color = c_naranja, size = 5) +
    geom_point(aes(x = severe), color = c_alerta, size = 5) +
    
    # Labels
    geom_text(
      aes(x = moderate, label = paste0(moderate, "%")),
      color = c_naranja, family = "IBM Plex Sans", fontface = "bold", 
      vjust = -1.5, size = 4
    ) +
    geom_text(
      aes(x = severe, label = paste0(severe, "%")),
      color = c_alerta, family = "IBM Plex Sans", fontface = "bold", 
      vjust = -1.5, size = 4
    ) +
    geom_text(
      aes(x = max(moderate) + 5, label = sprintf("Total:\n%d%%", total_poverty)),
      color = c_marino, family = "IBM Plex Sans", fontface = "italic", 
      size = 3.5, hjust = 0, lineheight = 0.9
    ) +
    
    scale_x_continuous(
      limits = c(0, max(dt$moderate) + 12),
      breaks = seq(0, 60, by = 10),
      labels = function(x) paste0(x, "%")
    ) +
    
    labs(
      title = "La Anatomía del Hambre Infantil",
      subtitle = "Porcentaje de niños (6-23 meses) bajo pobreza alimentaria. El gráfico revela la relación estructural entre la carencia <b style='color:#e69f00'>Moderada (3-4 grupos de alimentos)</b> y la privación <b style='color:#e3111f'>Severa (0-2 grupos)</b>. En el Sur de Asia, la crisis no es solo de volumen total (77%), sino de la extrema gravedad de la misma, con una tasa severa que engulle a casi el 40% de la población infantil.",
      caption = generar_caption_2026("18", "UNICEF Data Day (Relationships)", "UNICEF Global Databases: Child Food Poverty 2024", c_alerta, c_marino),
      x = "Prevalencia en la población infantil (%)",
      y = NULL
    ) +
    
    theme_minimal(base_size = 16, base_family = "Inter") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_marino),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "IBM Plex Sans", face = "bold", size = rel(1.8), color = c_marino, margin = margin(b = 10)),
      
      plot.subtitle = element_textbox_simple(family = "Inter", size = rel(0.95), color = "#4a5b6e", margin = margin(b = 30), lineheight = 1.3),
      axis.title.x = element_text(family = "IBM Plex Sans", size = rel(0.85), face = "bold", margin = margin(t = 15)),
      
      axis.text.y = element_text(family = "IBM Plex Sans", face = "bold", size = rel(1.1), color = c_marino, hjust = 1),
      axis.text.x = element_text(family = "IBM Plex Sans", size = rel(0.9), color = c_marino),
      
      panel.grid.major.y = element_blank(), # Quitamos las líneas horizontales para limpiar el palo de la pesa
      panel.grid.major.x = element_line(color = "#d1d5e0", linewidth = 0.5, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      
      plot.caption = element_markdown(family = "Inter", size = rel(0.75), color = c_marino, hjust = 0, lineheight = 1.6, margin = margin(t = 30)),
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 19 — Evolution (Timeseries)
# =============================================================================

plot_dia19_evolution <- function(dt, paleta) {
  
  setup_fonts_cat4()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  c_fondo   <- unname(paleta["light"])
  c_texto   <- unname(paleta["dark"])
  c_inver   <- unname(paleta["danger"]) # Magenta radiactivo
  c_sano    <- unname(paleta["info"])   # Turquesa
  
  p <- ggplot(dt, aes(x = meses, y = as.factor(ano), height = yield, fill = estado)) +
    
    # Ridgeline
    geom_ridgeline(
      stat = "identity", 
      scale = 0.4,         # Controla cuánto se superponen las montañas
      alpha = 0.85, 
      color = c_texto,     
      linewidth = 0.6
    ) +
    
    scale_fill_manual(
      values = c("Invertida (Recesión inminente)" = c_inver, "Normal (Expansión)" = c_sano)
    ) +
    scale_x_continuous(
      trans = "log10",
      breaks = c(1, 3, 6, 12, 24, 60, 120, 360),
      labels = c("1M", "3M", "6M", "1Y", "2Y", "5Y", "10Y", "30Y"),
      expand = c(0, 0)
    ) +
    
    # Invertir el eje Y
    scale_y_discrete(limits = rev, expand = expansion(mult = c(0.01, 0.05))) +
    
    labs(
      title = "El Reloj de la Recesión",
      subtitle = "Evolución histórica de la Curva de Tipos de EE.UU. (2000-2026). La anatomía de una crisis se hace visible cuando el frente de la curva (tipos a corto plazo) se eleva por encima del largo plazo. Las olas magentas revelan la inversión de la curva previa a la crisis de las Punto-Com, la Gran Recesión de 2008 y la agresiva constricción monetaria de la era post-COVID.",
      caption = generar_caption_2026("19", "Evolution (Timeseries)", "FRED Data vía {quantmod}", c_inver, c_texto),      x = "Vencimiento del Bono",
      y = NULL
    ) +
    
    theme_minimal(base_size = 14, base_family = "Pridi") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      # Tipografía editorial contundente
      plot.title = element_text(family = "Pridi", face = "bold", size = rel(2.5), color = c_texto, margin = margin(b = 20)),
      plot.subtitle = element_textbox_simple(family = "Pridi", size = rel(1.1), color = "#3b4140", margin = margin(b = 30), lineheight = 1.4),
      
      axis.title.x = element_text(family = "Pridi", size = rel(1.0), face = "bold", margin = margin(t = 15)),
      
      axis.text.y = element_text(family = "Pridi", face = "bold", size = rel(1.3), color = c_texto, vjust = 0),
      axis.text.x = element_text(family = "Pridi", face = "bold", size = rel(1.0), color = c_texto),
      
      panel.grid.major.y = element_line(color = "#b0b8b6", linewidth = 0.5, linetype = "dotted"),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      legend.position = "top",
      legend.justification = "left",
      legend.title = element_blank(),
      legend.text = element_text(family = "Pridi", face = "bold", size = rel(1.1)),
      legend.key.size = unit(0.5, "cm"),
      legend.margin = margin(b = 15),
      
      plot.caption = element_markdown(family = "Roboto Condensed", size = rel(0.85), color = c_texto, hjust = 0, lineheight = 1.6, margin = margin(t = 20)),      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 20 — Global Change (Timeseries)
# =============================================================================

plot_dia20_global_change <- function(dt, paleta) {
  
  setup_fonts_cat4()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  c_fondo   <- unname(paleta["light"])
  c_texto   <- unname(paleta["dark"])
  c_frio    <- unname(paleta["info"])     # Azul Cian
  c_calor   <- unname(paleta["danger"])   # Magenta 
  
  p <- ggplot(dt, aes(x = mes, y = Year, fill = anomalia)) +
    
    geom_tile(color = c_fondo, linewidth = 0.1) +
    
    scale_fill_gradient2(
      low = c_frio, 
      mid = c_fondo, 
      high = c_calor, 
      midpoint = 0,
      limits = c(-1.5, 1.5), 
      oob = scales::squish,
      guide = guide_colorbar(
        title = "Anomalía Térmica (°C)",
        title.position = "top",
        barwidth = unit(12, "cm"),
        barheight = unit(0.4, "cm")
      )
    ) +
    
    scale_y_continuous(
      trans = "reverse", # El tiempo cae hacia abajo
      breaks = seq(1880, 2030, by = 10),
      expand = c(0, 0)
    ) +
    
    scale_x_discrete(expand = c(0, 0), position = "top") + # Meses arriba
    
    labs(
      title = "El Tapiz del Calentamiento Global",
      subtitle = "Anomalía de la temperatura superficial terrestre y oceánica (1880-Presente) respecto a la media base. La matriz térmica revela un cambio de régimen absoluto: el siglo XIX y principios del XX (dominados por tonos cian) han sido devorados por un avance implacable del magenta extremo en las últimas dos décadas. Las estaciones ya no importan; el exceso térmico es sistémico.",
      caption = generar_caption_2026("20", "Global Change (Timeseries)", "NASA Goddard Institute for Space Studies (GISTEMP v4)", c_calor, c_texto),
      x = NULL,
      y = NULL
    ) +
    
    theme_minimal(base_size = 14, base_family = "Pridi") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Pridi", face = "bold", size = rel(2.1), color = c_texto, margin = margin(b = 20)),
      plot.subtitle = element_textbox_simple(family = "Pridi", size = rel(1.1), color = "#3b4140", margin = margin(b = 20), lineheight = 1.4),
      axis.text.y = element_text(family = "Pridi", face = "bold", size = rel(1.2), color = c_texto),
      axis.text.x = element_text(family = "Pridi", face = "bold", size = rel(1.1), color = c_texto, margin = margin(b = 10)),
      
      # Eliminamos grid porque los tiles hacen su propio grid
      panel.grid = element_blank(),
      
      legend.position = "bottom",
      legend.justification = "center",
      legend.title = element_text(family = "Pridi", face = "bold", size = rel(0.9)),
      legend.text = element_text(family = "Pridi", size = rel(0.9)),
      legend.margin = margin(t = 20, b = 0),
      
      plot.caption = element_markdown(family = "Roboto Condensed", size = rel(0.85), color = c_texto, hjust = 0, lineheight = 1.6, margin = margin(t = 30)),
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 21 — Historical (Timeseries)
# =============================================================================

plot_dia21_historical <- function(data_list, paleta) {
  
  setup_fonts_cat4()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  dt_long <- data_list$long
  dt_wide <- data_list$wide
  
  c_fondo   <- unname(paleta["light"])
  c_texto   <- unname(paleta["dark"])
  
  # Asignación de colores
  colores_grupos <- c(
    "1 a 2 SMI" = unname(paleta["danger"]),  # Magenta (El bloque que absorbe todo)
    "2 a 3 SMI" = unname(paleta["info"]),    # Turquesa (La clase media que se hunde)
    "Más de 3 SMI" = unname(paleta["accent2"]) # Verde (Rentas altas)
  )
  
  max_anio <- max(dt_long$Periodo)
  
  p <- ggplot() +
    geom_ribbon(
      data = dt_wide,
      aes(x = Periodo, ymin = pmin(smi_1_2, smi_2_3), ymax = pmax(smi_1_2, smi_2_3)),
      fill = c_texto, alpha = 0.06
    ) +
    
    geom_line(
      data = dt_long,
      aes(x = Periodo, y = porcentaje, color = grupo),
      linewidth = 1.8
    ) +
    
    # Puntos al inicio (2008) y al final (2023)
    geom_point(data = dt_long[Periodo %in% c(2008, max_anio)], aes(x = Periodo, y = porcentaje, color = grupo), size = 3) +
    
    # Etiquetas directas en 2023
    geom_text(
      data = dt_long[Periodo == max_anio],
      aes(x = Periodo + 0.3, y = porcentaje, label = sprintf("%s\n(%.1f%%)", grupo, porcentaje), color = grupo),
      family = "Pridi", fontface = "bold", size = 5, hjust = 0, lineheight = 0.9
    ) +
    
    # Anotación contextual
    annotate("text", x = 2008, y = 38, label = "Empate\nClase Media (2008)", family = "Pridi", color = "#3b4140", size = 5, hjust = 0, fontface = "italic") +
    geom_segment(aes(x = 2008.5, xend = 2008.5, y = 35.5, yend = 33.5), color = "#3b4140", arrow = arrow(length = unit(0.2, "cm"))) +
    
    # Hito temporal
    geom_vline(xintercept = 2019, color = "#3b4140", linetype = "dotted", linewidth = 0.8) +
    annotate("text", x = 2019.2, y = 15, label = "SMI\n+22%", family = "Pridi", color = "#3b4140", size = 5, hjust = 0) +
    
    scale_color_manual(values = colores_grupos, guide = "none") +
    
    scale_x_continuous(
      breaks = seq(2008, max_anio, by = 2),
      expand = expansion(mult = c(0.02, 0.25)) 
    ) +
    
    scale_y_continuous(
      breaks = seq(0, 60, by = 10),
      labels = function(x) paste0(x, "%")
    ) +
    
    labs(
      title = "El 'Efecto Embudo' del SMI",
      subtitle = "Distribución de trabajadores a jornada completa según múltiplos del Salario Mínimo (2008-2023). La 'Recuperación en K' queda al descubierto: mientras en 2008 la clase trabajadora y la clase media (2 a 3 SMI) estaban equilibradas, las fuertes subidas del SMI desde 2019 sin acompañamiento de los salarios medios han provocado una brutal compresión. Hoy, más de la mitad del país (<b style='color:#fb036c'>54%</b>) está acorralado en la franja baja.",
      caption = generar_caption_2026("21", "Historical (Timeseries)", "INE", unname(paleta["danger"]), c_texto),
      x = NULL,
      y = "% del Total de Trabajadores"
    ) +
    
    theme_minimal(base_size = 14, base_family = "Pridi") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Pridi", face = "bold", size = rel(2.4), color = c_texto, margin = margin(b = 15)),
      plot.subtitle = element_textbox_simple(family = "Pridi", size = rel(1.1), color = "#3b4140", margin = margin(b = 30), lineheight = 1.4),

      axis.title.y = element_text(family = "Pridi", size = rel(1.0), face = "bold", margin = margin(r = 15)),
      
      axis.text.y = element_text(family = "Pridi", face = "bold", size = rel(1.2), color = c_texto),
      axis.text.x = element_text(family = "Pridi", face = "bold", size = rel(1.1), color = c_texto, margin = margin(t = 5)),
      
      panel.grid.major.y = element_line(color = "#b0b8b6", linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      plot.caption = element_markdown(family = "Roboto Condensed", size = rel(0.85), color = c_texto, hjust = 0, lineheight = 1.6, margin = margin(t = 30)),
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 22 — New Tool (Timeseries)
# =============================================================================

plot_dia22_new_tool <- function(nube_obj, paleta) {
  
  setup_fonts_cat4()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  c_fondo   <- unname(paleta["light"])
  c_texto   <- unname(paleta["dark"])
  c_nube_up <- unname(paleta["info"])    # Turquesa 
  c_nube_dn <- unname(paleta["danger"])  # Magenta 
  
  # EL VECTOR MAESTRO (Basado en el código fuente de ichimoku)
  colores_ichimoku <- c(
    "#d1d8d7",               # pal[1]: Relleno de la Nube (Gris azulado muy suave para que las líneas destaquen)
    c_nube_dn,                 # pal[2]: Senkou B (Línea de soporte/resistencia lenta)
    c_nube_up,                 # pal[3]: Senkou A (Línea rápida)
    "#82908f",               # pal[4]: Tenkan-sen (Gris claro neutro)
    "#4a5b6e",               # pal[5]: Kijun-sen (Azul oscuro base)
    unname(paleta["accent2"]), # pal[6]: Chikou Span (Verde oscuro confirmación)
    c_nube_up,                 # pal[7]: Borde Vela UP (Turquesa)
    c_nube_dn,                 # pal[8]: Borde Vela DOWN (Magenta)
    c_texto,                   # pal[9]: Borde Vela FLAT
    c_fondo,                   # pal[10]: Relleno Vela UP (Usamos c_fondo para hacer velas alcistas "huecas" clásicas)
    c_nube_dn,                 # pal[11]: Relleno Vela DOWN (Magenta sólido)
    c_texto                    # pal[12]: Relleno Vela FLAT
  )

  p <- ichimoku::autoplot(
    nube_obj, 
    theme = colores_ichimoku, # Aplicamos el vector personalizado
    type = "none"
  ) +
    
    labs(
      title = "El Equilibrio de un Vistazo",
      subtitle = "Análisis del **Nikkei 225** mediante *Ichimoku Kinko Hyo*. Empleando el paquete **{ichimoku}** como herramienta no convencional, el gráfico proyecta las nubes dinámicas (Kumo) hacia el futuro. Observamos cómo el índice encuentra un soporte matemático perfecto en los bordes de la nube durante sus correcciones, impulsando su ruptura hacia máximos históricos.",
      caption = generar_caption_2026("22", "New Tool ({ichimoku})", "Yahoo Finance (^N225)", c_nube_up, c_texto),
      x = NULL,
      y = "Precio (JPY)"
    ) +
    
    # NUESTRO TEMA EDITORIAL (Aplastará el theme_ichimoku_light por defecto)
    theme_minimal(base_size = 14, base_family = "Pridi") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(family = "Pridi", face = "bold", size = rel(2.2), color = c_texto, margin = margin(b = 20)),
      plot.subtitle = element_textbox_simple(family = "Pridi", size = rel(1.1), color = "#3b4140", margin = margin(b = 20), lineheight = 1.4),

      axis.title.y = element_text(family = "Pridi", size = rel(1.0), face = "bold", margin = margin(r = 15)),
      axis.text.y = element_text(family = "Pridi", face = "bold", size = rel(1.1), color = c_texto),
      
      # Rotamos 45 grados para que las fechas no colisionen en el eje X
      axis.text.x = element_text(family = "Pridi", face = "bold", size = rel(1.1), color = c_texto, angle = 45, hjust = 1, margin = margin(t = 10)),
      
      panel.grid.major = element_line(color = "#b0b8b6", linewidth = 0.5, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      
      legend.position = "none",
      
      plot.caption = element_markdown(family = "Roboto Condensed", size = rel(0.85), color = c_texto, hjust = 0, lineheight = 1.6, margin = margin(t = 30)),
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 23 — Seasons (Timeseries)
# =============================================================================

plot_dia23_seasons <- function(dt, paleta) {
  
  setup_fonts_cat4()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  c_fondo   <- unname(paleta["light"])
  c_texto   <- unname(paleta["dark"])
  
  c_base    <- "#3b4140"                # Gris para el histórico
  c_humedo  <- unname(paleta["info"])     # Turquesa (Agua/Lluvia)
  c_seco    <- unname(paleta["danger"])   # Magenta (Alarma por sequía)
  c_actual  <- unname(paleta["accent2"])  # Verde para el año en curso
  
  # Extraemos los nombres exactos de los niveles calculados dinámicamente
  niveles <- levels(dt$highlight)
  n_historico <- niveles[1]
  n_actual    <- niveles[2]
  n_seco      <- niveles[3]
  n_humedo    <- niveles[4]
  
  p <- ggplot(dt, aes(x = fecha_ficticia, y = prec_acumulada, group = anio, color = highlight, linewidth = highlight, alpha = highlight)) +
    
    geom_line() +
    
    scale_color_manual(values = setNames(c(c_base, c_actual, c_seco, c_humedo), niveles)) +
    scale_linewidth_manual(values = setNames(c(0.4, 1.5, 1.5, 1.5), niveles)) +
    scale_alpha_manual(values = setNames(c(0.3, 1, 1, 1), niveles)) +
    
    # Formateo estacional del eje X
    scale_x_date(
      date_labels = "%b", 
      date_breaks = "1 month", 
      expand = c(0.05, 0.15), 
      limits = c(min(dt$fecha_ficticia), max(dt$fecha_ficticia))
    ) +
    
    scale_x_date(
      breaks = seq(as.Date("2024-01-01"), as.Date("2024-12-01"), by = "1 month"),
      date_labels = "%b", 
      expand = expansion(mult = c(0.01, 0.05)) # Mantenemos el aire, pero sin etiquetas extra
    ) +
    
    guides(
      color = guide_legend(nrow = 2, byrow = TRUE),
      linewidth = guide_legend(nrow = 2, byrow = TRUE),
      alpha = guide_legend(nrow = 2, byrow = TRUE)
    ) +
    
    labs(
      title = "El Mapa de la Sed",
      subtitle = "Evolución de la **Precipitación Acumulada** anual en el Observatorio del Retiro (Madrid). En un clima mediterráneo fuertemente estacional, la acumulación de agua (mm/m²) dicta el pulso de la región. El gráfico revela un amplio corredor histórico gris, roto por años extremos: la alarmante planicie de la <b style='color:#fb036c'>gran sequía</b> (donde el otoño y la primavera fallaron por completo) frente a la verticalidad de las <b style='color:#22adc9'>inundaciones récord</b>.",
      caption = generar_caption_2026("23", "Seasons (Timeseries)", "AEMET OpenData (Estación 3195). Limpieza de valores 'Ip'.", c_seco, c_texto),
      x = NULL,
      y = "Lluvia Acumulada (Litros / m²)"
    ) +
    
    theme_minimal(base_size = 14, base_family = "Pridi") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Pridi", face = "bold", size = rel(2.4), color = c_texto, margin = margin(b = 20)),
      
      # TEXTBOX RENDERIZADO ESTRICTAMENTE AQUÍ
      plot.subtitle = element_textbox_simple(family = "Pridi", size = rel(1.1), color = "#3b4140", margin = margin(b = 20), lineheight = 1.4),
      
      axis.title.y = element_text(family = "Pridi", size = rel(1.0), face = "bold", margin = margin(r = 15)),
      axis.text = element_text(family = "Pridi", face = "bold", size = rel(1.05), color = c_texto),
      
      panel.grid.major.y = element_line(color = "#b0b8b6", linewidth = 0.5, linetype = "dashed"),
      panel.grid.major.x = element_line(color = "#e0e5e3", linewidth = 0.8), # Guías para marcar el paso de los meses
      panel.grid.minor = element_blank(),
      
      # Leyenda arriba para limpiar el lienzo
      legend.position = "top",
      legend.justification = "center",
      legend.location = "plot",
      legend.title = element_blank(),
      legend.text = element_text(family = "Pridi", face = "bold", size = rel(1.1)),
      legend.key.size = unit(0.8, "cm"),
      
      plot.caption = element_markdown(family = "Roboto Condensed", size = rel(0.85), color = c_texto, hjust = 0, lineheight = 1.6, margin = margin(t = 30)),
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 24 — South China Morning Post (Timeseries)
# =============================================================================

plot_dia24_scmp_skeleton <- function(dt) {
  
  # Paleta SCMP
  c_bg <- "#EAE8D9"      # Fondo blanco roto
  c_area <- "#F7A395"    # Rojo/Coral (El crecimiento de Asia)
  c_texto <- "#000000"   # Negro absoluto SCMP
  c_grid <- "#C8B4A8"    # Marrón suave para líneas muy finas
  
  p <- ggplot(dt, aes(x = year, y = exports_trill)) +
    geom_area(fill = c_area, alpha = 0.85) +
    
    # Línea que delimita el borde
    geom_line(color = "#000000", linewidth = 0.5) +
    
    # MAGIA SCMP: Giramos los ejes para que sea vertical, y revertimos el año para que 1970 esté arriba
    coord_flip() +
    scale_x_reverse(breaks = seq(1970, 2020, by = 10), expand = c(0, 0)) +
    scale_y_continuous(position = "right", breaks = seq(0, 10, by = 2), labels = function(x) paste0("$", x, "T"), expand = c(0, 0)) +
    
    labs(
      title = "SKELETON PLOT: El Despertar del Dragón",
      subtitle = "Exportaciones de Asia Oriental y Pacífico (Trillones USD).\nDeja este espacio superior vacío para la cabecera SCMP.",
      x = NULL,
      y = "Volumen de Exportaciones (Billones de USD)"
    ) +
    
    theme_minimal(base_family = "sans") + # Usa una fuente genérica temporalmente
    theme(
      plot.background = element_rect(fill = c_bg, color = NA),
      panel.background = element_rect(fill = c_bg, color = NA),
      text = element_text(color = c_texto),
      
      # Todo alineado al estilo SCMP
      axis.text.y = element_text(face = "bold", size = 14, margin = margin(r = 10)),
      axis.text.x = element_text(face = "bold", size = 14),
      
      # Grid súper fino solo en vertical
      panel.grid.major.x = element_line(color = c_grid, linewidth = 0.3, linetype = "dotted"),
      panel.grid.major.y = element_line(color = c_grid, linewidth = 0.3),
      panel.grid.minor = element_blank(),
      
      plot.title = element_text(face = "bold", size = 20),
      plot.margin = margin(50, 40, 50, 40)
    )
  
  return(p)
}


plot_dia24_scmp_final <- function(dt, ruta_imagen = "R/30DayChartChallenge2026/data/day24_dragon.png", output_dir) {
  
  # Tipografía SCMP
  font_add_google("Merriweather", "Merriweather")
  showtext_opts(dpi = 300)
  showtext_opts(dpi = 300)
  showtext_auto()

  c_area <- "#F7A395"    # Rojo/Coral SCMP
  c_texto <- "#080606"   # Negro profundo
  c_grid <- "#C8B4A8"    # Marrón suave para el grid
  
  p_base <- ggplot(dt, aes(x = year, y = exports_trill)) +
    
    geom_area(fill = c_area, alpha = 0.65) +
    geom_line(color = c_texto, linewidth = 0.6) +
    
    # 2001: China entra en la OMC
    annotate("segment", x = 2001, xend = 2001, y = 1.7, yend = 1.9, color = c_texto, linewidth = 0.3) +
    annotate("text", x = 2001, y = 2.0, label = "2001\nChina entra en la OMC", family = "Merriweather", fontface = "bold", size = 4, hjust = 0, lineheight = 0.9) +
    
    # 2008: Crisis Financiera Global
    annotate("segment", x = 2009, xend = 2009, y = 3.9, yend = 4.6, color = c_texto, linewidth = 0.3) +
    annotate("text", x = 2009, y = 4.7, label = "2009\nCrisis Financiera Global", family = "Merriweather", size = 4, hjust = 0, lineheight = 0.9) +
    
    # 2020: Pandemia
    annotate("segment", x = 2020, xend = 2020, y = 6.4, yend = 7.4, color = c_texto, linewidth = 0.3) +
    annotate("text", x = 2020, y = 7.5, label = "2020\nShock del COVID-19", family = "Merriweather", size = 4, hjust = 0, lineheight = 0.9) +
    
    # Ejes invertidos
    coord_flip() +
    scale_x_reverse(breaks = seq(1970, 2020, by = 10), expand = c(0, 0)) +
    scale_y_continuous(position = "right", breaks = seq(0, 10, by = 2), labels = function(x) paste0("$", x, "T"), limits = c(0, 10), expand = c(0, 0.2)) +
    
    labs(
      title = "El Despertar del Dragón",
      subtitle = "Exportaciones de Asia Oriental y Pacífico (Trillones USD).\nTras décadas de aletargamiento, la región se convirtió\nen la fábrica del mundo a partir del año 2000.",
      caption = generar_caption_2026("24", "South China Morning Post", "World Bank Art: Nano Banana", c_area, c_texto),
      x = NULL,
      y = "Volumen de Exportaciones (Billones de USD)"
    ) +
    
    theme_minimal(base_family = "Merriweather") +
    theme(
      # LA CLAVE: Fondos 100% transparentes para que se vea la imagen de abajo
      plot.background = element_rect(fill = "transparent", color = NA),
      panel.background = element_rect(fill = "transparent", color = NA),
      
      text = element_text(color = c_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      plot.title = element_text(face = "bold", size = rel(2.8), margin = margin(b = 20)),
      plot.subtitle = element_text(size = rel(1.2), color = "#4a4a4a", margin = margin(b = 40), lineheight = 1.3),
      
      axis.text.y = element_text(face = "bold", size = rel(1.3), margin = margin(r = 10)),
      axis.text.x = element_text(face = "bold", size = rel(1.2)),
      axis.title.x = element_text(margin = margin(t = 20)),
      
      panel.grid.major.x = element_line(color = c_grid, linewidth = 0.4, linetype = "dotted"),
      panel.grid.major.y = element_line(color = c_grid, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      
      plot.caption = element_textbox_simple(
        family = "Merriweather", 
        size = rel(0.9), 
        color = "#666666", 
        halign = 0,                     
        lineheight = 1.6,               
        width = grid::unit(1, "npc"),   
        margin = margin(t = 30)
      ),
      plot.margin = margin(20, 40, 40, 40)
    )
  
  # Ensamblar la imagen
  img_fondo <- image_read(ruta_imagen)
  
  # ggdraw() crea un lienzo en blanco. 
  # draw_image() pinta el dragón al fondo.
  # draw_plot() pinta nuestro gráfico transparente encima.
  p_ensamblado <- ggdraw() +
    draw_image(img_fondo, scale = 1, x = 0, y = 0) + 
    draw_plot(p_base)
  
  ggsave(paste0(output_dir, "/dia24_scmp.png"), p_ensamblado, 
           width = 8, height = 16, dpi = 300, 
           device = ragg::agg_png)
  
  return(p_ensamblado)
}


# =============================================================================
# DÍA 25 — Space (Uncertainties)
# =============================================================================

plot_dia25_space <- function(dt, paleta) {
  
  setup_fonts_cat5()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  c_fondo   <- unname(paleta["dark"])       
  c_texto   <- unname(paleta["light_bg"])   
  c_grid    <- "#3d3531"                    
  
  c_linea   <- unname(paleta["magenta"])    # La probabilidad oficial
  c_ribbon  <- unname(paleta["blue"])       # El margen de error (El cono)
  c_alerta  <- unname(paleta["yellow"])     
  
  p <- ggplot(dt, aes(x = fecha)) +
    
    geom_ribbon(aes(ymin = prob_low, ymax = prob_high), fill = c_ribbon, alpha = 0.15) +
    geom_line(aes(y = prob_high), color = c_ribbon, linewidth = 0.3, alpha = 0.5, linetype = "dashed") +

    geom_line(aes(y = prob_mean), color = c_linea, linewidth = 1.2) +
    
    geom_point(data = dt[es_pico == TRUE], aes(y = prob_mean), color = c_alerta, size = 3) +
    geom_text(data = dt[es_pico == TRUE], aes(y = prob_high + 0.5, label = "26 DIC 2004\nPico de Incertidumbre\n(1 entre 37)"), 
              family = "Space Mono", color = c_alerta, size = 3.8, fontface = "bold", hjust = 0.5, lineheight = 0.9) +

    annotate("segment", x = as.Date("2004-12-27"), xend = as.Date("2005-01-02"), y = 1.5, yend = 4, color = c_texto, linewidth = 0.3) +
    annotate("text", x = as.Date("2005-01-03"), y = 4.2, label = "27 DIC:\nDescubren fotos antiguas de 1994.\nEl cono de órbita se afina y\nel riesgo colapsa a 0%.", 
             family = "Manrope", color = c_texto, size = 3.8, hjust = 0, lineheight = 0.9) +
    
    geom_hline(yintercept = 0, color = c_texto, linewidth = 0.5) +
    
    # Ejes adaptados a fechas reales
    scale_x_date(date_breaks = "1 week", date_labels = "%d %b\n%Y", expand = c(0, 0)) +
    scale_y_continuous(expand = expansion(mult = c(0, 0.1)), breaks = seq(0, 10, by = 2), labels = function(x) paste0(x, "%")) +
    
    labs(
      title = "El Pánico del Asteroide Apophis",
      subtitle = "Probabilidad oficial de impacto en la Tierra y banda de incertidumbre orbital reportada por la NASA en Diciembre de 2004. Durante días, la escasez de datos generó un margen de error masivo (azul), elevando la probabilidad de impacto al 2.7%. Todo colapsó cuando la recuperación de fotografías antiguas afinó el modelo.",
      caption = generar_caption_2026("25", "Space (Uncertainties)", "NASA JPL Sentry Earth Impact Monitoring (Historical Data)", c_linea, c_texto),
      x = NULL,
      y = "Probabilidad de Impacto Estimada (%)"
    ) +
    
    theme_minimal(base_size = 14, base_family = "Space Mono") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Manrope", face = "bold", size = rel(2.2), color = c_texto, margin = margin(b = 10)),
      plot.subtitle = element_textbox_simple(
        family = "Manrope", size = rel(1.1), color = "#a8b1c2", 
        margin = margin(b = 30), lineheight = 1.4, width = grid::unit(1, "npc")
      ),
      
      plot.caption = element_textbox_simple(
        family = "Manrope", size = rel(0.85), color = "#a8b1c2", 
        halign = 0, lineheight = 1.6, width = grid::unit(1, "npc"), margin = margin(t = 30)
      ),
      
      axis.title.y = element_text(family = "Space Mono", size = rel(1.0), margin = margin(r = 15)),
      axis.text = element_text(family = "Space Mono", size = rel(1.1), color = "#88929e"),
      
      panel.grid.major = element_line(color = c_grid, linewidth = 0.4, linetype = "dotted"),
      panel.grid.minor = element_blank(),
      
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 26 — Trend (Uncertainties)
# =============================================================================

plot_dia26_trend <- function(datos, paleta) {
  
  setup_fonts_cat5()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  # Separar los data.tables
  dt_real <- datos$real
  dt_proj <- datos$proyecciones
  
  # Extraemos colores de la paleta
  c_fondo  <- unname(paleta["light_bg"])   # #f8f9fd
  c_texto  <- unname(paleta["dark"])       # #241e1c
  c_grid   <- unname(paleta["light_alt"])  # #e2e6e4
  c_pua    <- unname(paleta["blue"])       # #3884ff (Las proyecciones)
  c_alerta <- unname(paleta["magenta"])    # #ff006e (El efecto anuncio)
  
  # Fechas clave para el Efecto Anuncio
  fecha_anuncio <- as.Date("2014-06-05")   # Draghi anuncia tipos negativos
  fecha_ejecucion <- as.Date("2015-03-09") # Comienza oficialmente el QE
  
  p <- ggplot() +
    
    # Efecto Anuncio
    annotate("rect", xmin = fecha_anuncio, xmax = fecha_ejecucion, 
             ymin = -Inf, ymax = Inf, fill = c_alerta, alpha = 0.08) +
    
    # Las proyecciones fallidas calculadas con modelo NSS
    geom_line(data = dt_proj, aes(x = fecha, y = tasa_proj, group = vintage), 
              color = c_pua, linewidth = 0.7, linetype = "dashed", alpha = 0.6) +
    
    # La tendencia real del Euribor
    geom_line(data = dt_real, aes(x = fecha, y = tasa_real), 
              color = c_texto, linewidth = 1.5) +
    
    # Línea del Cero (Psicológica y matemática)
    geom_hline(yintercept = 0, color = c_texto, linewidth = 0.5, linetype = "dotted") +
    
    # Labels
    annotate("segment", x = fecha_anuncio, xend = fecha_anuncio, y = -Inf, yend = Inf, 
             color = c_alerta, linewidth = 0.5) +
    
    annotate("text", x = fecha_anuncio + 15, y = -0.6, 
             label = "EFECTO ANUNCIO\nDraghi abre la puerta\na los tipos negativos", 
             family = "Space Mono", color = c_alerta, size = 3.6, fontface = "bold", hjust = 0, lineheight = 0.9) +
    
    annotate("text", x = as.Date("2016-05-01"), y = 0.12, 
             label = "PROYECCIONES (NSS Forward)\nEl mercado esperaba constantemente\nun repunte que nunca llegó.", 
             family = "Manrope", color = c_pua, size = 3.6, fontface = "italic", hjust = 0, lineheight = 0.9) +
    
    # ESCALAS (Ajustadas a los datos reales empíricos)
    scale_x_date(date_breaks = "1 year", date_labels = "%Y", expand = c(0, 0), 
                 limits = c(as.Date("2013-01-01"), as.Date("2019-01-01"))) +
    
    scale_y_continuous(breaks = seq(-1.0, 1.0, by = 0.2), limits = c(-0.9, 0.5), 
                       labels = function(x) paste0(x, "%")) +
    
    # TITULARES Y NARRATIVA
    labs(
      title = "El Erizo del BCE: Promesas y Realidad",
      subtitle = "Expectativas del mercado (líneas azules discontinuas) frente a la evolución real de la curva a 1 año (línea negra). El gráfico evidencia la brutal incertidumbre de los modelos predictivos: trimestre tras trimestre, el mercado proyectaba un retorno a la normalidad. Sin embargo, la credibilidad del BCE generó un 'Efecto Anuncio' que hundió la curva real hacia terreno negativo mucho antes de que se ejecutaran las compras masivas de bonos.",
      caption = generar_caption_2026("26", "Trend (Uncertainties)", "Datos: ECB Yield Curve Parameters (Nelson-Siegel-Svensson)", c_pua, c_texto),
      x = NULL,
      y = "Tipo de Interés / Forward Rate (%)"
    ) +
    
    # TEMA VISUAL: Categoría 5 (Uncertainties)
    theme_minimal(base_size = 14, base_family = "Space Mono") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Manrope", face = "bold", size = rel(2.05), color = c_texto, margin = margin(b = 20)),
      plot.subtitle = element_textbox_simple(
        family = "Manrope", size = rel(1.1), color = "#555b6e", 
        margin = margin(b = 30), lineheight = 1.4, width = grid::unit(1, "npc")
      ),
      plot.caption = element_textbox_simple(
        family = "Manrope", size = rel(0.85), color = "#555b6e", 
        halign = 0, lineheight = 1.6, width = grid::unit(1, "npc"), margin = margin(t = 30)
      ),
      
      axis.title.y = element_text(family = "Space Mono", size = rel(1.0), margin = margin(r = 15)),
      axis.text = element_text(family = "Space Mono", size = rel(1.1), color = "#555b6e"),
      
      panel.grid.major = element_line(color = c_grid, linewidth = 0.5),
      panel.grid.minor = element_blank(),
      
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 27 — Animation (Uncertainties)
# =============================================================================

plot_dia27_animation <- function(datos, paleta) {
  
  setup_fonts_cat5()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  dt_puntos <- datos$puntos
  dt_lineas <- datos$lineas
  
  # Colores
  c_fondo   <- unname(paleta["dark"])       # #241e1c
  c_texto   <- unname(paleta["light_bg"])   # #f8f9fd
  c_puntos  <- unname(paleta["blue"])       # #3884ff (Azul eléctrico)
  c_linea   <- unname(paleta["magenta"])    # #ff006e (Rosa neón)
  
  p <- ggplot() +
    
    # Datos estáticos
    geom_point(data = dt_puntos, aes(x = x, y = y), 
               color = c_puntos, alpha = 0.3, size = 2) +
    
    # Línea móvil
    geom_line(data = dt_lineas, aes(x = x, y = y, group = 1), 
              color = c_linea, linewidth = 1.8, lineend = "round") +
    
    scale_x_continuous(limits = c(0, 10), breaks = seq(0, 10, 2)) +
    scale_y_continuous(limits = c(-5, 20), breaks = seq(-5, 20, 5)) +
    
    # Textos
    labs(
      title = "El Baile de la Regresión",
      subtitle = "Visualización de Incertidumbre mediante HOPs (Hypothetical Outcome Plots). En lugar de mostrar un área de confianza estática, la animación proyecta 50 regresiones lineales calculadas mediante *Bootstrapping*. La vibración de la línea rosa revela intuitivamente al cerebro la inestabilidad de la tendencia: allí donde oscila más, los datos son menos concluyentes.",
      caption = generar_caption_2026("27", "Animation (Uncertainties)", "Simulación Bootstrapping {gganimate}", c_linea, c_texto),
      x = "Variable Explicativa (X)",
      y = "Variable de Respuesta (Y)"
    ) +
    
    # Tema
    theme_minimal(base_size = 14, base_family = "Space Mono") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Manrope", face = "bold", size = rel(2.4), color = c_texto, margin = margin(b = 10)),
      plot.subtitle = element_textbox_simple(
        family = "Manrope", size = rel(1.1), color = "#a8b1c2", 
        margin = margin(b = 20), lineheight = 1.4, width = grid::unit(1, "npc")
      ),
      plot.caption = element_textbox_simple(
        family = "Manrope", size = rel(0.85), color = "#a8b1c2", 
        halign = 0, lineheight = 1.6, width = grid::unit(1, "npc"), margin = margin(t = 20)
      ),
      
      axis.title = element_text(family = "Space Mono", color = "#a8b1c2"),
      axis.text = element_text(family = "Space Mono", color = "#88929e"),
      panel.grid.major = element_line(color = "#3d3531", linewidth = 0.4),
      panel.grid.minor = element_blank(),
      
      plot.margin = margin(20, 30, 20, 30)
    ) +
    
    # Motor animación
    transition_states(frame, transition_length = 2, state_length = 1) +
    ease_aes('sine-in-out') # Transiciones suaves como si la línea "respirara"
  
  return(p)
}


# =============================================================================
# DÍA 28 — Modeling (Uncertainties)
# =============================================================================

plot_dia28_modeling <- function(datos, paleta, ruta_salida) {
  
  setup_fonts_cat5()
  setup_fonts_2026()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  dt_spag   <- datos$spaghetti
  dt_quant  <- datos$quantiles
  dt_vol    <- datos$vol_proj
  params    <- datos$params
  s0        <- datos$s0
  
  # Paleta Categoría 5
  c_fondo   <- unname(paleta["dark"])       
  c_texto   <- unname(paleta["light_bg"])   
  c_grid    <- "#3d3531"
  c_lineas  <- unname(paleta["blue"])       
  c_modelo  <- unname(paleta["magenta"])    
  c_alerta  <- unname(paleta["yellow"])
  
  # PANEL SUPERIOR: COMPARATIVA DE PRECIOS (GBM vs GARCH)  
  p_precios <- ggplot() +
    
    # Caos (Spaghetti)
    geom_line(data = dt_spag, aes(x = dia, y = precio, group = sim_id), 
              color = c_lineas, alpha = 0.1, linewidth = 0.2) +
    # Cono del 90%
    geom_ribbon(data = dt_quant, aes(x = dia, ymin = p05, ymax = p95), 
                fill = c_modelo, alpha = 0.15) +
    # Cono del 50%
    geom_ribbon(data = dt_quant, aes(x = dia, ymin = p25, ymax = p75), 
                fill = c_modelo, alpha = 0.25) +
    # Mediana (P50)
    geom_line(data = dt_quant, aes(x = dia, y = p50), 
              color = c_modelo, linewidth = 1) +
    
    # Separamos los dos modelos lado a lado
    facet_wrap(~modelo, ncol = 2) +
    
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 252, by = 63)) +
    scale_y_continuous(labels = scales::dollar_format(prefix = "$")) +
    
    labs(
      title = "La Ilusión de la Normalidad",
      subtitle = glue::glue("Comparativa de proyecciones a 1 año del S&P 500 (Base: ${round(s0, 2)}). El modelo clásico (GBM) asume volatilidad constante, subestimando el riesgo extremo. El modelo GARCH(1,1)-t captura la realidad empírica: volatilidad persistente (99.8%) y colas gordas (ν=5.15). Observa cómo el cono GARCH se abre mucho más en los extremos, revelando el verdadero abismo de los 'Cisnes Negros'."),
      x = NULL,
      y = "Precio Proyectado (USD)"
    ) +
    
    theme_minimal(base_size = 14, base_family = "Space Mono") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_texto),
      
      plot.title = element_text(family = "Manrope", face = "bold", size = rel(1.8), color = c_texto, margin = margin(b = 20, r = 20, l = 20)),
      plot.subtitle = element_textbox_simple(
        family = "Manrope", size = rel(0.90), color = "#a8b1c2", 
        margin = margin(b = 20, r = 20, l = 20), lineheight = 1.4, width = grid::unit(1, "npc")
      ),

      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      strip.text = element_text(family = "Space Mono", face = "bold", color = c_texto, size = rel(1.2), margin = margin(b = 10)),

      axis.text = element_text(family = "Space Mono", color = "#88929e", size = rel(0.9)),
      axis.title.y = element_text(family = "Space Mono", size = rel(1.0), margin = margin(r = 15)),
      axis.title.x = element_text(family = "Space Mono", size = rel(1.0), margin = margin(t = 15)),
      panel.grid.major = element_line(color = c_grid, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.margin = margin(30, 50, 10, 20)
    )
  
  # PANEL INFERIOR: DINÁMICA DE LA VOLATILIDAD (GARCH)
  p_vol <- ggplot(dt_vol, aes(x = dia)) +
    
    geom_ribbon(aes(ymin = vol_p10, ymax = vol_p90), fill = c_alerta, alpha = 0.15) +
    geom_line(aes(y = vol_media), color = c_alerta, linewidth = 1) +
    
    # Línea de volatilidad histórica
    geom_hline(yintercept = datos$vol_hist_pct, color = c_texto, linetype = "dashed", linewidth = 0.5) +
    
    annotate("text", x = 5, y = datos$vol_hist_pct + 3, label = "Volatilidad Histórica (21d)", 
             family = "Space Mono", color = c_texto, size = 3, hjust = 0) +
             
    scale_x_continuous(expand = c(0, 0), breaks = seq(0, 252, by = 63)) +
    scale_y_continuous(labels = function(x) paste0(x, "%"), limits = c(0, 80)) +
    
    labs(
      title = "Agrupamiento de Volatilidad Condicional (Volatility Clustering)",
      x = "Días de Trading (Proyección a 1 Año)",
      y = "Volatilidad Anual. (%)",
      caption = generar_caption_2026("28", "Modeling (Uncertainties)", "Simulación GARCH(1,1)-t vía {rugarch}", c_modelo, c_texto)
    ) +
    
    theme_minimal(base_size = 12, base_family = "Space Mono") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_texto),
      
    plot.title = element_text(family = "Manrope", face = "bold", size = rel(1.2), color = c_alerta, hjust = 0,                
        margin = margin(b = 15, l = 30) 
      ),      
      plot.caption = element_textbox_simple(
        family = "Manrope", size = rel(0.9), color = "#a8b1c2", 
        halign = 0, lineheight = 1.6, width = grid::unit(1, "npc"), margin = margin(t = 20)
      ),

      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      axis.text = element_text(family = "Space Mono", color = "#88929e", size = rel(0.9)),
      axis.title.y = element_text(family = "Space Mono", size = rel(1.0), margin = margin(r = 15)),
      axis.title.x = element_text(family = "Space Mono", size = rel(1.0), margin = margin(t = 15)),
      panel.grid.major = element_line(color = c_grid, linewidth = 0.4),
      panel.grid.minor = element_blank(),
      plot.margin = margin(30, 50, 10, 20)
    )
  

  # ENSAMBLAJE FINAL CON COWPLOT
  plot_final <- plot_grid(p_precios, p_vol, ncol = 1, rel_heights = c(2, 1), align = "v", axis = "lr")
  
  ggsave(ruta_salida, plot_final, width = 8, height = 10, dpi = 300, device = ragg::agg_png)

  plot_final

}


# =============================================================================
# DÍA 29 — Monochrome (Uncertainties)
# =============================================================================

plot_dia29_monochrome <- function(datos, paleta) {
  
  setup_fonts_cat5()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  dt_hist <- datos$hist
  dt_dots <- datos$dots
  
  # LA REGLA MONOCROMA: Solo dos tonos permitidos.
  c_fondo <- unname(paleta["light_bg"])   # #f8f9fd (El "Papel")
  c_tinta <- unname(paleta["dark"])       # #241e1c (La "Tinta")
  c_grid  <- unname(paleta["light_alt"])  # #e2e6e4 (Las "Guías")
  
  p <- ggplot() +
    
    # 1. EL PASADO (Línea implacable de la realidad histórica)
    # geom_step simula los saltos reales de las reuniones de política monetaria
    geom_step(data = dt_hist, aes(x = x, y = tasa), color = c_tinta, linewidth = 1.2) +
    
    # El ancla del "HOY" (Punto final de la curva histórica)
    geom_point(data = dt_hist[x == max(x)], aes(x = x, y = tasa), color = c_fondo, fill = c_tinta, shape = 21, size = 4, stroke = 1.5) +
    
    # 2. EL FUTURO (La Incertidumbre Institucional / El Dot Plot)
    geom_point(data = dt_dots, aes(x = x_final, y = tasa), 
               color = c_tinta, fill = c_tinta, shape = 21, size = 3.2, alpha = 0.7) +
    
    # Anotaciones Periodísticas
    annotate("text", x = 2023.5, y = 1.0, label = "TASA EFECTIVA\nREAL", 
             family = "Space Mono", color = c_tinta, size = 3.2, hjust = 0, lineheight = 0.9) +
             
    annotate("segment", x = 2026.2, xend = 2026.8, y = 3.625, yend = 3.625, 
             color = c_tinta, linewidth = 0.4, linetype = "dotted") +
             
    annotate("text", x = 2029.0, y = 4.8, label = "FRACTURA DE CONSENSO\n(Máxima Incertidumbre)", 
             family = "Space Mono", color = c_tinta, size = 3.2, hjust = 0, lineheight = 0.9) +
             
    # Escalas personalizadas para alinear historia y futuro
    scale_x_continuous(
      limits = c(2021.5, 2033), 
      breaks = c(2022, 2023, 2024, 2025, 2027.5, 2028.8, 2029.8, 2031.5), 
      labels = c("2022", "2023", "2024", "2025", "2026", "2027", "2028", "Largo\nPlazo"),
      expand = c(0, 0)
    ) +
    scale_y_continuous(limits = c(-0.5, 6), breaks = seq(0, 6, by = 1), labels = function(x) paste0(x, "%")) +
    
    labs(
      title = "La Anatomía del Desacuerdo",
      subtitle = "Proyecciones del FOMC (Dot Plot) de Marzo de 2026. Limitados a una sola tinta, la incertidumbre del Banco Central se manifiesta geométricamente a través de la dispersión visual. Tras la agresiva constricción monetaria iniciada en 2022 (línea continua), el consenso institucional se fractura. La nube de puntos revela un futuro inmediato fuertemente polarizado y una profunda duda matemática sobre la tasa de equilibrio a largo plazo.",
      caption = generar_caption_2026("29", "Monochrome (Uncertainties)", "US Federal Reserve (FOMC SEP March 18, 2026)", c_tinta, c_tinta),
      x = NULL,
      y = "Fed Funds Target Rate (%)"
    ) +
    
    # Theme estricto monocromo
    theme_minimal(base_size = 14, base_family = "Space Mono") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_tinta),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Manrope", face = "bold", size = rel(2.4), color = c_tinta, margin = margin(b = 20)),
      plot.subtitle = element_textbox_simple(
        family = "Manrope", size = rel(1.1), color = c_tinta, 
        margin = margin(b = 40), lineheight = 1.4, width = grid::unit(1, "npc")
      ),
      plot.caption = element_textbox_simple(
        family = "Manrope", size = rel(0.85), color = c_tinta, 
        halign = 0, lineheight = 1.6, width = grid::unit(1, "npc"), margin = margin(t = 30)
      ),
      
      axis.title.y = element_text(family = "Space Mono", size = rel(1.0), margin = margin(r = 15)),
      axis.text.y = element_text(family = "Space Mono", color = c_tinta, size = rel(1.1)),
      
      # Los saltos de línea (\n) en las etiquetas de X requieren lineheight
      axis.text.x = element_text(family = "Space Mono", color = c_tinta, size = rel(1.1), lineheight = 0.9, margin = margin(t = 10)),
      
      panel.grid.major.y = element_line(color = c_grid, linewidth = 0.5),
      panel.grid.major.x = element_blank(),
      panel.grid.minor = element_blank(),
      
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


# =============================================================================
# DÍA 29 — Global Health Data Exchange (Uncertainties)
# =============================================================================

plot_dia30_ghdx <- function(dt, paleta) {
  
  setup_fonts_cat5()
  showtext_opts(dpi = 300)
  showtext_auto()
  
  c_fondo   <- unname(paleta["dark"])       
  c_texto   <- unname(paleta["light_bg"])   
  c_grid    <- "#3d3531"
  c_alerta  <- unname(paleta["magenta"])    
  c_rango   <- "#8a244c"                    
  
  p <- ggplot(dt, aes(x = pct_val, y = pathogen)) +
    
    # 1. LA INCERTIDUMBRE PROPORCIONAL
    geom_linerange(aes(xmin = pct_lower, xmax = pct_upper), color = c_rango, linewidth = 4, alpha = 0.6) +
    
    # 2. EL VALOR CENTRAL (%)
    geom_point(color = c_alerta, size = 5, shape = 21, fill = c_fondo, stroke = 1.5) +
    
    # Textos sobre los puntos (Añadimos % y un offset dinámico para que no pise la barra)
    geom_text(aes(x = pct_upper + 0.8, label = sprintf("%.1f%%", pct_val)), 
              family = "Space Mono", color = c_texto, size = 3.5, hjust = 0) +
    
    # Escalas adaptadas a porcentajes
    scale_x_continuous(labels = function(x) paste0(x, "%"), 
                       expand = expansion(mult = c(0, 0.15))) +
    
    labs(
      title = "La Pandemia Oculta:\nSuperbacterias",
      subtitle = "Porcentaje de muertes globales (2019) atribuibles directamente a la Resistencia Antimicrobiana por patógeno. Solo la bacteria *E. coli* es responsable de más del 17% de la letalidad mundial. Los puntos indican la estimación central del IHME, mientras que las barras magentas visualizan el inmenso Intervalo de Incertidumbre (95% UI), reflejo de los vacíos de vigilancia epidemiológica a nivel global.",
      caption = generar_caption_2026("30", "GHDx (Uncertainties)", "Fuente: IHME Global Burden of Disease (AMR 2019)", c_alerta, c_texto),
      x = "Proporción del Total de Muertes AMR (%)",
      y = NULL
    ) +
    
    theme_minimal(base_size = 14, base_family = "Space Mono") +
    theme(
      plot.background = element_rect(fill = c_fondo, color = NA),
      panel.background = element_rect(fill = c_fondo, color = NA),
      text = element_text(color = c_texto),
      
      plot.title.position = "plot",
      plot.caption.position = "plot",
      
      plot.title = element_text(family = "Manrope", face = "bold", size = rel(2.4), color = c_texto, margin = margin(b = 10)),
      plot.subtitle = element_textbox_simple(
        family = "Manrope", size = rel(1.1), color = "#a8b1c2", 
        margin = margin(b = 30), lineheight = 1.4, width = grid::unit(1, "npc")
      ),
      plot.caption = element_textbox_simple(
        family = "Manrope", size = rel(0.85), color = "#a8b1c2", 
        halign = 0, lineheight = 1.6, width = grid::unit(1, "npc"), margin = margin(t = 30)
      ),
      
      axis.text.y = element_text(family = "Manrope", face = "italic", color = c_texto, size = rel(1.2), margin = margin(r = 15)),
      axis.title.x = element_text(family = "Space Mono", size = rel(1.0), margin = margin(t = 15), color = "#a8b1c2"),
      axis.text.x = element_text(family = "Space Mono", color = "#88929e"),
      
      panel.grid.major.x = element_line(color = c_grid, linewidth = 0.5, linetype = "dashed"),
      panel.grid.major.y = element_blank(),
      panel.grid.minor = element_blank(),
      
      plot.margin = margin(30, 40, 30, 40)
    )
    
  return(p)
}


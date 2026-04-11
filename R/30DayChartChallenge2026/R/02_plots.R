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
library(tidygraph)
library(showtext)
library(logger)
library(glue)


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


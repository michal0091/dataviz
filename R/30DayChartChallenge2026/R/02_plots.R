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
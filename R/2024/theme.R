# theme -------------------------------------------------------------------
# libs --------------------------------------------------------------------
library(sysfonts)
library(showtext)
library(ggplot2)
library(ggtext)

caption_text <- function(viz_author = "Michal Kinel",
                         source_text,
                         color_text_1 = "#352725",
                         color_text_2 = "#3f68e3",
                         github_icon = '&#xf113',
                         github_username = "michal0091",
                         twitter_icon = "&#xf081",
                         twitter_username = "nico_kinel",
                         linkedin_icon = "&#xf08c",
                         linkedin_username = "michal-kinel",
                         mastodon_icon = "&#xf4f6",
                         mastodon_username = "miki_peltzer",
                         mastodon_server = "techhub.social") {
  
  social_caption <- glue::glue(
    "
  <span style='color: {color_text_2}'><strong>Data Visualization</strong>:   {viz_author}  </span>
  <span style='color: {color_text_1}'><strong>Source</strong>:   {source_text}</span><br>
  <span style='font-family:\"fa-brands\"; color: {color_text_2};'>{github_icon};</span>
  <span style='color: {color_text_1}'>{github_username} </span>
  <span style='font-family:\"fa-brands\"; color: {color_text_2}'>{twitter_icon};</span>
  <span style='color: {color_text_1}'>{twitter_username} </span>
  <span style='font-family:\"fa-brands\"; color: {color_text_2}'>{linkedin_icon};</span>
  <span style='color: {color_text_1}'>{linkedin_username} </span>
  <span style='font-family:\"fa-brands\"; color: {color_text_2}'>{mastodon_icon};</span>
  <span style='color: {color_text_1}'>{mastodon_username}@<span><span style='color: {color_text_1}'>{mastodon_server}</span>
  "
  )
  
  social_caption
  
}

# Colors ------------------------------------------------------------------
color_background <- "#fbfdfe"
color_text_1   <- "#352725"
color_text_2   <- "#3f68e3"
color_set_1 <- "#2a668a"
color_set_2 <- "#f9ad0d"
color_set_3 <- "#67deb0" 


#  ** pallettes ------------------------------------------------------------
gradient <-
  c("#2a668a",
    "#0089a4",
    "#00aba9",
    "#40cb9b",
    "#9ce582",
    "#f9f871")

main <- c("#2a668a", "#6399c0", "#f9ad0d", "#b6ab00", "#67deb0", "#384b42")

# Fonts -------------------------------------------------------------------
font_add_google(name = "Inter", family = "inter_regular", regular.wt = 400, bold.wt = 700)
font_add_google(name = "Inter", family = "inter_bold", regular.wt = 700, bold.wt = 900)
font_add_google(name = "Inter", family = "inter_light", regular.wt = 300, bold.wt = 400)
font_add_google(name = "Inter", family = "inter_thin", regular.wt = 100, bold.wt = 200)
font_add(family = "fa-brands", regular = "R/30DayChartChallenge2024/theme/fa-brands-400.ttf")
showtext_auto()

# Pallete -----------------------------------------------------------------
#  ** pallette list -------------------------------------------------------
my_palettes <- list(`main`  = main,
                    `gradient` = gradient)


#  ** base pallette function ----------------------------------------------
my_pal <- function(palette = "main",
                   reverse = FALSE,
                   ...) {
  pal <- my_palettes[[palette]]
  if (reverse)
    pal <- rev(pal)
  colorRampPalette(pal, ...)
}


#  ** scale functions -----------------------------------------------------
# Color
scale_color_my <-
  function(palette = "main",
           discrete = TRUE,
           reverse = FALSE,
           ...) {
    pal <- my_pal(palette = palette, reverse = reverse)
    if (discrete) {
      discrete_scale("colour", palette = pal, ...)
    } else {
      scale_color_gradientn(colours = pal(256), ...)
    }
  }

# Fill
scale_fill_my <-
  function(palette = "main",
           discrete = TRUE,
           reverse = FALSE,
           ...) {
    pal <- my_pal(palette = palette, reverse = reverse)
    
    if (discrete) {
      discrete_scale("fill", palette = pal, ...)
    } else {
      scale_fill_gradientn(colours = pal(256), ...)
    }
  }


# Theme functions ---------------------------------------------------------
theme_my <- function(font_regular = "inter_regular",
                     font_bold = "inter_bold",
                     font_light = "inter_light",
                     color_text_1 = "#352725",
                     color_text_2 = "#3f68e3",
                     color_background = "#fbfdfe") {
  theme_void()  %+replace%
    theme(
      #--- General: text
      text = element_text(family = "inter_regular", color = color_text_1),
      #--- Plot
      plot.title = element_text(
        size = 26,
        family = "inter_bold",
        color = color_text_1,
        face = "bold",
        hjust = 0,
        vjust = 4
      ),
      plot.subtitle = element_text(
        size = 22,
        family = "inter_bold",
        color = color_text_2,
        hjust = 0,
        vjust = 4
      ),
      plot.caption =  element_textbox_simple(
        size = 10.5,
        lineheight = .5,
        padding = margin(.1, .1, .1, .1, "cm"),
        margin = margin(.1, .1, .1, .1, "cm"),
      ),
      plot.tag = element_text(
        size = 14,
        family = "inter_bold",
        color = color_text_2
      ),
      plot.background = element_rect(fill = color_background, color = NA),
      plot.margin = margin(.8, .8, .8, .8, "cm"),
      #--- Panel
      panel.background = element_rect(fill = color_background, color = NA),
      panel.grid.major = element_line(
        colour = color_text_1,
        linetype = "dotted",
        linewidth = .3
      ),
      panel.grid.minor = element_blank(),
      #--- Axis
      axis.title = element_text(
        size = 12,
        family = "inter_bold",
        color = color_text_1
      ),
      axis.title.y = element_text(
        size = 12,
        family = "inter_bold",
        color = color_text_1,
        angle = 90
      ),
      axis.text = element_text(
        size = 10,
        family = "inter_light",
        color = color_text_1
      ),
      #--- Strip
      strip.background = element_rect(fill = color_text_1, color = color_text_1),
      strip.text = element_text(
        color = color_background,
        size = 14,
        family = "inter_bold"
      ),
      strip.placement = "outside",
      strip.clip = "off",
      #--- Legend
      legend.title = element_text(
        size = 14,
        family = "inter_bold",
        color = color_text_1
      ),
      legend.text = element_text(
        size = 12,
        family = "inter_regular",
        color = color_text_1
      ),
      legend.background = element_rect(fill = color_background, color = NA),
      legend.key = element_rect(fill = color_background, color = NA),
      legend.spacing = unit(0.1, "cm"),
      legend.key.spacing = unit(0.1, "cm")
      
    )
}

# Libs --------------------------------------------------------------------
library(patchwork)
library(emojifont)
library(ggtext)
library(tidyverse)



# Caption text ------------------------------------------------------------
caption_text <- function(viz_author = "Michal Kinel",
                         source_text,
                         github_icon = '&#xf113',
                         github_username = "michal0091",
                         twitter_icon = "&#xf081",
                         twitter_username = "nico_kinel",
                         linkedin_icon = "&#xf08c",
                         linkedin_username = "michal-kinel",
                         mastodon_icon = "&#xf4f6",
                         mastodon_username = "miki_peltzer",
                         mastodon_server = "techhub.social",
                         day_type,
                         day_hashtag) {
  
  social_caption <- glue::glue(
    "
  <span style='color: {color_text_2}'><strong>Data Visualization</strong>:   {viz_author}  </span>
  <span style='color: {color_text_1}'><strong>Source</strong>:   {source_text}</span><br>
  <span style='font-family:\"fa-brands\"; color: {highlight_color};'>{github_icon};</span>
  <span style='color: {text_color}'>{github_username} </span>
  <span style='font-family:\"fa-brands\"; color: {highlight_color}'>{twitter_icon};</span>
  <span style='color: {text_color}'>{twitter_username} </span>
  <span style='font-family:\"fa-brands\"; color: {highlight_color}'>{linkedin_icon};</span>
  <span style='color: {text_color}'>{linkedin_username} </span>
  <span style='font-family:\"fa-brands\"; color: {highlight_color}'>{mastodon_icon};</span>
  <span style='color: {text_color}'>{mastodon_username}@<span><span style='color: {text_color}'>{mastodon_server}</span><br>
  <span style='color: {color_text_2}'>#30DayChartChallenge </span>
  <span style='color: {color_text_1}'><strong>{day_type}</strong> </span>
  <span style='color: {color_text_2}'>{day_hashtag}</span>
  "
  )
  
  social_caption
  
}


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
theme_my <- function() {
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
        colour = color_text,
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


# Test plot ---------------------------------------------------------------
plot <- ggplot(mpg, aes(displ, hwy, colour = class)) +
  geom_point(size = 2) +
  labs(
    title = "Title of the plot",
    subtitle = "Subtitle of the plot",
    caption = social_caption,
    tag = "Tag tex"
  ) +
  theme_my() +
  scale_color_my()


ggsave(
  filename = "test_plot.png",
  path = normalizePath("R/30DayChartChallenge2024/"),
  plot = plot,
  device = "png",
  units = "mm",
  width = 148,
  height = 105,
  dpi = 300
)


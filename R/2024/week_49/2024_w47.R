# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(ggforce)
library(ggtext)
library(ggrepel)
library(emojifont)
library(sysfonts)
library(showtext)
library(ggplot2)
library(tidyverse)
library(mlmhelpr)
library(sysfonts)
library(showtext)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")

# Font Awesome
font_add(family = "fa-brands", regular = "fonts/fa-brands-400.ttf")
showtext_auto()

# Functions ---------------------------------------------------------------
new_caption_text <- function(viz_author = "Michal Kinel",
                             source_text,
                             color_text_1 = "#352725",
                             color_text_2 = "#3f68e3",
                             github_icon = '&#xf113',
                             github_username = "michal0091",
                             twitter_icon = "&#xf081",
                             twitter_username = "nico_kinel",
                             linkedin_icon = "&#xf08c",
                             linkedin_username = "michal-kinel",
                             bluesky_icon = "&#xe671",
                             bluesky_username = "mikipe",
                             bluesky_server = "bsky.sociall") {
  
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
  <span style='font-family:\"fa-brands\"; color: {color_text_2}'>{bluesky_icon};</span>
  <span style='color: {color_text_1}'>{bluesky_username}.<span><span style='color: {color_text_1}'>{bluesky_server}</span>
  "
  )
  
  social_caption
  
}


# Data --------------------------------------------------------------------
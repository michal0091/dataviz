# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(ggforce)
library(ggtext)
library(ggrepel)
library(emojifont)
library(sysfonts)
library(showtext)
library(patchwork)
library(lubridate)
library(zoo)
library(terra)
library(tidyterra)
library(giscoR)
library(sf)
# devtools::install_github("mtennekes/tmaptools")
library(tmaptools)

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

# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")

urls <- c(
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2000_GLOBE_R2023A_4326_3ss/V1-0/tiles/GHS_POP_E2000_GLOBE_R2023A_4326_3ss_V1_0_R5_C18.zip",
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2000_GLOBE_R2023A_4326_3ss/V1-0/tiles/GHS_POP_E2000_GLOBE_R2023A_4326_3ss_V1_0_R6_C18.zip",
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_3ss/V1-0/tiles/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0_R5_C18.zip",
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_3ss/V1-0/tiles/GHS_POP_E2020_GLOBE_R2023A_4326_3ss_V1_0_R6_C18.zip"

)

options(timeout = 600)

create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
}
path <- "R/30DayChartChallenge2024/day_23/ghsl_s3"
create_dir(path)


lapply(urls, function(x) {
  download.file(x, file.path(path, basename(x)))
  unzip(zipfile = file.path(path, basename(x)),
        exdir = path, overwrite = T)
})


# Load data
file_names <- list.files(
  path = path,
  pattern = "tif$",
  full.names = T
)
file_names_2000 <- file_names[str_detect(file_names, "E2000")]
file_names_2020 <- file_names[str_detect(file_names, "E2020")]

pop_rasters_2000 <- lapply(
  file_names_2000,
  terra::rast
)

pop_rasters_2000 <- sprc(pop_rasters_2000)
pop_rasters_2000 <- terra::mosaic(pop_rasters_2000) 
pop_rasters_2000 <- terra::project(pop_rasters_2000, "EPSG:4326") 

pop_rasters_2020 <- lapply(
  file_names_2020,
  terra::rast
)
c_pop_rasters_2020 <- sprc(pop_rasters_2020)
pop_rasters_2020 <- terra::mosaic(c_pop_rasters_2020) 

# Download Spain 
spain <- sf::st_as_sf(raster::getData('GADM', country = 'ES', level = 2))
guadalajara <- spain %>% filter(NAME_2 == "Guadalajara")
ClM <- spain %>% filter(NAME_1 == "Castilla-La Mancha")

pop_rasters <- list(pop_rasters_2000, pop_rasters_2020)
# Corp to country borders
ClM_pop_rasters <- lapply(
  pop_rasters,
  function(x) {
    terra::crop(
      x,
      terra::vect(ClM),
      snap = "in",
      mask = T
    )
  }
)



# Population change
pop_change <- (
  ClM_pop_rasters[[2]] - ClM_pop_rasters[[1]]
) %>%  terra::project("EPSG:4326")

# Categorize

get_categories <- function(x){
  terra::ifel(
    pop_change == 0, 0,
    terra::ifel(
      pop_change > 0, 1,
      terra::ifel(
        pop_change < 0, -1, pop_change
      )
    )
  )
}

pop_change_cats <- get_categories(pop_change) %>% 
  as.factor()



# plot --------------------------------------------------------------------

cols <- c(
  c4_color_accent_3,
  c4_color_set_4,
  c4_color_accent_1
)

plot <- ggplot() +
  tidyterra::geom_spatraster(
    data = pop_change_cats
  ) +
  geom_sf(
    data = ClM,
    fill = "transparent",
    color = c4_color_text_2,
    size = .5
  ) +
  scale_fill_manual(
    values = cols,
    labels = c(
      "Decline",
      "Uninhabited",
      "Growth"
    ),
    na.translate = FALSE
  ) + 
  labs(
    title = "Population change in Castilla-La Mancha",
    subtitle = "2000 - 2020",
    fill = NULL,
    caption = caption_text(
      source_text = "GHS-POP R2023A - GHS population grid multitemporal (1975-2030)",
      color_text_1 = c4_color_text_1,
      color_text_2 = c4_color_text_2
    )
  ) +
  guides(
    fill = guide_legend(
      direction = "horizontal",
      keyheight = unit(10, "pt"), 
      keywidth = unit(25, "pt"),
      label.position = "bottom",
      label.hjust = .5,
      nrow = 1,
      byrow = T,
      drop = T
    )
  ) +
  theme_my(
    font_regular     = "josefin_regular",
    font_bold        = "josefin_bold",
    font_light       = "josefin_light",
    color_text_1     = c4_color_text_1,
    color_text_2     = c4_color_text_2,
    color_background = c4_color_background,
    title_size       = 50
  ) +
  theme(
    plot.margin = margin(25, 0, 15, 0, "pt"),
    legend.position = "bottom",
    panel.grid.major = element_blank(),
    axis.text = element_blank()
  )


ratio_map <- tmaptools::get_asp_ratio(ClM, res = 320)


# Save --------------------------------------------------------------------
ggsave(
  "R/30DayChartChallenge2024/day_23/clm.png",
  plot,
  width = 1920 * ratio_map,
  height = 1920,
  units = "px",
  dpi = 320
)


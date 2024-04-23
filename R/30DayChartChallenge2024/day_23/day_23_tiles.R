# 30DayChartChallenge2024 / day_23_tiles

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


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_23 <- cfg$day_23



# Data --------------------------------------------------------------------
urls <- c(
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2000_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2000_GLOBE_R2023A_4326_30ss_V1_0.zip",
  "https://jeodpp.jrc.ec.europa.eu/ftp/jrc-opendata/GHSL/GHS_POP_GLOBE_R2023A/GHS_POP_E2020_GLOBE_R2023A_4326_30ss/V1-0/GHS_POP_E2020_GLOBE_R2023A_4326_30ss_V1_0.zip"
)

options(timeout = 600)

create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
}
path <- "R/30DayChartChallenge2024/day_23/ghsl"
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

pop_rasters <- lapply(
  file_names,
  terra::rast
)

# Download Spain 
spain <- giscoR::gisco_get_countries(
  country = "ES",
  resolution = "1"
) 

# Set x and y limits for the plot, then make the points an sf object,
# set the crs as the same for spain
ylims <- c(34.11933311110609, 44.20841560392998)
xlims <- c(-13.109840681271763, 6.313986821597108)
box_coords <- tibble(x = xlims, y = ylims) %>% 
  st_as_sf(coords = c("x", "y")) %>% 
  st_set_crs(st_crs(spain))

#get the bounding box of the two x & y coordintates, make sfc
bounding_box <- st_bbox(box_coords) %>% st_as_sfc()
spain <- st_intersection(spain, bounding_box)


# Corp to country borders
spain_pop_rasters <- lapply(
  pop_rasters,
  function(x) {
    terra::crop(
      x,
      terra::vect(spain),
      snap = "in",
      mask = T
    )
  }
)

# 5. CALCULATE POPULATION DIFFERENCE
#-----------------------------------

pop_change <- (
  spain_pop_rasters[[2]] - spain_pop_rasters[[1]]
) %>%  terra::project("EPSG:4326")

# 6. CATEGORIES
#--------------

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

# 7. MAP
#-------

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
    data = spain,
    fill = "transparent",
    color = "grey40",
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
    title = "Population change in Peninsular Spain",
    subtitle = "2000 - 2020",
    caption = caption_text(
      source_text  = cfg_day_23$source,
      day_type     = cfg_day_23$category,
      day_hashtag  = cfg_day_23$theme,
      day          = cfg_day_23$day, 
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
  # coord_sf(crs = crs_lambert) +
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
    legend.position = "bottom"
  )


ratio_map <- tmaptools::get_asp_ratio(spain, res = 320)


# Save --------------------------------------------------------------------
ggsave(
  "R/30DayChartChallenge2024/day_23/day_23_tiles.png",
  plot,
  width = 1920 * ratio_map,
  height = 1920,
  units = "px",
  dpi = 320
)

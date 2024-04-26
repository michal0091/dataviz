# 30DayChartChallenge2024 / day_25_GlobalChange

# Libraries ---------------------------------------------------------------
library(RCurl)
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
library(ggOceanMaps)
library(ggspatial)
library(magick)
library(sf)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_25 <- cfg$day_25


# Data -------------------------------------------------------------------
create_dir <- function(dir) {
  if (!dir.exists(dir)) {
    dir.create(dir)
  }
}

path <- "R/30DayChartChallenge2024/day_25/data"
plot_path <- "R/30DayChartChallenge2024/day_25/plots"
create_dir(path)
create_dir(plot_path)



need_download <- FALSE
if (need_download) {
  ice_dates = c(
    "01_Jan",
    "02_Feb",
    "03_Mar",
    "04_Apr",
    "05_May",
    "06_Jun",
    "07_Jul",
    "08_Aug",
    "09_Sep",
    "10_Oct",
    "11_Nov",
    "12_Dec"
  )
  
  #Arctic
  sea_url = "ftp://sidads.colorado.edu/DATASETS/NOAA/G02135/north/monthly/shapefiles/shp_extent/"
  
  
  
  #Load all years of data, for each month
  for (i in 1:12) {
    sea_url_single = glue::glue("{sea_url}{ice_dates[i]}/")
    filenames = getURL(sea_url_single,
                       ftp.use.epsv = FALSE,
                       dirlistonly = TRUE)
    filenames = strsplit(filenames, "\n")
    filenames = unlist(filenames)
    filenames = gsub("\r", "", filenames)
    filenames = filenames[!filenames %in% c(".", "..")]
    
    for (filename in filenames) {
      download.file(paste(sea_url_single, filename, sep = ""),
                    paste(path, "/", filename, sep = ""))
    }
  }
  
}


# Base map
base  <- ggOceanMaps::basemap_data(limits = 30)
base <- base[["shapefiles"]][["land"]] %>% st_as_sf()
base <- st_transform(base, 3411)

ratio_map <- tmaptools::get_asp_ratio(base, res = 320)

# plot function
generate_sea_obj = function(path, plot_path, yearval, mon_val) {
  month <-
    c("Jan",
      "Feb",
      "Mar",
      "Apr",
      "May",
      "Jun",
      "Jul",
      "Aug",
      "Sep",
      "Oct",
      "Nov",
      "Dec")
  
  sea_ice_temp = unzip(sprintf(
    "%s/extent_N_%d%02d_polygon_v3.0.zip",
    path,
    yearval,
    mon_val
  ),
  exdir = path)
  temp_address = sprintf("%s/extent_N_%d%02d_polygon_v3.0.shp",
                         path,
                         yearval,
                         mon_val)
  temp_address = sprintf("extent_N_%d%02d_polygon_v3.0.shp", yearval, mon_val)
  ice_layer  = sf::st_read(
    dsn = path,
    layer = gsub("\\.shp", "", temp_address),
    crs = 3411,
    quiet = TRUE
  )
  p <- ggplot() +
    geom_sf(data = ice_layer,
            fill = "#ae72d8",
            color = "#a730a6") +
    geom_sf(data = base,
            fill = "grey80",
            color = "grey40") +
    labs(
      title = sprintf("Sea Ice Extent in %s %d", month[mon_val], yearval),
      x = "",
      y = "",
      caption = caption_text(
        source_text  = cfg_day_25$source,
        day_type     = cfg_day_25$category,
        day_hashtag  = cfg_day_25$theme,
        day          = cfg_day_25$day,
        color_text_1 = "#e54159",
        color_text_2 = "#2d3564"
      )
    ) +
    coord_sf(crs = 3411) +
    theme_void() +
    theme(
      plot.background = element_rect(fill = "#fbfdfe", color = NA),
      plot.margin = margin(25, 0, 15, 0, "pt"),
      plot.title = element_text(
        size = 36,
        family = "roboto_bold",
        color = "#2d3564",
        hjust = 0,
        vjust = 4,
        lineheight = .34,
        margin = margin(0, 0, 0, 0, "pt")
      ),
      plot.caption =  element_textbox_simple(
        size = 11,
        lineheight = .5,
        padding = margin(2, 2, 2, 2, "pt"),
        margin = margin(5, 0, 0, 0, "pt"),
      )
    )
  
  ggsave(
    paste0(
      plot_path,
      "/plot_",
      yearval,
      "_",
      fifelse(
        nchar(mon_val) == 1,
        paste0("0", mon_val),
        as.character(mon_val)
      ),
      ".png"
    ),
    p,
    width = 860 * ratio_map,
    height = 860,
    units = "px",
    background = "white",
    dpi = 320
  )
  
}


yearvals = 1980:2023
mon_vals = 1:12

for (yearval in yearvals) {
  for (mon_val in mon_vals) {
    if (yearval == 1987 & mon_val == 12)
      next
    if (yearval == 1988 & mon_val == 1)
      next
    generate_sea_obj(path, plot_path, yearval, mon_val)
  }
}


filenames <- list.files(plot_path, full.names = TRUE)
paths <- data.table(path = filenames)
# extract numbers from the filenames
paths[, y := as.integer(stringr::str_extract(path, "\\d+"))]
paths[, m := as.integer(stringr::str_extract(str_sub(string = path, 37, 39), "\\d+"))]
paths <- paths[order(y, m)]

m <- magick::image_read(paths[1, path])
for (i in 2:nrow(paths)) {
  m <- c(m, magick::image_read(paths[i, path]))
}

m <-
  magick::image_animate(m,
                        fps = 10,
                        dispose = "previous",
                        optimize = TRUE)
magick::image_write(m,
                    "30DayChartChallenge2024/day_25/day_25_GlobalChange_20fps.gif",
                    quality = 100)

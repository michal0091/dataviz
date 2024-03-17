# Header ------------------------------------------------------------------
#
# Author: Michal Kinel
# Copyright (c) Michal Kinel, 2024
# Email:  michal.kinel@gmail.com
# 
# Date: 2024-03-16
#
# Script Name: 2024_w11_rainfall_retiro.R
#
# Script Description:
#
#
# Notes:
#
#

# Set options -------------------------------------------------------------
cat("Setting options... \n\n", sep = "")
options(scipen = 999) # turns off scientific notation
options(encoding = "UTF-8") # sets string encoding to UTF-8 instead of ANSI


# Install packages & load libraries ---------------------------------------
cat("Install packages & load libraries... \n\n", sep = "")
packages <- c("tidyverse", "data.table", "showtext", "sysfonts", "patchwork",
              "emojifont") # list of packages to load
n_packages <- length(packages) # count how many packages are required

new_pkg <- packages[!(packages %in% installed.packages())] # determine which packages aren't installed

# Install missing packages
if(length(new_pkg)){
  install.packages(new_pkg)
}

# Load all requried libraries
for(n in 1:n_packages){
  cat("Loading Library #", n, " of ", n_packages, "... Currently Loading: ", packages[n], "\n", sep = "")
  lib_load <- paste("library(\"",packages[n],"\")", sep = "") # create string of text for loading each library
  eval(parse(text = lib_load)) # evaluate the string to load the library
}


# Load data ---------------------------------------------------------------
cat("Load data... \n\n", sep = "")
rainfall <- fread("R/2024/week_11/rainfall_retiro.csv", dec = ",")

rainfall[, year := nafill(year, "locf")]

# Subset 1923-2023
rainfall <- rainfall[year >= 1923]

# Styles ------------------------------------------------------------------
cat("Styles... \n\n", sep = "")

# Color palette
color_background <- "#fcfbf9"
color_font <- "#252b35"
color_accent_1 <- "#79253B"
color_accent_2 <- "#3B7925"
color_gradient_low <- "#253b79"
color_gradient_high <- "#94cfcb"

# Fonts
font_add_google(name = "Lato")
font_base <- "Lato"
showtext_auto()


# Plot --------------------------------------------------------------------
cat("Plot... \n\n", sep = "")

plot_1 <- rainfall[, ggplot(.SD, aes(
  x = reorder(factor(month), -month),
  y = mm,
  color = year
)) +
  geom_jitter(
    position = position_jitter(seed = 1991, width = 0.2),
    size = 2,
    alpha = .35, 
    stroke = 0
  ) +
  stat_summary(
    data = .SD[year <= 1955],
    fun = mean,
    geom = "point",
    aes(shape = "≤ 1955"),
    size = 2,
    color = color_accent_2
  ) +
  stat_summary(
    data = .SD[year > 1990],
    fun = mean,
    geom = "point",
    aes(shape = "> 1990"),
    size = 3,
    color = color_accent_1
  ) +
  scale_shape_manual("Mean", values =  c("≤ 1955" = 5, "> 1990" = 18)) +
  coord_flip() +
  scale_y_continuous(limits = c(0, 200), expand = c(0.08, 0)) +
  scale_colour_steps(low = color_gradient_low, high = color_gradient_high) +
  labs(
    title = "Monthly rainfall",
    x = "Month",
    y = "Rainfall (mm)",
    color = NULL
  ) +
  theme_void() +
  theme(
    text = element_text(
      family = font_base,
      color = color_font,
      size = 14
    ),
    plot.margin = margin(1, 1, 0, 1, "cm"),
    plot.background = element_rect(fill = color_background, color  = NA),
    legend.position = "bottom",
    legend.key.width = unit(.6, "cm"),
    legend.key.height = unit(.15, "cm"),
    axis.text = element_text(
      color = color_font,
      family = font_base,
      size = 18
    ),
    axis.title = element_text(
      color = color_font,
      family = font_base,
      size = 18
    ),
    axis.title.y = element_text(
      color = color_font,
      family = font_base,
      size = 18,
      angle = 90
    ),
    plot.title = element_text(
      hjust = 0,
      vjust = 2,
      color = color_font,
      family = font_base,
      size = 22,
      face = "bold"
    )
  )]

sum_year <- rainfall[, .(sum_year = sum(mm, na.rm = TRUE)), year]
sum_year[, rolling_mean_10y := frollmean(sum_year, 10, align = "right", fill = NA)]
sum_year[, rolling_sd_10y := frollapply(sum_year, 10, sd, align = "right", fill = NA)]

plot_2 <-
  sum_year[, ggplot(.SD, aes(year, sum_year, color = "Yearly rainfall")) +
             geom_line() +
             geom_line(aes(year, rolling_mean_10y, color = "10y roll mean")) +
             geom_ribbon(
               aes(
                 y = sum_year,
                 ymin = rolling_mean_10y - rolling_sd_10y,
                 ymax = rolling_mean_10y + rolling_sd_10y,
                 fill = "± sd"
               ),
               alpha = .2,
               color = NA
             ) +
             scale_x_continuous(
               breaks = seq(1923, 2023, 20),
               limits = c(1923, 2023),
               expand = c(.02, .02)
             ) +
             scale_y_continuous(limits = c(0, 800),
                                breaks = seq(0, 800, 100)) +
             scale_color_manual(values = c("Yearly rainfall" = color_gradient_low,
                                           "10y roll mean" = color_gradient_high)) +
             scale_fill_manual(values = c("± sd" = color_gradient_high)) +
             labs(
               title = "Yearly rainfall",
               x = "Year",
               y = "Rainfall (mm)",
               color = NULL,
               fill = NULL
             ) +
             theme_void() +
             theme(
               text = element_text(
                 family = font_base,
                 color = color_font,
                 size = 16
               ),
               plot.margin = margin(1, 1, 0, 1, "cm"),
               plot.background = element_rect(fill = color_background, color  = NA),
               legend.position = "bottom",
               axis.text = element_text(
                 color = color_font,
                 family = font_base,
                 size = 18
               ),
               axis.title = element_text(
                 color = color_font,
                 family = font_base,
                 size = 18
               ),
               axis.title.y = element_text(
                 color = color_font,
                 family = font_base,
                 size = 18,
                 angle = 90
               ),
               plot.title = element_text(
                 hjust = 0,
                 vjust = 2,
                 color = color_font,
                 family = font_base,
                 size = 22,
                 face = "bold"
               ),
               panel.grid.major = element_line(
                 colour = color_font,
                 linetype = "dotted",
                 linewidth = .3
               )
             )]


# Caption -----------------------------------------------------------------
cat("Caption... \n\n", sep = "")
source_text <- "AEMET"
highlight_color <- color_gradient_low
text_color <- color_font
github_icon <- '&#xf113'
github_username <- "michal0091"
twitter_icon <- "&#xf081"
twitter_username <- "nico_kinel"
linkedin_icon <- "&#xf08c"
linkedin_username <- "michal-kinel"
mastodon_icon <- "&#xf4f6"
mastodon_username <- "miki_peltzer"
mastodon_server <- "techhub.social"
social_caption <- glue::glue(
  "<span style='color: {text_color}'>Source: {source_text}</span><br>
  <span style='font-family:\"fa-brands\";
                color: {highlight_color}'>{github_icon};</span>
  <span style='color: {text_color}'>{github_username}</span>
  <span style='font-family:\"fa-brands\";
                color: {highlight_color}'>{twitter_icon};</span>
  <span style='color: {text_color}'>{twitter_username}</span>
  <span style='font-family:\"fa-brands\";
                color: {highlight_color}'>{linkedin_icon};</span>
  <span style='color: {text_color}'>{linkedin_username}</span>
  <span style='font-family:\"fa-brands\";
                color: {highlight_color}'>{mastodon_icon};</span>
  <span style='color: {text_color}'>{mastodon_username}@<span><span style='color: {text_color}'>{mastodon_server}</span>"
)


# Combine plots -----------------------------------------------------------
cat("Combine plots... \n\n", sep = "")

combined_plot <-
  plot_1 + plot_2 +
  plot_layout(widths = c(1, 2)) +
  plot_annotation(
    title = "Rainfall in Retiro, Madrid",
    subtitle = "Monthly and yearly rainfall between 1923 and 2023",
    caption = social_caption,
    theme = theme(
      plot.background = element_rect(fill = color_background, color = NA),
      plot.title = element_text(
        hjust = 0,
        vjust = 0,
        color = color_font,
        family = font_base,
        size = 26,
        face = "bold"
      ),
      plot.subtitle = element_text(
        hjust = 0,
        vjust = 1,
        color = color_font,
        family = font_base,
        size = 22
      ),
      plot.caption = element_markdown(
        hjust = 0,
        halign = 0,
        family = font_base,
        size = 14,
        color = color_font
      )
    )
  )

# Save plot ---------------------------------------------------------------
cat("Save plot... \n\n", sep = "")

ggsave(
  filename = "rainfall_retiro.png",
  path = normalizePath("R/2024/week_11/"),
  plot = combined_plot,
  device = "png",
  units = "cm",
  width = 21,
  height = 13
)


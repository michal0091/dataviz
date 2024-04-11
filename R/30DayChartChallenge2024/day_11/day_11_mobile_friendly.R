# 30DayChartChallenge2024 / day_11_mobile_friendly

# Libraries ---------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggforce)
library(ggtext)
library(emojifont)
library(sysfonts)
library(showtext)
library(patchwork)
library(pbapply)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_11 <- cfg$day_11



# Functions ---------------------------------------------------------------
# Define the Collatz operator function
collatz <- function(n) {
  if (n %% 2 == 0) return(n / 2)  # Even: divide by 2
  return(3 * n + 1)               # Odd: 3n + 1
}

# Define the Collatz orbit function
collatz_orbit <- function(n) {
  orbit <- c()                    # Initialize empty orbit vector
  while (n > 1) {
    orbit <- c(orbit, n)          # Append n to the orbit
    n <- collatz(n)               # Apply Collatz rule to get new n
  }
  return(c(orbit, 1))             # Return orbit with the root (1)
}

# Function to determine turn angle based on even/odd value
turn_angle <- function(n, angle, twist) {
  if (n %% 2 == 0) {
    -angle # Even: clockwise turn
  } else {
    angle * twist  # Odd: anti-clockwise turn with twist factor
  }            
}

# Function to reverse a vector
reverse <-  function(x) {
  x[length(x):1]
}

# Function to convert degrees to radians
deg2rad <- function(deg) {(deg * pi) / (180)}


# Function to generate Harriss visualization data
harriss_viz_data <- function(obs, n_sample, angle = 8, twist = 1.618, alpha = c(0.01, .99), lw = c(.05, 2), pal, seed = 199101) {
  # Set seed for reproducibility
  set.seed(seed)
  
  # Sample n_sample numbers from 1 to n_sample
  smp <- sample(1:n_sample, obs, replace = FALSE)
  if (length(pal) != 1) {
    pal_cols <- colorRampPalette(pal)(obs)
  }
  
  pblapply(1:obs, function(x) {
    n <- smp[x]
    
    # Reverse the orbit (convention: 1 is root)
    orbit <- reverse(collatz_orbit(n))
    
    # Initialize variables
    prev_x <- 0
    prev_y <- 0
    heading <- 0
    
    seq_orbit <- seq_along(orbit)
    
    dt <- lapply(seq_orbit, function(i) {
      o <- orbit[i]
      
      # Update heading based on turn angle
      heading <<- heading + turn_angle(o, angle, twist)
      
      # Calculate new coordinates
      new_x <- prev_x + (cos(deg2rad(heading)))
      new_y <- prev_y + (sin(deg2rad(heading)))
      
      # Plot the line segment with adjusted width and alpha
      data.table(
        seq = seq_orbit,
        n = n,
        x = c(prev_x, new_x),
        y = c(prev_y, new_y)
      ) -> dt_aux
    
      
      # Update previous coordinates
      prev_x <<- new_x
      prev_y <<- new_y
      
      dt_aux
    }) %>% rbindlist()
    
   if (length(pal) == 1) {
      dt[,  `:=`(
        col = pal,
        lwd = scales::rescale(1 / seq_orbit, to = lw),
        alpha = scales::rescale(1 / seq_orbit, to = alpha))]
    } else {
      color <- pal_cols[x]
      dt[,  `:=`(
        col = color,
        lwd = scales::rescale(1 / (1/.N), to = lw),
        alpha = scales::rescale(1 / (1/.N), to = alpha))]
    }
    
    dt
  }) %>% rbindlist()

}

my_obs <- 5000
my_twist <- 2
even <- 5
odd <- even * my_twist

data <- harriss_viz_data(obs = my_obs, n_sample = 1000000, angle = even, twist = my_twist, lw = c(.01, .5), pal = viridis::magma(20))  


# Plot --------------------------------------------------------------------
plot <-
  data[, ggplot(.SD, aes(
    x,
    y, 
    group = n
  )) +
    geom_path(size = lwd, color = col, alpha = alpha) +
    coord_fixed() +
    labs(
      title = "The Collatz Conjecture",
      subtitle = paste0("Harriss-like visualisations of ", formatC(my_obs, big.mark = ",")," orbits\n",
                        "for a set of ", formatC(my_obs, big.mark = ",")," initial values\n",
                        "chosen at random from\n",
                        "the initial values up to 1,000,000\n",
                        "Even: ", even, "° ", "Odd: ", odd, "° angles"),
      x = NULL,
      y = NULL,
      caption = caption_text(
        source_text = cfg_day_11$source,
        day_type =  cfg_day_11$category,
        day_hashtag = cfg_day_11$theme,
        day = cfg_day_11$day,
        color_text_1 = c2_color_text_1,
        color_text_2 = c2_color_text_2
      )
    ) +
    scale_x_continuous(guide = "none") +
    scale_y_continuous(guide = "none") +
    theme_my(
      font_regular     = "roboto_regular",
      font_bold        = "roboto_bold",
      font_light       = "roboto_light",
      color_text_1     = c2_color_text_1,
      color_text_2     = c2_color_text_2,
      color_background = c2_color_background,
      title_size       = 36 
    ) +
    theme( 
      plot.margin = margin(25, 0, 10, 10, "pt"),
      panel.grid.major = element_blank(),
      plot.caption =  element_textbox_simple(
        size = 14,
        lineheight = .25,
        padding = margin(10, 10, 10, 0, "pt"),
        margin = margin(10, 10, 10, 0, "pt"),
      )
    )]


# Save plot ---------------------------------------------------------------
ggsave(
  filename = "day_11_mobile_friendly.png",
  path = normalizePath("R/30DayChartChallenge2024/day_11"),
  plot = plot,
  device = "png",
  units = "px",
  width = 1080,
  height = 1920,
  dpi = 280
)

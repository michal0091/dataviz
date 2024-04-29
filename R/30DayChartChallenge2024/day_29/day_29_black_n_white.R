# 30DayChartChallenge2024 / day_29_black_n_white

# Libraries ---------------------------------------------------------------
library(data.table)
library(tidyverse)
library(primes)
library(f())


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_29 <- cfg$day_29


# Data --------------------------------------------------------------------

# Prime numbers
set.seed(16011991)
n <- 365
r <- 2000

random <- function(n) {
  if (is_prime(n)) return(n / 2)
  return(2 * n - 1)
}
dt <- data.table(x = rep(1:n, r),
                 y = as.vector(replicate(r, cumsum(
                   sapply(sample(1:30, n, TRUE), random) * sample(c(-1, 1), n, TRUE)
                 ))),
                 id = rep(1:r, each = n))

# Plot --------------------------------------------------------------------

plot <- ggplot(dt, aes(x = x, y = y, group = id)) +
  geom_line(color = "white", alpha = 0.03) +
  scale_x_continuous(expand = c(0, 0)) +
  labs(
    title = "Random walk simulation",
    subtitle = "2.000 random walks using prime numbers as a rule\nif n is prime, n/2, otherwise 2n-1",
    caption  = caption_text(
      source_text  = cfg_day_29$source,
      day_type     = cfg_day_29$category,
      day_hashtag  = cfg_day_29$theme,
      day          = cfg_day_29$day, 
      color_text_1 = "white", 
      color_text_2 = "white"
    )
  ) +
  theme_void() +
  theme(
    plot.margin = margin(35, 0, 0, 0, "pt"),
    plot.background = element_rect(fill = "black"),
    panel.background = element_rect(fill = "black"),
    plot.title = element_text(
      size = 56,
      family = "roboto_bold",
      color = "white",
      face = "bold",
      hjust = 0,
      vjust = 4.5,
      lineheight = .34,
      margin = margin(5, 5, 5, 35, "pt")
    ),
    plot.subtitle = element_text(
      size = 42,
      family = "roboto_bold",
      color = "white",
      hjust = 0,
      vjust = 3,
      lineheight = .3,
      margin = margin(5, 5, 5, 35, "pt")
    ),
    plot.caption =  element_textbox_simple(
      size = 26,
      lineheight = .5,
      padding =  margin(3, 3, 3, 3, "pt"),
      margin =  margin(15, 10, 15, 35, "pt"),
    ),
    plot.title.position = "plot",
    plot.caption.position = "plot"
  )



# Save --------------------------------------------------------------------
ggsave(
  "R/30DayChartChallenge2024/day_29/day_29_black_n_white.png",
  plot,
  width = 1920,
  height = 1920,
  units = "px",
  dpi = 320, limitsize = F
)

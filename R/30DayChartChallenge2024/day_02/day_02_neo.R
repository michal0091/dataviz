# 30DayChartChallenge2024 / day_02_neo

# Libraries ---------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggtext)
library(emojifont)
library(sysfonts)
library(showtext)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")

# Config ------------------------------------------------------------------
cfg <- yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_2 <- cfg$day_2 


# Data --------------------------------------------------------------------
death_rates_energy <- fread("R/30DayChartChallenge2024/day_02/death-rates-from-energy-production-per-twh.csv")
death_rates_energy[, label := paste0(Entity, ": ", `Deaths per TWh of electricity production`)]
label_length <- death_rates_energy[, max(nchar(as.character(label)))]
death_rates_energy[, label := paste0(label, strrep(" ", label_length - nchar(as.character(label))))]
death_rates_energy[, Entity := factor(Entity, levels = Entity[order(`Deaths per TWh of electricity production`)])]


# Plot --------------------------------------------------------------------
plot <-  ggplot(death_rates_energy,
       aes(x = Entity, xend = Entity,
           y = 0, yend = `Deaths per TWh of electricity production`)) +
  geom_link(size = 4, lineend = "round", color = color_set_2) +
  geom_link(size = 3, lineend = "round", color = color_set_1) +
  geom_text(aes(label = label),
            hjust = -.2, family = "inter_regular", color = color_text_1, size = rel(6)) +
  scale_y_continuous(limits = c(0, 40)) +
  coord_radial(theta = "y", -1.09 * pi, end =  0.9 * pi,
               inner.radius = .15) + 
  labs(title = "Death rates per unit of electricity production",
       subtitle = "Death rates are measured based on deaths from\naccidents and air pollution per terawatt-hour of electricity",
       caption = caption_text(
         source_text = cfg_day_2$source,
         day_type =  cfg_day_2$category,
         day_hashtag = cfg_day_2$theme
       ),
       x = NULL, y = NULL) +
  theme_my(title_size = 24) +
  theme(panel.grid.major = element_blank(),
        axis.text = element_blank())

# Save --------------------------------------------------------------------
ggsave(
  filename = "day_02_neo.png",
  path = normalizePath("R/30DayChartChallenge2024/day_02"),
  plot = plot,
  device = "png",
  units = "px",
  width = 1080,
  height = 1080,
  dpi = 320
)







# 30DayChartChallenge2024 / day_02_neo

# Libraries ---------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggforce)
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
label_length <- death_rates_energy[, max(nchar(as.character(label)))] + 2
death_rates_energy[, label := paste0(label, strrep(" ", label_length - nchar(as.character(label))))]
death_rates_energy[, Entity := factor(Entity, levels = Entity[order(`Deaths per TWh of electricity production`)])]
death_rates_energy <- death_rates_energy[order(Entity)]


# Don't know why but necessary 
death_rates_energy[, label := fcase(
  Entity == "Solar", paste0("   ", label),
  Entity == "Wind", paste0("  ", label),
  Entity == "Nuclear", paste0("   ", label),
  Entity == "Hydropower", paste0(" ", label),
  Entity == "Gas", paste0("   ", label),
  Entity == "Biomass", paste0(" ", label),
  Entity == "Oil", paste0("   ", label),
  Entity == "Coal", paste0("  ", label),
  Entity == "Brown coal", label
)]



# Plot --------------------------------------------------------------------
plot <- ggplot(
  death_rates_energy,
  aes(
    x = Entity,
    xend = Entity,
    y = 0,
    yend = `Deaths per TWh of electricity production`
  )
) +
  geom_text(
    aes(label = label),
    hjust = -.2,
    family = "inter_bold",
    color = color_text_1,
    size = rel(5),
  ) +
  geom_link(size = 3.5,
            lineend = "round",
            color = color_set_2) +
  geom_link(size = 2.2,
            lineend = "round",
            color = color_set_1) +
  scale_y_continuous(limits = c(0, 40)) +
  coord_radial(theta = "y",
               -1.09 * pi,
               end =  0.9 * pi,
               inner.radius = .15) +
  geom_richtext(aes(I(.12), I(0), label = "<span style='font-family:fa-solid;'>&#xf0e7;</span>"),
                size = 24,
                label.colour = NA,
                fill = NA,
                col = color_set_2, 
                inherit.aes = F) +
  geom_richtext(aes(I(0), I(0), label = "<span style='font-family:fa-solid;'>&#xf54c;</span>"),
                size = 20,
                label.colour = NA,
                fill = NA,
                col = color_set_1, 
                inherit.aes = F) +
  labs(
    title = "Death rates per unit of electricity production",
    subtitle = "Death rates are measured based on deaths from\naccidents and air pollution per terawatt-hour of electricity",
    caption = caption_text(
      source_text = cfg_day_2$source,
      day_type =  cfg_day_2$category,
      day_hashtag = cfg_day_2$theme,
      day = cfg_day_2$day
    ),
    x = NULL,
    y = NULL
  ) +
  theme_my(title_size = 24) +
  theme(
    panel.grid.major = element_blank(),
    plot.margin = margin(1, 0, 0, 0, "cm"),
    axis.text = element_blank(),
    plot.subtitle = element_text(lineheight = .25, vjust = 1),
    plot.caption =  element_textbox_simple(
      lineheight = .5,
      padding = margin(.1, .1, .1, .1, "lines"),
      margin = margin(0, 0, 1, 0, "lines"),
    )
  ) 

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







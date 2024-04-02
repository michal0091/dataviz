# 30DayChartChallenge2024 / day_3_makeover

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
cfg_day_3 <- cfg$day_3


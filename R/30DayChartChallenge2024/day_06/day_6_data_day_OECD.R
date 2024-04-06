# 30DayChartChallenge2024 / day_6_data_day_OECD

# Libraries ---------------------------------------------------------------
library(data.table)
library(ggplot2)
library(ggforce)
library(ggtext)
library(emojifont)
library(sysfonts)
library(showtext)
library(patchwork)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_6 <- cfg$day_6


# Data --------------------------------------------------------------------
dt <- fread("R/30DayChartChallenge2024/day_06/OECD_AV_AN_WAGE.csv")

dt_sub <- dt[TIME_PERIOD %in% c(2020, 2022), .(REF_AREA, `Reference area`, TIME_PERIOD, OBS_VALUE)]




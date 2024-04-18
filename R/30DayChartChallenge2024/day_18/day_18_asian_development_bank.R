# 30DayChartChallenge2024 / day_18_asian_development_bank

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
library(geomnet)
library(ggnetwork)
library(igraph)


# Source ------------------------------------------------------------------
source("R/30DayChartChallenge2024/theme/styles.R")
source("R/30DayChartChallenge2024/theme/theme.R")


# Config ------------------------------------------------------------------
cfg <-
  yaml::read_yaml("R/30DayChartChallenge2024/cfg/plots.yml")[["default"]]
cfg_day_18 <- cfg$day_18


# Data --------------------------------------------------------------------
exch <- fread("R/30DayChartChallenge2024/day_18/exchange.csv", dec = ",")
library(readxl)
KIDB <- read_excel("R/30DayChartChallenge2024/day_18/KIDB.xlsx") %>% 
  as.data.table()

KIDB <- KIDB[Economy %in% exch[, country]]
KIDB <- KIDB[, .(int_tourism_receipts_pc = 1000 * `2022`[Indicator == "International Tourism Receipts ($ million)"] / `2022`[Indicator == "Total population"],
         gdp_pc_lp = `2022`[Indicator == "GDP at current prices"] / `2022`[Indicator == "Total population"]), by = .(country = Economy)]
KIDB <- na.omit(KIDB)
KIDB <- merge(KIDB, exch, by = "country")
KIDB[, gdp_pc_usd := gdp_pc_lp / exchange_rate]


# Plot --------------------------------------------------------------------

plot <- KIDB[, ggplot(.SD, aes(x = gdp_pc_usd, y = int_tourism_receipts_pc)) +
        geom_point(aes(color = region), size = 3) +
        geom_text_repel(aes(label = country), size = rel(8), 
                        box.padding = 1, point.padding = 1,
                        max.overlaps = 12,
                        family = "redhat_regular") +
        scale_x_log10(expand = c(0.1, 0.1),
                      labels = scales::label_dollar(accuracy = 1000L)) +
        scale_y_log10(expand = c(0.1, 0.5),
                      
                      labels = scales::label_dollar(accuracy = 1000L)) +
        scale_color_manual(values = c("#d99058", 
                                      "#02517a",
                                      "#9556eb",
                                      "#c0dccd")) +
        labs(
          title = "International Tourism Receipts vs\nGDP per capita",
          subtitle = "Asian Development Bank 2022",
          x = "GDP per capita (USD)",
          y = "International Tourism Receipts per capita (USD thousand)",
          color = NULL,
          caption = caption_text(
            source_text = cfg_day_18$source,
            day_type =  cfg_day_18$category,
            day_hashtag = cfg_day_18$theme,
            day = cfg_day_18$day, 
            color_text_1 = c3_color_text_1, 
            color_text_2 = c3_color_text_2
          )
        )  +
       theme_my(
         font_regular     = "redhat_regular",
         font_bold        = "redhat_bold",
         font_light       = "redhat_light",
         color_text_1     = c3_color_text_1,
         color_text_2     = c3_color_text_2,
         color_background = c3_color_background,
         title_size       = 50
       ) +
         theme(
           plot.margin = margin(20, 60, 10, 20, "pt"),
           plot.caption =  element_textbox_simple(
             size = 20,
             lineheight = .5,
             padding = margin(4, 0, 4, 0, "pt"),
             margin = margin(20, 0, 0, 0, "pt"),
           ),
           legend.position = "bottom",
           legend.spacing = unit(2.5, "pt"),
           legend.key.spacing = unit(2.5, "pt"),
           legend.key.height = unit(8, 'pt'),
           legend.key.width = unit(12.944, 'pt'),
           legend.title = element_text(hjust = 0),
           panel.grid.major = element_line(
             colour = c3_color_text_2,
             linetype = "dashed",
             linewidth = .3
           ),
           axis.text = element_text(
             size = rel(1.8),
             family = "redhat_regular",
             color = c3_color_text_2,
             margin = margin(2, 2, 2, 0, "pt")
           )
         )]


# Save plot ---------------------------------------------------------------
ggsave(
  filename = "R/30DayChartChallenge2024/day_18/day_18_asian_development_bank.png",
  plot = plot,
  width = 1920,
  height = 1920,
  units = "px",
  dpi = 320
)




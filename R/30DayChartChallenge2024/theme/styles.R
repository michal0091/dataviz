# libs -----------------------------------------------------------------------
library(sysfonts)
library(showtext)
library(ggplot2)


# Colors ------------------------------------------------------------------
color_background <- "#fbfdfe"
color_text_1   <- "#352725"
color_text_2   <- "#3f68e3"
color_set_1 <- "#2a668a"
color_set_2 <- "#f9ad0d"
color_set_3 <- "#67deb0" 


c2_color_background <- "#0e0204"
c2_color_text_1   <- "#f1f2f4"
c2_color_text_2   <- "#d2f796"
c2_color_accent_1   <- "#00f1bc"
c2_color_accent_2   <- "#fe686c"
c2_color_set_1 <- "#992696"
c2_color_set_2 <- "#ba3339"
c2_color_set_3 <- "#636567" 



c3_color_background <- "#1A7070"
c3_color_text_1   <- "#FFE44D"
c3_color_text_2   <- "#DEEDF9"
c3_color_accent_1   <- "#d6762c"
c3_color_accent_2   <- "#451a70"
c3_color_set_1 <- "#869b97"
c3_color_set_2 <- "#ff9a9b"
c3_color_set_3 <- "#44bcc6" 



c4_color_background <- "#cddfe0"
c4_color_text_1 <- "#0e130b"
c4_color_text_2 <- "#103c43"


c4_color_set_1 <- "#db4726"
c4_color_set_2 <- "#4170ab"
c4_color_set_3 <- "#f4c14c"
c4_color_set_4 <- "#e7c2ba"

c4_col_set <- c(
  "#387c2b", "#fa6400",
  "#ffa100", "#949494", 
  "#0099c4", "#6e3ab7"
)


c4_color_accent_1 <- "#18984b"
c4_color_accent_2 <- "#e6c51b"
c4_color_accent_3 <- "#ed0c29"




#  ** pallettes ------------------------------------------------------------
gradient <-
  c(color_set_1,
    "#0089a4",
    "#00aba9",
    "#40cb9b",
    "#9ce582",
    "#f9f871")

main <-
  c(color_set_1,
    "#6399c0",
    color_set_2,
    "#b6ab00",
    color_set_3,
    "#384b42")



#  ** aux colors -----------------------------------------------------------
white       <- "#F0F0F0"
light_gray  <- "#fcfcfd"
medium_gray <- "#d2d2d2"
dark_gray   <- "#3c3c3c"



# Fonts -------------------------------------------------------------------
font_add_google(name = "Inter", family = "inter_regular", regular.wt = 400, bold.wt = 700)
font_add_google(name = "Inter", family = "inter_bold", regular.wt = 700, bold.wt = 900)
font_add_google(name = "Inter", family = "inter_light", regular.wt = 300, bold.wt = 400)
font_add_google(name = "Inter", family = "inter_thin", regular.wt = 100, bold.wt = 200)

# Roboto
font_add_google(name = "Roboto", family = "roboto_regular", regular.wt = 400, bold.wt = 700)
font_add_google(name = "Roboto", family = "roboto_bold", regular.wt = 700, bold.wt = 900)
font_add_google(name = "Roboto", family = "roboto_light", regular.wt = 300, bold.wt = 400)
font_add_google(name = "Roboto", family = "roboto_thin", regular.wt = 100, bold.wt = 200)

# Red Hat Mono
font_add_google(name = "Red Hat Mono", family = "redhat_regular", regular.wt = 500, bold.wt = 600)
font_add_google(name = "Red Hat Mono", family = "redhat_bold", regular.wt = 700, bold.wt = 900)
font_add_google(name = "Red Hat Mono", family = "redhat_light", regular.wt = 400, bold.wt = 500)

# Josefin Sans
font_add_google(name = "Josefin Sans", family = "josefin_regular", regular.wt = 400, bold.wt = 700)
font_add_google(name = "Josefin Sans", family = "josefin_bold", regular.wt = 700, bold.wt = 900)
font_add_google(name = "Josefin Sans", family = "josefin_light", regular.wt = 300, bold.wt = 400)
font_add_google(name = "Josefin Sans", family = "josefin_thin", regular.wt = 100, bold.wt = 200)




# Font Awesome
font_add(family = "fa-brands", regular = "R/30DayChartChallenge2024/theme/fa-brands-400.ttf")
font_add(family = "fa-solid", regular = "R/30DayChartChallenge2024/theme/fa-solid-900.ttf")
showtext_auto()


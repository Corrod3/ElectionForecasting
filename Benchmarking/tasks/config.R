# data wrangling
library(dplyr)
#library(rvest)
library(purrr)
library(magrittr)
library(tidyr)
library(zoo)
library(xml2)

# graphics
library(ggplot2)
library(directlabels)
library(grid)
library(stringr)
library(lubridate)
library(ggrepel)
library(stargazer)
library(Metrics)


windowsFonts(Times=windowsFont("TT Times New Roman"))


options(scipen = 999)
farben = c("AfD" = "#009dd1","Union" = "#222222", "FDP" = "#ffb700", "Gruene" = "#349f29", "Linke" = "#cc35a0", "SPD" = "#ce1b1b")
farben_ci = c("SPD" = "#eba4a4","AfD" = "#99d8ed","Gruene" = "#aed9a9", "Union" = "#bbbbbb", "Linke" = "#ebaed9", "FDP" = "#ffe299")
plabels = c("SPD" = "SPD ", "AfD" = "AfD ","Gruene" = "Gruene ", "Union" = "Union ", "Linke" = "Linke ", "FDP" = "FDP ")
mlabels = c("SPD" = "SPD", "AfD" = "AfD","Gruene" = "Gruene", "Union" = "Union", "Linke" = "Linke", "FDP" = "FDP")

sztheme_points <- theme(
  strip.background = element_blank(),
  strip.text.y = element_blank(),
  strip.text.x = element_blank(),
  axis.text = element_text(family = "Times", size = 18),
  axis.ticks.y = element_blank(),
  axis.ticks.x = element_line(color = "#999999", size = 0.2),
  axis.ticks.length = unit(0.1, "in"),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major.x = element_blank(),
  panel.grid.major.y = element_blank(),
  panel.grid.minor.y = element_line(color = "#999999", size = 0.2),
  panel.grid.minor.x = element_blank(),
  legend.position = "none",
  plot.background = element_blank(),
  plot.margin = unit(c(0.2, 0.6, 0.1, 0.1), "lines"))

sztheme_lines <- theme(
  strip.background = element_blank(),
  strip.text.y = element_blank(),
  strip.text.x = element_blank(),
  axis.text = element_text(family = "Times", size = 14),
  axis.text.x = element_text(margin = margin(0.1,0,0,0,"in")),
  axis.line.y = element_blank(),
  axis.ticks = element_blank(),
  axis.ticks.length = unit(0,"lines"),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major.y = element_line(colour = "#eeeeee", size = 0.3),
  panel.grid.major.x = element_line(colour = "#eeeeee", size = 0.3),
  panel.spacing = unit(c(0,0,0,0), "lines"),
  plot.background = element_blank(),
  plot.margin = unit(c(0, 0.8, 0.1, 0), "in"),
  legend.margin = margin(0.1, 0, 0.1, -3.5, "in"),
  legend.background = element_blank(),
  legend.title = element_blank(),
  legend.position = "top",
  legend.direction = "vertical",
  legend.key = element_blank(),
  legend.key.size = unit(0.15, "in"),
  legend.text = element_text(family = "Times", size = 14, colour = "#666666"),
  text = element_text(size = 16, family = "Times", colour = "#666666")
)

sztheme_lines_mobile <- theme(
  plot.margin = unit(c(0,0,0,0), "lines"),
  legend.margin = margin(0, 0, 0, 0.3, "in"), 
  legend.text = element_text(family = "Times", size = 14, colour = "#666666")
)

sztheme_teaser <- theme(
  strip.background = element_blank(),
  strip.text.y = element_blank(),
  strip.text.x = element_blank(),
  axis.text = element_blank(),
  axis.line.y = element_blank(),
  axis.ticks = element_blank(),
  # axis.ticks.length = unit(0,"lines"),
  axis.title.x = element_blank(),
  axis.title.y = element_blank(),
  panel.background = element_blank(),
  panel.border = element_blank(),
  panel.grid.major.y = element_line(colour = "#eeeeee", size = 0.3),
  # panel.grid.minor.y = element_line(colour = "#eeeeee", size = 0.2),
  panel.grid.major.x =  element_blank(),
  # panel.grid.minor.x = element_line(colour = "#eeeeee", size = 0.2),
  panel.spacing = unit(c(0,0,0,0), "lines"),
  plot.background = element_blank(),
  plot.margin = unit(c(0, -0.5, 0, -1), "in"),
  legend.position = "none"
)

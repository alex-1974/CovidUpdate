library(tidyverse)
library(showtext)

theme_covid <- function(){ 
  font_add("libertinus sans", "LibertinusSans-Regular.otf")
  font_add("libertinus serif", "LibertinusSerif-Regular.otf")
  font <- "libertinus sans"   #assign font family up front
  #theme_minimal() %+replace%    #replace elements we want to change
  theme(
    plot.title = element_text(family = "libertinus serif", size = (16), 
                              margin = margin(10, 0, 5, 0)),
    plot.subtitle = element_text(family = "libertinus sans", size = (13), 
                                 margin = margin(0, 0, 5, 0)),
    plot.caption = element_text(family = "libertinus sans", size = (12)),
    # color background 
    panel.background = element_blank(),
    # modify grid 
    panel.grid.major.x = element_line(colour = "gray", linetype = 3, size = 0.5),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.y =  element_line(colour = "gray", linetype = 3, size = 0.5),
    panel.grid.minor.y = element_blank(),
    # modify text, axis and colour 
    axis.text = element_text(colour = "gray", face = "italic"),
    axis.title = element_text(colour = "black"),
    axis.ticks = element_line(colour = "gray"),
    # legend at the bottom 
    legend.position = "bottom"
  )
}

mycolors <- c(
  "orange" = rgb(230,159,0,maxColorValue = 255),
  "skyBlue" = rgb(86,180,233,maxColorValue = 255),
  "blueishGreen" = rgb(0,158,115,maxColorValue = 255),
  "yellow" = rgb(240,228,66,maxColorValue = 255),
  "blue" = rgb(0,114,178,maxColorValue = 255),
  "vermillion" = rgb(213,94,0,maxColorValue = 255),
  "reddishPurple" = rgb(204,121,167,maxColorValue = 255)
)

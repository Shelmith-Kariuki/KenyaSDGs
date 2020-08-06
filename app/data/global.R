## This script contains some universal objects that we will need in the app

## Load the libraries
library(googlesheets4)
library(tidyverse)
library(rKenyaCensus)



## Remove scientific notation
options(scipen = 9999)

# Set the theme
my_theme<-theme(legend.position = "bottom",
                legend.title = element_blank(),
                axis.line = element_line(size = 1.5),
                plot.title = element_text(family="Source Sans Pro Semibold", face = "italic",
                                          size = rel(2.0), hjust = 0.5, vjust = -0.5),
                plot.subtitle = element_text(size = rel(1.5), hjust = 0.5),
                plot.caption = element_text(size = rel(1.5)),
                axis.text = element_text(family = "Source Sans Pro Semibold", size = rel(0.9)),
                axis.text.x = element_text(vjust = 1, hjust = 0.6),
                axis.title = element_text(family = "Source Sans Pro Semibold", size = rel(1.2)),
                legend.text = element_text(family = "Source Sans Pro Semibold", size = rel(1.2)),
                panel.background = element_rect(fill = NA))

radar_theme<-theme(legend.position = "bottom",
                legend.title = element_blank(),
                plot.title = element_text(family="Source Sans Pro Semibold", face = "italic",
                                          size = rel(2.0), hjust = 0.5, vjust = -0.5),
                plot.subtitle = element_text(size = rel(1.5), hjust = 0.5),
                plot.caption = element_text(size = rel(1.5)),
                axis.text = element_text(family = "Source Sans Pro Semibold", size = rel(0.9)),
                axis.text.x = element_text(vjust = 1, hjust = 0.6),
                axis.title = element_text(family = "Source Sans Pro Semibold", size = rel(1.2)),
                legend.text = element_text(family = "Source Sans Pro Semibold", size = rel(1.2)))



## Define a list of the SDGs
sdg_file <- googlesheets4::read_sheet("https://docs.google.com/spreadsheets/d/1VQX6ts--j0ZkqNWY9x4NeH9fo-OmXSLO8VIxFOmDUno/edit#gid=1020912451",
                                      sheet = 2)
sdgs <- unique(sdg_file$SDG)

## We are going to filter the SDGs to only be left with the ones we are ready to analyse.

sdgs <- grep("9", sdgs, value = T, ignore.case = T)

## Color palettes

### sdg4
s4 <- c("#f5d4d8", "#cc1e35", "#dc546c")
s4b <- c("gray", "#90021f")
red_to_chocolate <- c("#C24352","#B32134", "#FFD1A9", "#E97451", "#FF0000", "#BF0000", "#800000", 
                      "#CF0009", "#E15B20","#F7A815", "#F0BA70", "#92000A", "black")

blues_colors <- c("#309AF1", "#66BEF9", "#B7E8EB", "#9CDEEB", "#1165C1", "#043F98", "#003366")
oranges <- c("#FF7F00", "#FFEFD5", "#FFA500", "#FFD700", "#FF4F00", "#EE7F2D","#FDBCB4", "#E34234", "#F88379", "#ED9121",
              "#E86100" )

orange_radar_tot <- c("gray", "#FF4F00")

orange_radar_female <- c("gray", "#FF7518")

orange_radar_male <- c("gray", "#8B4513")

orange_femalemales <- c( "#FF7518" , "#8B4513")


### County choices for sdg4
choices4 <- V4_T2.2_new %>% 
  distinct(County) %>% 
  filter(County != "XXX") %>% 
  pull()

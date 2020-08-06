## This script is used to further clean the data used in SDG1 and generate 
## functions and analysis tables.

## Load other libraries that we will need.
library(tidyverse)
library(rKenyaCensus)
library(ggrepel)
library(fmsb)

## 1st plots

data <- V4_T2.36

loc_df <- data %>%
  ungroup() %>%
  filter(AdminArea == "xxx") %>%
  filter(SubCounty == "RURAL" | SubCounty == "URBAN"|SubCounty == "Rural" | SubCounty == "Urban") %>%
  select(-County, -AdminArea, -ConventionalHouseholds)%>%
  column_to_rownames(var = "SubCounty") #%>%
  #select(-NotStated)

vars_to_drop <- "NotStated"

if(vars_to_drop %in% names(loc_df)){
  loc_df <- loc_df %>% 
    select(-vars_to_drop)
}


most_minimum <- min(apply(loc_df, 1, function(x) min(x)))
most_maximum <- max(apply(loc_df, 1, function(x) max(x)))

loc_df2 <- rbind(rep(100,ncol(loc_df)) , rep(most_minimum,ncol(loc_df)), loc_df)


red_to_chocolate <- c("#C24352","#B32134", "#FFD1A9", "#E97451", "#FF0000", "#BF0000", "#800000",
                      "#CF0009", "#E15B20","#F7A815", "#F0BA70", "#92000A", "black")



radarchart(loc_df2,
           axistype=1 ,
           #customize the polygons
           pcol=c("#F0BA70","#92000A" ),
           #pfcol=c("#F0BA70","#92000A" ),
           plwd=4,
           plty=1,
           #customize the grid
           cglcol="#E97451",
           cglty=2,
           axislabcol="grey",
           #caxislabels = seq(min(loc_df2),max(loc_df2)+20,5), seg=length(seq(min(loc_df2),max(loc_df2)+20,5))-1,
           cglwd=0.8,
           #custom labels
           vlcex=1.1)
title("Percentage Distribution of Conventional Households \nby\n Main Type of Cooking Fuel",
      cex.main = 1.5,
      sub = "National wide", cex.sub = 2,  col.sub = "black")

# Add a legend
legend(x=1.1, y=-0.9, legend=rownames(loc_df2[-c(1,2),]), bty="n", pch=20 ,
       col=c("#F0BA70","#92000A" ) , text.col="black", cex=1.2, pt.cex=3)

radar_func <- function(dat,tit, cscheme){
data <- dat

loc_df <- data %>%
  ungroup() %>%
  filter(AdminArea == "xxx") %>%
  filter(SubCounty == "RURAL" | SubCounty == "URBAN"|SubCounty == "Rural" | SubCounty == "Urban") %>%
  select(-County, -AdminArea, -ConventionalHouseholds)%>%
  column_to_rownames(var = "SubCounty") #%>%
#select(-NotStated)

vars_to_drop <- "NotStated"

if(vars_to_drop %in% names(loc_df)){
  loc_df <- loc_df %>% 
    select(-vars_to_drop)
}


most_minimum <- min(apply(loc_df, 1, function(x) min(x)))
most_maximum <- max(apply(loc_df, 1, function(x) max(x)))

loc_df2 <- rbind(rep(100,ncol(loc_df)) , rep(most_minimum,ncol(loc_df)), loc_df)


red_to_chocolate <- c("#C24352","#B32134", "#FFD1A9", "#E97451", "#FF0000", "#BF0000", "#800000",
                      "#CF0009", "#E15B20","#F7A815", "#F0BA70", "#92000A", "black")


secondhighest <- stack(loc_df2) %>% arrange(desc(values)) %>% distinct(values) %>% pull()
secondhighest <- secondhighest[2]
radarchart(loc_df2,
           axistype=1 ,
           #customize the polygons
           pcol=cscheme,
           #pfcol=cscheme,
           plwd=4,
           plty=1,
           #customize the grid
           cglcol="#E97451",
           cglty=2,
           axislabcol="grey",
           caxislabels = seq(0,secondhighest,20), seg=length(seq(0,secondhighest,20))-1,
           cglwd=0.8,
           #custom labels
           vlcex=1.1)

title(tit,
      cex.main = 1.5,
      sub = "National wide", cex.sub = 2,  col.sub = "black")

# Add a legend
legend(x=1.0, y=-1.0, legend=rownames(loc_df2[-c(1,2),]), bty="n", pch=20 ,
       col=cscheme , text.col="black", cex=0.8, pt.cex=3)

}

dat <- V4_T2.36
tit <- "Percentage Distribution of Conventional Households \nby\n Main Type of Cooking Fuel"
cscheme <- c("#309AF1","#003366" )
radar_func(dat, tit, cscheme)
  
## 2nd plots

donut_plots <- function(dat, county, title, cpt, cscheme){

data <- dat

data <- data %>% 
  filter(AdminArea == "County" & County == county) %>% 
  select(-SubCounty, -AdminArea, -ConventionalHouseholds)

## Reshape the data

data <- data %>% 
  gather("Type", "Percentage", -County)

# Compute percentages

data <- data %>%
  mutate(fraction = Percentage / 100)



# Compute the cumulative percentages (top of each rectangle)

data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$Type, ":\n", data$Percentage,"%")

# Drop 0s
data <- data %>% 
  filter(Percentage != 0 & Type !="NotStated") %>% 
  top_n(6, Percentage)


# Make the plot
p <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Type)) +
  geom_rect() +
  geom_label_repel(x=4.0, color = "black", family = "Source Sans Pro Semibold",
                   aes(y = labelPosition, label = label), 
                   data = data, size=4, show.legend = F, 
                   nudge_x = 1, force = 2)+
  #geom_label(x=4.6,aes(label=label,y=labelPosition),inherit.aes = TRUE, show.legend = TRUE)+
  #geom_text( x=4.8, aes(y=labelPosition, label=label), color="black", size=4) + # x here controls label position (inner / outer)
  scale_fill_manual(values=cscheme) +
  scale_color_manual(values=cscheme) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(plot.title = element_text(family="Source Sans Pro Semibold", face = "italic",
                                  size = rel(2.0), hjust = 0.5, vjust = -0.5),
        plot.subtitle = element_text(size = rel(1.5), hjust = 0.5),
        plot.caption = element_text(size = rel(1.5)))+
  theme(legend.position = "none")+
  labs(title = title, x = NULL,
       caption = cpt)
return(p)

}

# dat <- V4_T2.12
# county <- "NAIROBI CITY"
# donut_plots(dat, county)
## This script is used to further clean the data used in SDG9 and generate 
## functions and analysis tables.

## Load other libraries that we will need.
library(tidyverse)
library(rKenyaCensus)
library(ggrepel)
library(fmsb)

################## Generate the dataset 

dfa <- V4_T2.32 %>% 
  filter(AdminArea == "xxx" | AdminArea == "County") %>% 
  select(County, contains("_perc"))
dfa$County[1:3] <- c("Total", "Total: Rural", "Total: Urban")

dfb <- V4_T2.33 %>% 
  filter(AdminArea == "xxx" | AdminArea == "County") %>% 
  select(County, contains("_perc"))
dfb$County[1:3] <- c("Total", "Total: Rural", "Total: Urban")

dfc <- V4_T2.34 %>% 
  filter(AdminArea == "xxx" | AdminArea == "County") %>% 
  select(County, contains("_perc"))
dfc$County[1:3] <- c("Total", "Total: Rural", "Total: Urban")


df0 <- Reduce(function(x,y) merge(x = x, y = y, by = "County"), 
              list(dfa, dfb, dfc))

df0 <- df0 %>% 
  gather("Box", "With", -County, na.rm=TRUE) %>% 
  mutate(Without = 100 - With) %>% 
  gather("Category","Value", -County, -Box) %>%
  group_by(County) %>%
  arrange(County, Box, Category) %>% 
  mutate(ind = ifelse(Box  %in% grep("MPO",Box, value = T, ignore.case = T), "MPO",
               ifelse(Box  %in% grep("UoDLT",Box, value = T, ignore.case = T), "UoDLT",
               ifelse(Box  %in% grep("SearchedOnline",Box, value = T, ignore.case = T), 
                      "Searched Goods Online",
               ifelse(Box  %in% grep("UoI",Box, value = T, ignore.case = T), "UoI",
                      Box))))) %>% 
  mutate(Box = paste0(gsub("MPO_|_Perc|UoDLT_|SearchedOnline_|UoI_","",Box),"s"))

############### Generate the plot

sdg9_plots <- function(county, box, indicator, cscheme, title,st, cpt){

  # df <- dat %>% 
  #   filter(AdminArea == "xxx" | AdminArea == "County") %>% 
  #   select(County, contains("_perc")) %>% 
  #   gather("Box", "With", -County, na.rm=TRUE) %>% 
  #   mutate(Without = 100 - With) %>% 
  #   gather("Category","Value", -County, -Box) %>%
  #   group_by(County) %>%
  #   arrange(County, Box, Category) %>% 
  #   mutate(Box = paste0(gsub("MPO_|_Perc","",Box),"s"))
  # 
 df <- df0
 
  if(title %in% grep("Mobile Phone Ownership", title, value = T, ignore.case = T)){
    df  <- df %>% 
      mutate(Category = ifelse(Category == "With", "Own \nmobile phones", "Don't Own \nmobile phones"))
  }else
    if(title %in% grep("Internet", title, value = T, ignore.case = T)){
    df  <- df %>% 
      mutate(Category = ifelse(Category == "With", "Used Internet", "Didn't Use Internet"))
  }else
    if(title %in% grep("Desktop", title, value = T, ignore.case = T)){
    df  <- df %>% 
      mutate(Category = ifelse(Category == "With", "Used a \nDesktop/Laptop/Tablet", "Never used a\n Desktop/Laptop/Tablet"))
    }else
      if(title %in% grep("online", title, value = T, ignore.case = T)){
      df  <- df %>% 
        mutate(Category = ifelse(Category == "With", "Searched goods online", "Never searched goods online"))
    }
    
  

data <- df %>% 
  filter(County == county & Box == box & ind == indicator)

# Compute percentages

data <- data %>%
  mutate(fraction = Value / 100)

# Compute the cumulative percentages (top of each rectangle)

data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$Category, ":\n", data$Value,"%")

# Make the plot
p <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
  geom_rect() +
  geom_label(x=3.5, color = "black", family = "Source Sans Pro Semibold",
                   aes(y = labelPosition, label = label), 
                   data = data, size=4)+
  scale_fill_manual(values=cscheme) +
  scale_color_manual(values=cscheme) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(plot.title = element_text(family="Source Sans Pro Semibold", face = "italic",
                                  size = rel(1.3), hjust = 0.5, vjust = -0.5),
        plot.subtitle = element_text(size = rel(1.0), hjust = 0.5),
        plot.caption = element_text(size = rel(1.0)))+
  theme(legend.position = "none")+
  labs(title = title, subtitle = st, x = NULL,
       caption = cpt)
return(p)
}

# dat <- V4_T2.32
# county <- "WEST POKOT"
# box <- "MPO_Total_Perc"
# cscheme <- orange_radar_tot
# title <- "Population Age 3 years and Above Owning a Mobile Phone"
# cpt <- "TURKANA"
# sdg9_plots(dat, county, box, cscheme, title, cpt)



dfa <- V4_T2.32 %>% 
  filter(AdminArea == "xxx" | AdminArea == "County") %>% 
  select(County, MPO_Total, MPO_Female, MPO_Male) %>% 
  mutate(MPO_Female = round(MPO_Female / MPO_Total *100, 1),
         MPO_Male = round(MPO_Male / MPO_Total *100, 1)) %>% 
  select(-MPO_Total) 

dfb <- V4_T2.33 %>% 
  filter(AdminArea == "xxx" | AdminArea == "County") %>% 
  select(County, UoI_Total, UoI_Male, UoI_Female, UoDLT_Total, UoDLT_Male, UoDLT_Female) %>% 
  mutate(UoI_Female = round(UoI_Female / UoI_Total *100, 1),
         UoI_Male = round(UoI_Male / UoI_Total *100, 1),
         UoDLT_Female = round(UoDLT_Female / UoDLT_Total *100, 1),
         UoDLT_Male = round(UoDLT_Male / UoDLT_Total *100, 1)) %>% 
  select(-UoI_Total, -UoDLT_Total) 

dfc <- V4_T2.34 %>% 
  filter(AdminArea == "xxx" | AdminArea == "County") %>% 
  select(County, SearchedOnline_Total, SearchedOnline_Female, SearchedOnline_Male) %>% 
  mutate(SearchedOnline_Female = round(SearchedOnline_Female / SearchedOnline_Total *100, 1),
         SearchedOnline_Male = round(SearchedOnline_Male / SearchedOnline_Total *100, 1)) %>% 
  select(-SearchedOnline_Total) 

dfa$County[1:3] <- c("Total", "Total: Rural", "Total: Urban")
dfb$County[1:3] <- c("Total", "Total: Rural", "Total: Urban")
dfc$County[1:3] <- c("Total", "Total: Rural", "Total: Urban")

df02 <- Reduce(function(x,y) merge(x = x, y = y, by = "County"), 
              list(dfa, dfb, dfc))

df02 <- df02 %>% 
  gather("Category","Value", -County) %>%
  group_by(County) %>%
  arrange(County, Category) %>% 
  mutate(ind = ifelse(Category  %in% grep("MPO",Category, value = T, ignore.case = T), "MPO",
                      ifelse(Category  %in% grep("UoDLT",Category, value = T, ignore.case = T), "UoDLT",
                             ifelse(Category  %in% grep("SearchedOnline",Category, value = T, ignore.case = T), 
                                    "Searched Goods Online",
                                    ifelse(Category  %in% grep("UoI",Category, value = T, ignore.case = T), "UoI",
                                           Category))))) %>% 
  mutate(Category = paste0(gsub("MPO_|_Perc|UoDLT_|SearchedOnline_|UoI_","",Category),"s"))


sdg9_malefemale <- function(county,indicator, cscheme, title,st, cpt){
  
# df2 <- V4_T2.32 %>%
#   filter(AdminArea == "xxx" | AdminArea == "County") %>%
#   select(County, MPO_Total, MPO_Female, MPO_Male) %>%
#   mutate(MPO_Female = round(MPO_Female / MPO_Total *100, 1),
#          MPO_Male = round(MPO_Male / MPO_Total *100, 1)) %>%
#   select(-MPO_Total) %>%
#   gather("Category","Value", -County)

data <- df02 %>% 
  ungroup() %>% 
  filter(County == county & ind == indicator)

# Compute percentages

data <- data %>%
  mutate(fraction = Value / 100)

# Compute the cumulative percentages (top of each rectangle)

data$ymax <- cumsum(data$fraction)

# Compute the bottom of each rectangle
data$ymin <- c(0, head(data$ymax, n=-1))

# Compute label position
data$labelPosition <- (data$ymax + data$ymin) / 2

# Compute a good label
data$label <- paste0(data$Category, ":\n", data$Value,"%")

# Make the plot
p <- ggplot(data, aes(ymax=ymax, ymin=ymin, xmax=4, xmin=3, fill=Category)) +
  geom_rect() +
  geom_label(x=3.5, color = "black", family = "Source Sans Pro Semibold",
             aes(y = labelPosition, label = label), 
             data = data, size=4)+
  scale_fill_manual(values=cscheme) +
  scale_color_manual(values=cscheme) +
  coord_polar(theta="y") +
  xlim(c(2, 4)) +
  theme_void() +
  theme(plot.title = element_text(family="Source Sans Pro Semibold", face = "italic",
                                  size = rel(1.3), hjust = 0.5, vjust = -0.5),
        plot.subtitle = element_text(size = rel(1.0), hjust = 0.5),
        plot.caption = element_text(size = rel(1.0)))+
  theme(legend.position = "none")+
  labs(title = title, subtitle = st, x = NULL,
       caption = cpt)
return(p)
}


dat <- df02
county <- "WEST POKOT"
box <- "MPO_Total_Perc"
cscheme <- orange_femalemales
title <- "Population Age 3 years and Above Owning a Mobile Phone"
st = ""
cpt <- "TURKANA"
indicator = "MPO"
sdg9_malefemale(county, indicator,  cscheme, title, st, cpt)

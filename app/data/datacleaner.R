## Author: Shelmith Kariuki
## Date: 2nd August, 2020
## This script contains codes used to clean the datasets a bit

## Load the libraries we need.
library(googlesheets4)
library(tidyverse)
library(rKenyaCensus)

## Read in the county data
county_data <- read_csv("data/countysubcounty.csv")

## Generate an id variable, that will be used for merging the data, and
## remove trailing or leading spaces from the Subcounty variable
county_data <- county_data %>%
  mutate(id = as.numeric(as.character(seq_along(SubCounty))),
         SubCounty = trimws(SubCounty))

## Generate a function that generates an id variable, and merges the data
cleaner <- function(dataset){
 
  ## Remove trailing or leading spaces, and generate an id variable.
  dataset <- dataset %>% 
    mutate(SubCounty = trimws(SubCounty)) %>% 
    mutate(id = sort(c(rep(0,6), rep(1:392,3))))
    
 ## Merge the datasets
  new_dataset <- left_join(dataset, county_data, by = c("SubCounty","id"))
  
 ## Clean and reorganise the new dataset
  new_dataset <- new_dataset %>% 
    group_by(County) %>% 
    mutate(num = seq_along(County)) %>% 
    mutate(AdminArea = ifelse(num <=3, "County", "SubCounty")) %>% 
    ungroup() %>% 
    mutate(County = ifelse(SubCounty == "KENYA", "XXX", County),
           AdminArea = ifelse(SubCounty == "KENYA", "XXX", AdminArea)) %>% 
    select(-id, -num) %>% 
    select(County, SubCounty, AdminArea, everything())
  
  ## return the new dataset
  return(new_dataset)
  
}

## Create a list of the dataset
dat_list <- list(V4_T2.2, V4_T2.3, V4_T2.4, V4_T2.5)

## Extract the datasets
newdfs_list <- map(dat_list, cleaner)

## Assign the datasets to their respective names

data_files <- c("V4_T2.2", "V4_T2.3", "V4_T2.4", "V4_T2.5")
assign_function <- function(file, data) {
  assign(paste0(file, "_new"), data,envir=.GlobalEnv)
}

walk2(data_files, newdfs_list,assign_function)

## Good job....




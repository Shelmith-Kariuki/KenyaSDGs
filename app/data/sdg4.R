## This script is used to further clean the data used in SDG4 and generate 
## functions and analysis tables.

## Source scripts
source("data/datacleaner.R")
source("data/global.R")
## Location type graph function

lt_func <- function(dat, xvar, grp_var, ylab, colors, title, cpt){
  lt_g <-  ggplot(data = dat, aes_string(x = xvar, y = "Percentage", fill = grp_var))+
    geom_bar(stat = "identity", position = "dodge")+
    geom_text(aes(label = paste0(Percentage,"%")),family="Source Sans Pro Semibold", hjust = 0.5,
              vjust = -0.25, size = 4,position = position_dodge(width = 0.9))+
    my_theme+
    #theme(axis.text.x = element_text(angle = 45))+
    scale_fill_manual(values = colors)+
    labs(title = title, x = NULL, y = ylab,
         caption = cpt)+
    ylim(c(0,100))
  
  
  return(lt_g)
}
##*********************************************************************************************
## 1 School attendance status (location type)
plot41a_df1 <- V4_T2.2_new %>% 
  filter(Gender == "Rural"|Gender == "Urban") %>% 
  select(SubCounty, Gender, County, AdminArea, contains("_Perc")) %>% 
  gather("Status","Percentage", -SubCounty, -Gender, -County, -AdminArea) %>% 
  rename(locationtype = Gender) %>% 
  mutate(Status = gsub("_Perc", "", Status),
         Status = ifelse(Status == "StillinSchool", "Still in School",
                         ifelse(Status == "LeftSchoolAfterC", "Left School \nAfter Completion",
                                ifelse(Status == "LeftSchoolBeforeC", "Left School \nBefore Completion",
                                       ifelse(Status == "NeverbeentoSchool", "Never been \nto School",
                                              ifelse(Status == "DK", "Don't Know",Status)))))) %>% 
  mutate(Status = fct_relevel(Status, "Never been \nto School", "Still in School",
                              "Left School \nBefore Completion", "Left School \nAfter Completion",
                              "Don't Know")) 

## School attendance status_new (Gender)
plot41b_df1 <- V4_T2.2_new %>% 
  filter(Gender == "Male"|Gender == "Female") %>% 
  select(SubCounty, Gender, County, AdminArea, contains("_Perc")) %>% 
  gather("Status","Percentage", -SubCounty, -Gender, -County, -AdminArea) %>% 
  mutate(Status = gsub("_Perc", "", Status),
         Status = ifelse(Status == "StillinSchool", "Still in School",
                         ifelse(Status == "LeftSchoolAfterC", "Left School \nAfter Completion",
                                ifelse(Status == "LeftSchoolBeforeC", "Left School \nBefore Completion",
                                       ifelse(Status == "NeverbeentoSchool", "Never been \nto School",
                                              ifelse(Status == "DK", "Don't Know",Status)))))) %>% 
  mutate(Status = fct_relevel(Status, "Never been \nto School", "Still in School",
                              "Left School \nBefore Completion", "Left School \nAfter Completion",
                              "Don't Know"))%>% 
  filter(AdminArea == "County")
##*********************************************************************************************
## 3: School attendance status (location type)
plot41a_df3 <- V4_T2.3_new 

for(i in 6:length(V4_T2.3_new)){
  plot41a_df3[,i] <- round(plot41a_df3[,i] / plot41a_df3[,"Total"] *100, 1)
}

plot41a_df3 <- plot41a_df3 %>% 
  filter(Gender == "Rural"|Gender == "Urban") %>% 
  select(-Total, -DK, -NotStated) %>% 
  gather("Level","Percentage", -SubCounty, -Gender, -County, -AdminArea) %>% 
  rename(locationtype = Gender) %>% 
  mutate(Level = ifelse(Level == "AdultBasicEducation", "Adult Basic \nEducation",Level )) %>% 
  mutate(Level = fct_relevel(Level, "PrePrimary","Primary","Secondary","TVET","University",
                             "Adult Basic \nEducation","Madrasa/Duksi"))


## School attendance status (gender)
plot41b_df3 <- V4_T2.3_new 
for(i in 6:length(V4_T2.3_new)){
  plot41b_df3[,i] <- round(plot41b_df3[,i] / plot41b_df3[,"Total"] *100, 1)
}

plot41b_df3 <- plot41b_df3 %>% 
  filter(Gender == "Male"|Gender == "Female") %>% 
  select(-Total, -DK, -NotStated) %>% 
  gather("Level","Percentage", -SubCounty, -Gender, -County, -AdminArea) %>% 
  mutate(Level = ifelse(Level == "AdultBasicEducation", "Adult Basic \nEducation",Level )) %>% 
  mutate(Level = fct_relevel(Level, "PrePrimary","Primary","Secondary","TVET","University",
                             "Adult Basic \nEducation","Madrasa/Duksi"))%>% 
  filter(AdminArea == "County")

##*********************************************************************************************
## 4: Highest level of education reached (location type)
plot41a_df4 <- V4_T2.4_new
for(i in 6:length(V4_T2.4_new)){
  plot41a_df4[,i] <- round(plot41a_df4[,i] / plot41a_df4[,"Total"] *100, 1)
}

plot41a_df4 <- plot41a_df4 %>% 
  filter(Gender == "Rural"|Gender == "Urban") %>% 
  select(-Total, -DK, -NotStated) %>% 
  gather("Level","Percentage", -SubCounty, -Gender, -County, -AdminArea) %>% 
  rename(locationtype = Gender) %>% 
  mutate(Level = ifelse(Level == "AdultBasicEducation", "Adult Basic \nEducation",Level )) %>% 
  mutate(Level = fct_relevel(Level, "PrePrimary","Primary","Secondary","TVET","University",
                             "Adult Basic \nEducation","Madrasa/Duksi"))


## Highest level of education reached (gender)
plot41b_df4 <- V4_T2.4_new 
for(i in 6:length(V4_T2.4_new)){
  plot41b_df4[,i] <- round(plot41b_df4[,i] / plot41b_df4[,"Total"] *100, 1)
}

plot41b_df4 <- plot41b_df4 %>% 
  filter(Gender == "Male"|Gender == "Female") %>% 
  select(-Total, -DK, -NotStated) %>% 
  gather("Level","Percentage", -SubCounty, -Gender, -County, -AdminArea) %>% 
  mutate(Level = ifelse(Level == "AdultBasicEducation", "Adult Basic \nEducation",Level )) %>% 
  mutate(Level = fct_relevel(Level, "PrePrimary","Primary","Secondary","TVET","University",
                             "Adult Basic \nEducation","Madrasa/Duksi"))%>% 
  filter(AdminArea == "County")
##*********************************************************************************************
##*
## 5: Highest level of education completed (location type)
plot41a_df5 <- V4_T2.5_new
for(i in 6:length(V4_T2.5_new)){
  plot41a_df5[,i] <- round(plot41a_df5[,i] / plot41a_df5[,"Total"] *100, 1)
}

plot41a_df5 <- plot41a_df5 %>% 
  filter(Gender == "Rural"|Gender == "Urban") %>% 
  select(-Total, -DK, -NotStated) %>% 
  gather("Level","Percentage", -SubCounty, -Gender, -County, -AdminArea) %>% 
  rename(locationtype = Gender) %>% 
  mutate(Level = ifelse(Level == "AdultBasicEducation", "Adult Basic \nEducation",Level )) %>% 
  mutate(Level = fct_relevel(Level, "PrePrimary","Primary","Secondary","TVET","University",
                             "Adult Basic \nEducation","Madrasa/Duksi"))


## Highest level of education completed (gender)
plot41b_df5 <- V4_T2.5_new 
for(i in 6:length(V4_T2.5_new)){
  plot41b_df5[,i] <- round(plot41b_df5[,i] / plot41b_df5[,"Total"] *100, 1)
}

plot41b_df5 <- plot41b_df5 %>% 
  filter(Gender == "Male"|Gender == "Female") %>% 
  select(-Total, -DK, -NotStated) %>% 
  gather("Level","Percentage", -SubCounty, -Gender, -County, -AdminArea) %>% 
  mutate(Level = ifelse(Level == "AdultBasicEducation", "Adult Basic \nEducation",Level )) %>% 
  mutate(Level = fct_relevel(Level, "PrePrimary","Primary","Secondary","TVET","University",
                             "Adult Basic \nEducation","Madrasa/Duksi"))%>% 
  filter(AdminArea == "County")
##*********************************************************************************************
## Maps
##*********************************************************************************************

# Kenya_df <- rKenyaCensus::KenyaCounties_SHP %>% 
#   st_as_sf()%>% 
#   st_transform(crs = 4326) %>% 
#   select(County, geometry)
# 
# shp_merger <- function(dataset){
#   df <- full_join(Kenya_df, dataset, by="County")
#   df <- df %>% 
#     select(County, geometry, everything())
#   return(df)
# }
# 
# ## Create a list of the dataset
# dat_list <- list(plot41b_df1, plot41b_df3, plot41b_df4, plot41b_df5)
# 
# ## Extract the datasets
# newdfs_list <- map(dat_list, shp_merger)
# 
# ## Assign the datasets to their respective names
# 
# data_files <- c("plot41b_df1", "plot41b_df3", "plot41b_df4", "plot41b_df5")
# assign_function <- function(file, data) {
#   assign(paste0(file, "_shpdata"), data,envir=.GlobalEnv)
# }
# 
# walk2(data_files, newdfs_list,assign_function)

## 2: School attendance status by age (V4_T2.6a)

df_sixa <- V4_T2.6a %>%
  gather("Age_Bracket", "Values", -SchoolAttendanceStatus, -Gender, -Total, na.rm = T) %>%
  filter(Gender != "Intersex" & Gender != "Total" & SchoolAttendanceStatus !="Total") %>%
  group_by(Age_Bracket, Gender) %>% 
  mutate(Percentage = round(Values / sum(Values) *100, 1)) %>%
  filter(SchoolAttendanceStatus != "Not Stated" & SchoolAttendanceStatus != "Total"&
           SchoolAttendanceStatus != "Don't Know") %>% 
  ungroup() %>% 
  mutate(Age_Bracket = fct_relevel(Age_Bracket,"Total","3-5","4-5","6-13","14-17","18-22","23+"))%>% 
  mutate(SchoolAttendanceStatus = ifelse(SchoolAttendanceStatus == "At School/Learning Institution",
                                         "Still in School",
                                         ifelse(SchoolAttendanceStatus == "Left School/Learning Institution After Completion",
                                                "Left School \nAfter Completion",
                                                ifelse(SchoolAttendanceStatus == "Left School/Learning Institution Before Completion",
                                                       "Left School \nBefore Completion",
                                                       ifelse(SchoolAttendanceStatus == "Never Been to School/Learning Institution","Never been to School",
                                                              SchoolAttendanceStatus))))) %>% 
  mutate(SchoolAttendanceStatus = fct_relevel(SchoolAttendanceStatus, "Never been to School", "Still in School" ,
                                              "Left School \nBefore Completion", "Left School \nAfter Completion"))

sixa_func <- function(dat, st){
lt_g <-  ggplot(data = dat,
                aes_string(x = "SchoolAttendanceStatus", y = "Percentage", fill = "Gender"))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = paste0(Percentage,"%")),family="Source Sans Pro Semibold", hjust = 0.5,
            vjust = -0.25, size = 4,position = position_dodge(width = 0.9))+
  my_theme+
  #theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c(s4, s4b))+
  labs(title = "Population Age 3 Years and Above by  \n School Attendance Status and age groups",
       subtitle = paste0("\n(", st, ")"),
       x = NULL, y = "Percentage",
       caption = "National wide")+
  ylim(c(0,100))
return(lt_g)
}



# lt_g <-  ggplot(data = df_sixa,
#                 aes_string(x = "SchoolAttendanceStatus", y = "Percentage", fill = "Gender"))+
#   geom_bar(stat = "identity", position = "dodge")+
#   geom_text(aes(label = paste0(Percentage,"%")),family="Source Sans Pro Semibold", hjust = 0.5,
#             vjust = -0.25, size = 4,position = position_dodge(width = 0.9))+
#   facet_wrap(~SchoolAttendanceStatus, ncol = 2, nrow = 2)+
#   my_theme+
#   #theme(axis.text.x = element_text(angle = 45))+
#   scale_fill_manual(values = c(s4, s4b))+
#   labs(title = "Population Age 3 Years and Above by School Attendance Status \n and age groups",
#        subtitle = "",
#        x = NULL, y = "Percentage",
#        caption = "")
# lt_g

## 2: School attendance status by age and County (V4_T2.6b)

V4_T2.6b_2 <- V4_T2.6b
V4_T2.6b_2 <- V4_T2.6b_2 %>% 
  gather("SchoolAttendanceStatus", "Value",-Age, -County, -Total, -Male, -Female,  na.rm = TRUE) 
V4_T2.6b_2 <- V4_T2.6b_2 %>% 
  mutate(Percentage = ifelse(SchoolAttendanceStatus %in% 
                               grep("_Total", SchoolAttendanceStatus, value = T, ignore.case = T),
                             round(Value / Total *100, 1),
                             ifelse(SchoolAttendanceStatus %in% 
                                      grep("_Male", SchoolAttendanceStatus, value = T, ignore.case = T),
                                    round(Value / Male *100, 1),  
                                    ifelse(SchoolAttendanceStatus %in% 
                                             grep("_Female", SchoolAttendanceStatus, value = T, ignore.case = T),
                                           round(Value / Female *100, 1),NA)))) %>% 
  filter(!SchoolAttendanceStatus %in% 
           grep("DK|Not|Stated", SchoolAttendanceStatus, value = T, ignore.case = T)) %>% 
  separate(SchoolAttendanceStatus, into = c("SchoolAttendanceStatus", "Gender"), sep = "_") %>% 
  select(-Total, -Male, -Female) %>% 
  select(County, Gender, everything()) %>% 
  mutate(SchoolAttendanceStatus = ifelse(SchoolAttendanceStatus == "StillinSchool", "Still in School",
                         ifelse(SchoolAttendanceStatus == "LeftSchoolAfterC", "Left School \nAfter Completion",
                                ifelse(SchoolAttendanceStatus == "LeftSchoolBeforeC", "Left School \nBefore Completion",
                                       ifelse(SchoolAttendanceStatus == "NeverbeentoSchool", "Never been to School",
                                              ifelse(SchoolAttendanceStatus == "DK", "Don't Know",SchoolAttendanceStatus)))))) %>% 
  mutate(SchoolAttendanceStatus = fct_relevel(SchoolAttendanceStatus, "Never been to School", "Still in School",
                              "Left School \nBefore Completion", "Left School \nAfter Completion",
                              "Don't Know")) %>% 
  mutate(Age = fct_relevel(Age,"Total","3-5","4-5","6-13","14-17","18-22","23+")) %>% 
  filter(Age!="Total" & Gender!="Total") #%>% 
  #mutate(SchoolAttendanceStatus = gsub("\\n","",SchoolAttendanceStatus))


sixb_func <- function(dat, st, cpt){
lt_g <-  ggplot(data = dat,
                aes_string(x = "SchoolAttendanceStatus", y = "Percentage", fill = "Gender"))+
  geom_bar(stat = "identity", position = "dodge")+
  geom_text(aes(label = paste0(Percentage,"%")),family="Source Sans Pro Semibold", hjust = 0.5,
            vjust = -0.25, size = 4,position = position_dodge(width = 0.9))+
  #facet_wrap(~SchoolAttendanceStatus, ncol = 2, nrow = 2)+
  my_theme+
  #theme(axis.text.x = element_text(angle = 45))+
  scale_fill_manual(values = c(s4, s4b))+
  labs(title = "Population Age 3 Years and Above by \nSchool Attendance Status and age groups",
       subtitle = paste0("\n(", st, ")"),
       x = NULL, y = "Percentage",
       caption = cpt)
lt_g
}
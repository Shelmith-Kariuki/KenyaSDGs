library(shiny)
library(shinydashboard)
library(shinydashboardPlus)
library(leaflet)
library(tmap)
library(ggrepel)


source("data/sdg1.R")
source("data/sdg4.R")
source("data/sdg9.R")
source("data/global.R")

# Define UI for application that draws a histogram

## header
header <- dashboardHeader(
    # Set height of dashboardHeader
    tags$li(class = "dropdown",
            tags$style(".main-header {max-height: 60px}"),
            tags$style(".main-header .logo {height: 60px;}"),
            tags$style(".sidebar-toggle {height: 60px; padding-top: 1px !important;}"),
            tags$style(".navbar {min-height:60px !important}")
    ),
    ## title and title width
    title = tags$h1(tags$b(tags$i("Kenya SDG Tracker"))),
    titleWidth = 500)

## sidebar
sidebar <- dashboardSidebar(width = 400,
    sidebarMenu(
        menuItem(span(tags$i("About"),style="color:white;font-size:23px"), tabName = "home",
                 icon=icon("home"),
                 img(src = "intro2.png", width = 390)),
        br(),
        tags$style(type='text/css', ".selectize-input { font-size: 20px; line-height: 20px; width: 380px;} .selectize-dropdown { font-size: 20px; line-height: 20px; }"),
         selectInput("sdg", span(tags$i("Select SDG"),style="color:white;font-size:23px"),
                    choices = sdgs),
        selectInput("indicator", tags$h3(tags$b(tags$i("Select Indicator"))), choices = ""),
        selectInput("county", tags$h3(tags$b(tags$i("Select County/SubCounty"))), choices = choices4),
        br(),br(),br(),
        div(img(src = "sdgposter.png", width = 390), style="text-align: center;"),
       
        menuItem(span(tags$i("Created by:"),style="color:white;font-size:23px"),
                 tabName = "author",icon=icon("user"),
                 div(img(src = "Shel.jpg", width = 200, height = 200), style="text-align: center;"),
                 br(),
                 menuSubItem(span(tags$i("Website"),style="color:white;font-size:18px"),
                             icon=icon("rss"),href = "https://shelkariuki.netlify.app/"),br(),
                 menuSubItem(span(tags$i("Github"),style="color:white;font-size:18px"),
                             icon=icon("github"),href = "https://github.com/Shelmith-Kariuki"),br(),
                 menuSubItem(span(tags$i("Twitter"),style="color:white;font-size:18px"),
                             icon=icon("twitter"),href = "https://twitter.com/Shel_Kariuki"),br(),
                 menuSubItem(span(tags$i("LinkedIn"),style="color:white;font-size:18px"),
                             icon=icon("linkedin-in"),href = "https://www.linkedin.com/in/shelmith-kariuki-44351363")
                 ,br())
    )
)

## body
body <- dashboardBody(
  
  fluidRow(
    column(width = 2,
           imageOutput("image", width = "10%", height = "200px")),
    column(width = 10,
           textOutput("definition"),
           tags$head(tags$style("#definition{color: black;
                                 font-size: 20px;
                                 font-style: italic;
                                 }")),
           HTML(paste("<p style = 'color: black; font-size: 20px; font-style: italic'> To learn more about this goal, <a href='https://www.undp.org/content/undp/en/home/sustainable-development-goals.html'>click here.</a></p>"))
    )),
  conditionalPanel(
    condition = "input.sdg != 'SDG 9 : Industry, Innovation and Infrastructure'",
    
  conditionalPanel(condition = "input.indicator == 'School attendance status by age'", 
                   radioButtons("silver_shape",
                                label = "Age Bracket:", 
                                choices = unique(V4_T2.6b_2$Age))),
    fluidRow(column(width = 6,
                box(plotOutput("plot41a",width = "600px", height  = "600px"),title = "",  solidHeader = F, width = 14, status = "danger")),
             column(width = 6,
                box(plotOutput("plot41b",width = "600px", height  = "600px"),title = "",  solidHeader = F, width = 14, status = "danger"))
             )),
  conditionalPanel(
    condition = "input.sdg == 'SDG 9 : Industry, Innovation and Infrastructure'",
    
    fluidRow(column(width = 6,
                    box(plotOutput("plot91a",width = "600px", height  = "300px"),title = "",  solidHeader = F, width = 14, status = "danger")),
             column(width = 6,
                    box(plotOutput("plot91b",width = "600px", height  = "300px"),title = "",  solidHeader = F, width = 14, status = "danger"))
    ),
    
    fluidRow(column(width = 6,
                    box(plotOutput("plot91c",width = "600px", height  = "300px"),title = "",  solidHeader = F, width = 14, status = "danger")),
             column(width = 6,
                    box(plotOutput("plot91d",width = "600px", height  = "300px"),title = "",  solidHeader = F, width = 14, status = "danger"))
    )#,
    # fluidRow(column(width = 4,
    #                 box(plotOutput("plot92a",width = "300px", height  = "300px"),title = "",  solidHeader = F, width = 14, status = "danger")),
    #          column(width = 4,
    #                 box(plotOutput("plot92b",width = "300px", height  = "300px"),title = "",  solidHeader = F, width = 14, status = "danger")),
    #          column(width = 4,
    #                 box(plotOutput("plot92c",width = "300px", height  = "300px"),title = "",  solidHeader = F, width = 14, status = "danger"))
    # ),
    # fluidRow(column(width = 4,
    #                 box(plotOutput("plot93a",width = "300px", height  = "300px"),title = "",  solidHeader = F, width = 14, status = "danger")),
    #          column(width = 4,
    #                 box(plotOutput("plot93b",width = "300px", height  = "300px"),title = "",  solidHeader = F, width = 14, status = "danger")),
    #          column(width = 4,
    #                 box(plotOutput("plot93c",width = "300px", height  = "300px"),title = "",  solidHeader = F, width = 14, status = "danger"))
    # )
    
    
    
  ))

ui <- dashboardPage(skin = "red",
                    header, sidebar, body)
##****************************************************************************************


server <- function(input, output, session) {
    
## Indicators will be listed depending on the SDG selected
    observeEvent(input$sdg, {
        
        values <- sdg_file %>% 
            filter(SDG == input$sdg) %>% 
            select(Indicator) %>% 
            pull()
        updateSelectInput(session, "indicator", choices = values)
    })

## Add an image for each of the indicator
output$image <- renderImage({
    
  filename = paste0("www/",input$sdg,".png")
  list(src = filename)
    }, deleteFile = FALSE)

## Definition and importance of each sdg
output$definition <- renderText({
  def <- sdg_file %>% 
    filter(SDG == input$sdg) %>% 
    distinct(Def) %>% 
    pull
  return(def)
})



## Graph for plot41a

output$plot41a <- renderPlot({
if(input$indicator == "School attendance status"){
    dat = plot41a_df1
    lt_func(dat, "Status", "locationtype", "Percentage", s4, 
            paste0(input$indicator, "\n\nBy Location" ), "National level information only")
}else
    if(input$indicator == "Current level of education"){
        dat = plot41a_df3
        lt_func(dat, "Level", "locationtype", "Percentage", s4, paste0(input$indicator, "\n\nBy Location" ), "National level information only")
    }else
        if(input$indicator == "Highest level of education reached"){
            dat = plot41a_df4
            lt_func(dat, "Level", "locationtype", "Percentage", s4, paste0(input$indicator, "\n\nBy Location" ), "National level information only")     
    }else
        if(input$indicator == "Highest level of education completed"){
            dat = plot41a_df5
            lt_func(dat, "Level", "locationtype", "Percentage", s4, paste0(input$indicator, "\n\nBy Location" ), "National level information only")
        }else
          if(input$indicator == "School attendance status by age"){
            df <- df_sixa %>% 
              filter(Age_Bracket == input$silver_shape)
            
            dat = df
            sixa_func(dat, input$silver_shape)
          }else
            if(input$indicator == "Cooking fuel"){
              dat <- V4_T2.18
              title <- "Main Type of Cooking Fuel\n\n"
              cscheme <- c("#F0BA70","#92000A" )
              radar_func(dat, title, cscheme)
            }else
              if(input$sdg == "SDG 1 : No Poverty" & input$indicator == "Disposal of human waste (sanitation)"){
                dat <- V4_T2.16
                title <- "Main Mode of Human Waste Disposal\n\n"
                cscheme <- c("#F0BA70","#92000A" )
                radar_func(dat, title, cscheme)
              }else
                if(input$sdg == "SDG 1 : No Poverty" & input$indicator == "Drinking water"){
                  dat <- V4_T2.15
                  title <- "Main Source of Drinking Water\n\n"
                  cscheme <- c("#F0BA70","#92000A" )
                  radar_func(dat, title, cscheme)
                }else
                  if(input$indicator == "Electricity"){
                    dat <- V4_T2.19
                    title <- "Main Type of Lighting Fuel\n\n"
                    cscheme <- c("#F0BA70","#92000A" )
                    radar_func(dat, title, cscheme)
                  }else
                    if(input$indicator == "Housing: Main Roofing Material"){
                      dat <- V4_T2.12
                      title <- "Dominant Roofing Material of the Main Dwelling Unit\n\n"
                      cscheme <- c("#F0BA70","#92000A" )
                      radar_func(dat, title, cscheme)
                    }else
                      if(input$indicator == "Housing: Main Floor Material"){
                        dat <- V4_T2.14
                        title <- "Dominant Floor Material of the Main Dwelling Unit\n\n"
                        cscheme <- c("#F0BA70","#92000A" )
                        radar_func(dat, title, cscheme)
                      }else
                        if(input$indicator == "Housing: Main Wall Material"){
                          dat <- V4_T2.13
                          title <- "Dominant Wall Material of the Main Dwelling Unit\n\n"
                          cscheme <- c("#F0BA70","#92000A" )
                          radar_func(dat, title, cscheme)
                        }else
                          if(input$indicator == "Assets"){
                            dat <- V4_T2.36
                            title <- "Ownership of Selected Household Assets\n\n"
                            cscheme <- c("#F0BA70","#92000A" )
                            radar_func(dat, title, cscheme)
                          }else
                            if(input$sdg == "SDG 1 : No Poverty" & input$indicator == "Disposal of solid waste"){
                              dat <- V4_T2.17
                              title <- "Main Mode of Solid Waste Disposal\n\n"
                              cscheme <- c("#F0BA70","#92000A" )
                              radar_func(dat, title, cscheme)
                            }else
                              if(input$sdg == "SDG 6 : Clean Water and Sanitation" & input$indicator == "Disposal of solid waste"){
                                dat <- V4_T2.17
                                title <- "Main Mode of Solid Waste Disposal\n\n"
                                cscheme <- c("#309AF1","#003366" )
                                radar_func(dat, title, cscheme)
                              }else
                                if(input$sdg == "SDG 6 : Clean Water and Sanitation" & input$indicator == "Disposal of human waste (sanitation)"){
                                  dat <- V4_T2.16
                                  title <- "Main Mode of Human Waste Disposal\n\n"
                                  cscheme <- c("#309AF1","#003366" )
                                  radar_func(dat, title, cscheme)
                                }else
                                  if(input$sdg == "SDG 6 : Clean Water and Sanitation" & input$indicator == "Drinking water"){
                                    dat <- V4_T2.15
                                    title <- "Main Source of Drinking Water\n\n"
                                    cscheme <- c("#309AF1","#003366" )
                                    radar_func(dat, title, cscheme)
                                  }else
                                    if(input$sdg == "SDG 9 : Industry, Innovation and Infrastructure" & input$indicator == "Communication infrastructure"){
                                      
                                    }
        
        

})


## Graph for plot41b
output$plot41b <- renderPlot({
    if(input$indicator == "School attendance status"){
        df <- plot41b_df1 %>% filter(SubCounty == input$county)
        dat = df
        lt_func(dat, "Status", "Gender", "Percentage", s4b, 
                paste0(input$indicator, "\n\nBy Gender" ),input$county)
    }else
        if(input$indicator == "Current level of education"){
            df <- plot41b_df3 %>% filter(SubCounty == input$county)
            dat = df
            lt_func(dat, "Level", "Gender", "Percentage", s4b, paste0(input$indicator, "\n\nBy Gender" ),input$county)
    }else
        if(input$indicator == "Highest level of education reached"){
            df <- plot41b_df4 %>% filter(SubCounty == input$county)
            dat = df
            lt_func(dat, "Level", "Gender", "Percentage", s4b, paste0(input$indicator, "\n\nBy Gender" ),input$county)
    }else
        if(input$indicator == "Highest level of education completed"){
            df <- plot41b_df5 %>% filter(SubCounty == input$county)
            dat = df
            lt_func(dat, "Level", "Gender", "Percentage", s4b, paste0(input$indicator, "\n\nBy Gender" ),input$county)
        }else
          if(input$indicator == "School attendance status by age"){
            df <- V4_T2.6b_2 %>% 
              filter(County == input$county) %>% 
              filter(Age == input$silver_shape)
              
            dat = df
            sixb_func(dat, input$silver_shape, input$county)
          }else
            if(input$indicator == "Cooking fuel"){
              dat <- V4_T2.18
              county <- input$county
              title <- "Main Type of Cooking Fuel\n\n"
              cpt <- county
              cscheme <- red_to_chocolate
              donut_plots(dat, county, title, cpt, cscheme)
            }else
              if(input$sdg == "SDG 1 : No Poverty" & input$indicator == "Disposal of human waste (sanitation)"){
                dat <- V4_T2.16
                county <- input$county
                title <- "Main Mode of Human Waste Disposal\n\n"
                cpt <- county
                cscheme <- red_to_chocolate
                donut_plots(dat, county, title, cpt, cscheme)
              }else
                if(input$sdg == "SDG 1 : No Poverty" & input$indicator == "Drinking water"){
                  dat <- V4_T2.15
                  county <- input$county
                  title <- "Main Source of Drinking Water\n\n"
                  cpt <- county
                  cscheme <- red_to_chocolate
                  donut_plots(dat, county, title, cpt, cscheme)
                }else
                  if(input$indicator == "Electricity"){
                    dat <- V4_T2.19
                    county <- input$county
                    title <- "Main Type of Lighting Fuel\n\n"
                    cpt <- county
                    cscheme <- red_to_chocolate
                    donut_plots(dat, county, title, cpt, cscheme)
                  }else
                    if(input$indicator == "Housing: Main Roofing Material"){
                      dat <- V4_T2.12
                      county <- input$county
                      title <- "Dominant Roofing Material of the Main Dwelling Unit\n\n"
                      cpt <- county
                      cscheme <- red_to_chocolate
                      donut_plots(dat, county, title, cpt, cscheme)
                    }else
                      if(input$indicator == "Housing: Main Floor Material"){
                        dat <- V4_T2.14
                        county <- input$county
                        title <- "Dominant Floor Material of the Main Dwelling Unit\n\n"
                        cpt <- county
                        cscheme <- red_to_chocolate
                        donut_plots(dat, county, title, cpt, cscheme)
                      }else
                        if(input$indicator == "Housing: Main Wall Material"){
                          dat <- V4_T2.13
                          county <- input$county
                          title <- "Dominant Wall Material of the Main Dwelling Unit\n\n"
                          cpt <- county
                          cscheme <- red_to_chocolate
                          donut_plots(dat, county, title, cpt, cscheme)
                        }else
                          if(input$indicator == "Assets"){
                            dat <- V4_T2.36
                            county <- input$county
                            title <- "Ownership of Selected Household Assets\n\n"
                            cpt <- county
                            cscheme <- red_to_chocolate
                            donut_plots(dat, county, title, cpt, cscheme)
                          }else
                            if(input$sdg == "SDG 1 : No Poverty" & input$indicator == "Disposal of solid waste"){
                              dat <- V4_T2.17
                              county <- input$county
                              title <- "Main Mode of Solid Waste Disposal\n\n"
                              cpt <- county
                              cscheme <- red_to_chocolate
                              donut_plots(dat, county, title, cpt, cscheme)
                            }else
                              if(input$sdg == "SDG 6 : Clean Water and Sanitation" & input$indicator == "Disposal of solid waste"){
                                dat <- V4_T2.17
                                county <- input$county
                                title <- "Main Mode of Solid Waste Disposal\n\n"
                                cpt <- county
                                cscheme <- blues_colors
                                donut_plots(dat, county, title, cpt, cscheme)
                              }else
                                if(input$sdg == "SDG 6 : Clean Water and Sanitation" & input$indicator == "Disposal of human waste (sanitation)"){
                                  dat <- V4_T2.16
                                  county <- input$county
                                  title <- "Main Mode of Human Waste Disposal\n\n"
                                  cpt <- county
                                  cscheme <- blues_colors
                                  donut_plots(dat, county, title, cpt, cscheme)
                                }else
                                  if(input$sdg == "SDG 6 : Clean Water and Sanitation" & input$indicator == "Drinking water"){
                                    dat <- V4_T2.15
                                    county <- input$county
                                    title <- "Main Source of Drinking Water\n\n"
                                    cpt <- county
                                    cscheme <- blues_colors
                                    donut_plots(dat, county, title, cpt, cscheme)
                                  }else
                                    if(input$sdg == "SDG 9 : Industry, Innovation and Infrastructure" & input$indicator == "Communication infrastructure"){

                                    }
})

output$plot91a <- renderPlot({
  box <- "Totals"
  cscheme <- orange_radar_tot
  title <- "Mobile Phone Ownership\n"
  st <- "(Total Population)"
  cpt <- county
  sdg9_plots(V4_T2.32, input$county, box, cscheme, title,st, cpt)

})
 
output$plot91b <- renderPlot({
  box <- "MPO_Female_Perc"
  cscheme <- orange_femalemales
  title <- "Mobile Phone Ownership\n"
  st <- "Males vs Females"
  cpt <- county
  sdg9_malefemale(V4_T2.32, input$county, cscheme, title, st, cpt)

})

output$plot91c <- renderPlot({
  box <- "Females"
  cscheme <- orange_radar_female
  title <- "Mobile Phone Ownership\n"
  st <- "(Females)"
  cpt <- county
  sdg9_plots(V4_T2.32, input$county, box, cscheme, title,st, cpt)
  
})

output$plot91d <- renderPlot({
  box <- "Males"
  cscheme <- orange_radar_male
  title <- "Mobile Phone Ownership\n"
  st <- "(Males)"
  cpt <- county
  sdg9_plots(V4_T2.32, input$county, box, cscheme, title,st, cpt)
  
})


#orange_radar <- c("#FF4F00", "#FFD700")


  
  
}
# Run the application 
shinyApp(ui = ui, server = server)

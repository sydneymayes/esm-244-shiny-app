library(shiny)
library(shinythemes)
library(tidyverse)
library(here)
library(dplyr)
library(janitor)
library(sf)
library(tmap)
library(shinyWidgets)

### CA counties data set from Anna, we'll need to use one with crop type when we're ready
et_counties <- read_csv(here("data","counties_irrigation.csv"))

et_counties_clean <- et_counties %>% 
  clean_names() %>% 
  mutate(pred_et_mm_year = et_mm_year - ag_et_mm_year) %>% 
  select(name, mm_year, et_mm_year, ag_et_mm_year, 
         pred_et_mm_year, irrigation_efficiency, lon, lat)

### Crop type data from Anna (### THIS IS SPACE FOR SYD TO CODE ###)
et_crops <- read_csv(here("data", "bardata.csv"))


















### End of Syd's section!

### CA counties shapefile
ca_counties_sf <- read_sf(here("data/ca_counties/CA_Counties_TIGER2016.shp")) %>% 
  clean_names()

ca_subset_sf <- ca_counties_sf %>% 
  janitor::clean_names() %>% 
  select(name)

### converting data frame from Anna to shapefile

final_sf <- et_counties_clean %>% 
  drop_na() %>% 
  full_join(y = ca_subset_sf,
             by = "name") %>%
  select(-lon, -lat) %>% 
  pivot_longer(cols = mm_year:irrigation_efficiency,
               names_to = "var",
               values_to = "values")


### setting up colors for map on overview tab

color_list <- list(mm_year = c("red", 'orange', 'yellow'),
                   et_mm_year = c('green', 'blue', 'purple'),
                   ag_et_mm_year = c('cyan', 'blue', 'purple'),
                   pred_et_mm_year = c('purple', 'pink', 'red'),
                   irrigation_efficiency = c('cyan', 'blue', 'midnightblue'))

### (### THIS IS SPACE FOR RACHEL TO CODE ###)



















### End of Rachel's section!

### creating regions and region column
cv = c('Butte', 'Colusa', 'Fresno', 'Glenn', 'Kern', 'Kings', 'Madera', 'Merced', 'Placer', 'San Joaquin', 'Sacramento', 'Shasta', 'Solano', 'Stanislaus', 'Sutter', 'Tehama', 'Tulare', 'Yolo', 'Yuba')
nc = c('Alameda', 'Alpine', 'Amador', 'Calaveras', 'Contra Costa', 'Del Norte', 'El Dorado', 'Humboldt', 'Inyo', 'Lake', 'Lassen', 'Marin', 'Mariposa', 'Mendocino', 'Modoc', 'Mono', 'Monterey', 'Napa', 'Nevada', 'Placer', 'Plumas', 'San Benito', 'San Francisco', 'San Mateo', 'Santa Clara', 'Santa Cruz', 'Sierra', 'Siskiyou', 'Sonoma', 'Trinity', 'Tuolumne')
sc = c('Imperial', 'Los Angeles', 'Orange', 'Riverside', 'San Bernardino', 'San Diego', 'San Luis Obispo', 'Santa Barbara', 'Ventura')

et_counties_mod <- et_counties_clean %>% 
  mutate(region = case_when(name %in% cv ~ "Central Valley",
                            name %in% nc ~ "Northern California",
                            name %in% sc ~ "Southern California"))

### (### THIS IS SPACE FOR ASHLEY TO CODE ###)

  
  
  
  
  
  











  
### End of Ashley's section!

### Create the user interface (shiny uses camelCase)
ui <- fluidPage(theme = shinytheme('sandstone'),
  navbarPage("Irrigation Efficiency and Crop Type",
             tabPanel("Overview",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId = 'pick_variable_map',
                                       label = "Select to view in map",
                                       choices = c("Irrigation (mm/yr)" = "mm_year", 
                                                   "Total ET (mm/yr)" = "et_mm_year", 
                                                   "Agricultural ET (mm/yr)" = "ag_et_mm_year", 
                                                   "Natural ET (mm/yr)" = "pred_et_mm_year", 
                                                   "Irrigation Efficiency" = "irrigation_efficiency")
                                       ) # end of radioButtons
                                     ), # End of Overview sidebarPanel
                        
                        mainPanel("Put my map here!",
                                   plotOutput(outputId = 'ca_map')
                                  ) ### end mainPanel
                        
                                    ), #end of Overview sideBarLayout
                      ), #End "Overview" tabPanel
             
             
             
             tabPanel("Counties",
                      sidebarLayout(
                        sidebarPanel(
                          "WIDGETS",
                          virtualSelectInput(inputId = "select_county",
                                             label = "Select Counties",
                                             choices = list("Northern California" = nc,
                                                           "Central Valley" = cv,
                                                           "Southern California" = sc),
                                             showValueAsTags = TRUE,
                                             search = TRUE,
                                             multiple = TRUE
                                             ), # end virtualSelectInput
                          selectInput(inputId = 'pick_variable',
                                      label = 'Select Variable(s)',
                                      choices = c("Irrigation (mm/yr)" = "mm_year", 
                                                  "Total ET (mm/yr)" = "et_mm_year", 
                                                  "Agricultural ET (mm/yr)" = "ag_et_mm_year", 
                                                  "Natural ET (mm/yr)" = "pred_et_mm_year", 
                                                  "Irrigation Efficiency" = "irrigation_efficiency")
                                     ) # end selectInput
                                     ), #end sidebarPanel
                        
                        mainPanel("Put my graph here!",
                                  plotOutput(outputId = 'counties_plot'),
                                  tableOutput(outputId = 'counties_table')
                                 ) ### end mainPanel
                        
                                    ) # end sidebarLayout
                      ), # end tabPanel 'Counties'
             
             
             tabPanel("Crop Type", ### ! WE DON'T HAVE THIS DATA YET !
                      selectInput(inputId = 'pick_crop',
                                  label = 'Choose crop type:',
                                  choices = unique(et_counties_mod$crop) ### make sure to update once we get real data here
                                  ) # end of crop type selectInput
                     ), #end crop type tabPanel
             ) #end navbarPage
) # end of fluidPage


### Create the server function
server <- function(input, output){
  
### Tab 1 (Rachel)
  
  map_fill <- reactive({
    final_sf %>%
      filter(var == input$pick_variable_map)
  })

  # var_color <- reactive({
  #   color_list %>% 
  #     pluck(input$pick_variable_map)
  # })
  
  
  output$ca_map <- renderPlot({
    
  
    ggplot() + 
      geom_sf(data = map_fill(), aes(fill = values, geometry = geometry),
              color = 'black', size = 0.1) +
      theme_void()
      # scale_fill_gradient2(colors = 'red') +
  })
  
### Tab 2 ()
  
  
  

  
  
  
  
### Tab 3 ()

  
  
  
  
  
  
  
  
} ### end of server function

### Combine these into an app
shinyApp(ui = ui, server = server)

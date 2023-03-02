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
  select(name, mm_year, et_mm_year, ag_et_mm_year, pred_et_mm_year, irrigation_efficiency, lon, lat)

### CA counties shapefile
ca_counties_sf <- read_sf(here("data/ca_counties/CA_Counties_TIGER2016.shp")) %>% 
  clean_names()

ca_subset_sf <- ca_counties_sf %>% 
  janitor::clean_names() %>% 
  select(name)

# # ca_subset_for_merge <- ca_subset_sf %>% 
#   as.data.frame() %>% 
#   mutate(name = county_name) %>% 
#   select(-geometry, -county_name) 
  

### converting data frame from Anna to shapefile

### First, we need to merge to add land area column into ET dataset from Anna

final_sf <- et_counties_clean %>% 
  drop_na() %>% 
  full_join(y = ca_subset_sf,
             by = "name") %>%
  select(-lon, -lat) %>% 
  pivot_longer(cols = mm_year:irrigation_efficiency,
               names_to = "var",
               values_to = "values")

color_df <- data.frame(var = c("mm_year", 'et_mm_year', 'ag_et_mm_year', 
                         'pred_et_mm_year', 'irrigation_efficiency'),
                       palette = c('Reds', 'Oranges', 'Yellows', 'Greens', 'Blues'))


# et_counties_merged <- merge(et_counties_clean_dropna,
#                                      ca_subset_for_merge,
#                                      by = "name")

# et_counties_sf <- st_as_sf(et_counties_merged, coords = c("lon", 'lat'),
#                            crs = st_crs(ca_counties_sf))


### creating regions and region column
cv = c('Butte', 'Colusa', 'Fresno', 'Glenn', 'Kern', 'Kings', 'Madera', 'Merced', 'Placer', 'San Joaquin', 'Sacramento', 'Shasta', 'Solano', 'Stanislaus', 'Sutter', 'Tehama', 'Tulare', 'Yolo', 'Yuba')
nc = c('Alameda', 'Alpine', 'Amador', 'Calaveras', 'Contra Costa', 'Del Norte', 'El Dorado', 'Humboldt', 'Inyo', 'Lake', 'Lassen', 'Marin', 'Mariposa', 'Mendocino', 'Modoc', 'Mono', 'Monterey', 'Napa', 'Nevada', 'Placer', 'Plumas', 'San Benito', 'San Francisco', 'San Mateo', 'Santa Clara', 'Santa Cruz', 'Sierra', 'Siskiyou', 'Sonoma', 'Trinity', 'Tuolumne')
sc = c('Imperial', 'Los Angeles', 'Orange', 'Riverside', 'San Bernardino', 'San Diego', 'San Luis Obispo', 'Santa Barbara', 'Ventura')

et_counties_mod <- et_counties_clean %>% 
  mutate(region = case_when(name %in% cv ~ "Central Valley",
                            name %in% nc ~ "Northern California",
                            name %in% sc ~ "Southern California"))

### Graphics for Landing Page

# county_et_plot <- ggplot(data = et_counties_clean_dropna,
#                       aes(x = name, y = et_mm_year)) +
#   geom_histogram(aes(fill = name), color = "darkslategray", binwidth = 1, show.legend = FALSE) +
#   scale_color_viridis_b() +
#   labs(title = "", x = "County", y = "Average Annual Evapotranspiration (mm)") +
#   theme_bw()

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
  
  
  map_fill <- reactive({
    final_sf %>%
      filter(var == input$pick_variable_map)
  })

  var_color <- reactive({
    color_df %>% 
      filter(var == input$pick_variable_map)
  })
  
  
  output$ca_map <- renderPlot({
    
    ### having issues with this map
    ### I think that the data from Anna is showing up as points. We need to turn them into polygons based on the county shape
    ggplot() + 
      geom_sf(data = map_fill(), aes(fill = values, geometry = geometry),
              color = 'black', size = 0.1) +
      scale_fill_gradientn(colors = var_color()$palette) +
      theme_void()
  })
  
  output$penguin_table <- renderTable({
    penguin_table()
    
    
  })
  
  output$irrigation_image <- renderImage({
    
    list(src = "irrigation_efficiency_image.png",
         width = "100%",
         height = 330) }, deleteFile = FALSE)
}

### Combine these into an app
shinyApp(ui = ui, server = server)

library(shiny)
library(shinythemes)
library(tidyverse)
library(here)
library(dplyr)
library(janitor)
library(sf)
library(tmap)

### CA counties data set from Anna, we'll need to use one with crop type when we're ready
et_counties <- read_csv(here("data","counties_irrigation.csv"))

et_counties_clean <- et_counties %>% 
  clean_names()

### CA counties shapefile
ca_counties_sf <- read_sf(here("data/ca_counties/CA_Counties_TIGER2016.shp")) %>% 
  clean_names()

ca_subset_sf <- ca_counties_sf %>% 
  janitor::clean_names() %>% 
  select(county_name = name, land_area = aland)

### converting data frame from Anna to shapefile
et_counties_clean_dropna <- et_counties_clean %>% 
  drop_na()

et_counties_sf <- st_as_sf(et_counties_clean_dropna, coords = c("lon", 'lat'),
                           crs = st_crs(ca_counties_sf))


### creating regions and region column
cv = c('Butte', 'Colusa', 'Fresno', 'Glenn', 'Kern', 'Kings', 'Madera', 'Merced', 'Placer', 'San Joaquin', 'Sacramento', 'Shasta', 'Solano', 'Stanislaus', 'Sutter', 'Tehama', 'Tulare', 'Yolo', 'Yuba')
nc = c('Alameda', 'Alpine', 'Amador', 'Calaveras', 'Contra Costa', 'Del Norte', 'El Dorado', 'Humboldt', 'Inyo', 'Lake', 'Lassen', 'Marin', 'Mariposa', 'Mendocino', 'Modoc', 'Mono', 'Monterey', 'Napa', 'Nevada', 'Placer', 'Plumas', 'San Benito', 'San Francisco', 'San Mateo', 'Santa Clara', 'Santa Cruz', 'Sierra', 'Siskiyou', 'Sonoma', 'Trinity', 'Tuolumne')
sc = c('Imperial', 'Los Angeles', 'Orange', 'Riverside', 'San Bernardino', 'San Diego', 'San Luis Obispo', 'Santa Barbara', 'Ventura')

et_counties_mod <- et_counties_clean %>% 
  mutate(region = case_when(name %in% cv ~ "Central Valley",
                            name %in% nc ~ "Northern California",
                            name %in% sc ~ "Southern California"))


### Create the user interface (shiny uses camelCase)
ui <- fluidPage(theme = shinytheme('sandstone'),
  navbarPage("Irrigation Efficiency and Crop Type",
             tabPanel("Counties",
                      sidebarLayout(
                        sidebarPanel(
                          "WIDGETS",
                          selectInput(inputId = 'pick_counties',
                                      label = 'Choose a county:',
                                      choices = unique(et_counties_mod$name)
                                      ) # end selectInput
                                    ), #end sidebarPanel
                        
                        mainPanel("OUTPUT!")
                        
                      ) # end sidebarLayout
                      ), # end tabPanel 'Counties'
             tabPanel("Crop Type", ### ! WE DON'T HAVE THIS DATA YET !
                      selectInput(inputId = 'pick_crop',
                                  label = 'Choose crop type:',
                                  choices = unique(et_counties_mod$crop) ### make sure to update once we get real data here
                      ) # end selectInput
             ), #end sidebarLayout),
             tabPanel("TBD")
             ) #end navbarPage
) # end ui
 

### Create the server function
server <- function(input, output){
  penguin_select <- reactive({
    penguins %>%
      filter(species == input$penguin_species)
  })
  
  penguin_table <- reactive({
    penguins %>%
      filter(species == input$penguin_species) %>%
      group_by(sex) %>%
      summarize(mean_flip = mean(flipper_length_mm),
                mean_mass = mean(body_mass_g))
  }) ### end penguin_table
  
  output$penguin_plot <- renderPlot({
    
    ggplot(data = penguin_select(),
           aes(x = flipper_length_mm, y = body_mass_g)) +
      geom_point(color = input$pt_color) +
      theme_minimal()
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

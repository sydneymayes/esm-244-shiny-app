library(shiny)
library(shinythemes)
library(tidyverse)
library(here)
library(dplyr)
library(janitor)
library(sf)
library(tmap)
library(shinyWidgets)
library(stats)
library(plotly)

### CA counties data set from Anna, we'll need to use one with crop type when we're ready
et_counties <- read_csv(here("data","counties_irrigation.csv"))

et_counties_clean <- et_counties %>% 
  clean_names() %>% 
  mutate(pred_et_mm_year = et_mm_year - ag_et_mm_year) %>% 
  select(name, mm_year, et_mm_year, ag_et_mm_year, 
         pred_et_mm_year, irrigation_efficiency, lon, lat)

### Crop type data from Anna (### THIS IS SPACE FOR SYD TO CODE ###)
et_crops <- read_csv(here("data", "bardata.csv"))
et_crops_no_observed <- et_crops %>%
  filter(type != 'ET')


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
               values_to = "values") %>% 
  mutate(values_text = case_when(is.na(values) ~ "No data", 
                                 TRUE ~ round(values, 2) %>% as.character())) %>% 
                                 

  mutate(text = paste0(name, " County", "\n",
                       case_when(
                         var %in% "mm_year" ~ "Irrigation (mm/year)",
                         var %in% "et_mm_year" ~ "Total ET (mm/year)",
                         var %in% "ag_et_mm_year"~ "Agricultural ET (mm/year)",
                         var %in% "pred_et_mm_year" ~ "Simulated Natural ET (mm/year)",
                         var %in% "irrigation_efficiency" ~ "Irrigation Efficiency"),
                       ":", " ", values_text, " ",
                       case_when(
                         var %in% "mm_year" ~ "mm",
                         var %in% "et_mm_year" ~ "mm",
                         var %in% "ag_et_mm_year"~ "mm",
                         var %in% "pred_et_mm_year" ~ "mm",
                         var %in% "irrigation_efficiency" ~ "%"))) 
  # mutate(text = ifelse(is.na(values), "No data", text))




### setting up colors and legend for map on overview tab

color_list <- list(mm_year = c('blue4', 'cyan', 'darkgreen'),
                   et_mm_year = c('firebrick4', 'orange', 'yellow'),
                   ag_et_mm_year = c('seagreen', 'skyblue3', 'plum3'),
                   pred_et_mm_year = c('goldenrod4', 'sienna1', 'saddlebrown'),
                   irrigation_efficiency = c('palegreen3', 'turquoise', 'slateblue'))


legend_list <- list(mm_year = c("Irrigation (mm/yr)"),
                    et_mm_year = c("Total ET (mm/yr)"),
                    ag_et_mm_year = c("Agricultural ET (mm/yr)"),
                    pred_et_mm_year = c("Simulated Natural ET (mm/yr)"),
                    irrigation_efficiency = c("Irrigation Efficiency"))

### Colors for Ashley's plot
color_list_2 <- list(mm_year = c('blue4'),
                   et_mm_year = c('firebrick4'),
                   ag_et_mm_year = c('seagreen'),
                   pred_et_mm_year = c('goldenrod4'),
                   irrigation_efficiency = c('palegreen3'))


### creating regions and region column
cv = c('Butte', 'Colusa', 'Fresno', 'Glenn', 'Kern', 'Kings', 'Madera', 'Merced', 'Placer', 'San Joaquin', 'Sacramento', 'Shasta', 'Solano', 'Stanislaus', 'Sutter', 'Tehama', 'Tulare', 'Yolo', 'Yuba')
nc = c('Alameda', 'Alpine', 'Amador', 'Calaveras', 'Contra Costa', 'Del Norte', 'El Dorado', 'Humboldt', 'Inyo', 'Lake', 'Lassen', 'Marin', 'Mariposa', 'Mendocino', 'Modoc', 'Mono', 'Monterey', 'Napa', 'Nevada', 'Placer', 'Plumas', 'San Benito', 'San Mateo', 'Santa Clara', 'Santa Cruz', 'Sierra', 'Siskiyou', 'Sonoma', 'Trinity', 'Tuolumne')
sc = c('Imperial', 'Los Angeles', 'Orange', 'Riverside', 'San Bernardino', 'San Diego', 'San Luis Obispo', 'Santa Barbara', 'Ventura')

et_counties_mod <- et_counties_clean %>% 
  mutate(region = case_when(name %in% cv ~ "Central Valley",
                            name %in% nc ~ "Northern California",
                            name %in% sc ~ "Southern California"))


### Create the user interface (shiny uses camelCase)
ui <- fluidPage(theme = shinytheme('sandstone'),
  navbarPage("Irrigation Efficiency and Crop Type",

             tabPanel("Overview",      
                      
                      fluidRow(
                        h3("Background"
                          ) ### end of h3
                              ), ### end of fluidRow 
                      fluidRow(
                        "This project is meant to help visualize Agricultural Evapotranspiration (ET) 
                        data provided by Anna Boser, a 3rd year Bren School PhD student. Evapotranspiration refers
                        to the movement of water from the earth to the atmosphere. In this dataset, Agricultural ET is calculated by
                        taking observed ET from satellites and subtracting natural ET (simulated using machine learning). Here we 
                        visualize this data by county and by crop type. Irrigation efficiency is also calculated by dividing Agricultural ET
                        by Total Irrigation.\nBy visualizing this data we can see how much water counties in California are using for agriculture and how efficient they are at using 
                       it. Ultimately, this data can help inform water resource management decisions by highlighting which parts of the 
                        state are using the most water for agriculture, how efficient they are, and which crops are using the most water."
                              ), ### end of fluidRow
                      
                      column(12, align="center",
                             br(),
                             img(src = "https://extension.umn.edu/sites/extension.umn.edu/files/soil-water-components.png", 
                                 width = 500),
                             p("Sourced from University of Minnesota Extension")
                      ) #end of column
                      
                      
                      ), #End "Overview" tabPanel
             
             tabPanel("Map",
                      sidebarLayout(
                        sidebarPanel(
                          radioButtons(inputId = 'pick_variable_map',
                                       label = "Select to view in map",
                                       choices = c("Irrigation (mm/yr)" = "mm_year", 
                                                   "Total ET (mm/yr)" = "et_mm_year", 
                                                   "Agricultural ET (mm/yr)" = "ag_et_mm_year", 
                                                   "Simulated Natural ET (mm/yr)" = "pred_et_mm_year", 
                                                   "Irrigation Efficiency" = "irrigation_efficiency")
                                      ) # end of radioButtons
                                    ), # End of Map sidebarPanel
                        
                        mainPanel(h2("Spatial Exploration of Countywide Irrigation and Evapotranspiration Data", align = "center"),
                                  plotlyOutput(outputId = 'ca_map')
                                  ) ### end mainPanel
                        
                                  ) #end of Map sideBarLayout 
                      ), #End of "Map" tabPanel
             
             
             tabPanel("Counties",
                      sidebarLayout(
                        sidebarPanel(
                          "First select your variable of interest, and then select your counties of interest. The counties dropdown will only show a county if it has data available for a given variable.",
                          tags$head(tags$style(type = "text/css", paste0(".vscomp-dropbox {
                                                        position: absolute !important;
                                                        bottom: 100% !important;
                                                        top: auto !important;
                                                     }}"))),
                          div(style='height:300px'),
                          selectInput(inputId = 'pick_variable',
                                      label = 'Select Variable',
                                      choices = c("Irrigation (mm/yr)" = "mm_year", 
                                                  "Total ET (mm/yr)" = "et_mm_year", 
                                                  "Agricultural ET (mm/yr)" = "ag_et_mm_year", 
                                                  "Natural ET (mm/yr)" = "pred_et_mm_year", 
                                                  "Irrigation Efficiency" = "irrigation_efficiency")
                          ), # end selectInput
                          virtualSelectInput(inputId = "select_county",
                                             label = "Select Counties",
                                             choices = list("Northern California" = nc,
                                                           "Central Valley" = cv,
                                                           "Southern California" = sc),
                                             showValueAsTags = TRUE,
                                             search = TRUE,
                                             multiple = TRUE
                                             ) # end virtualSelectInput
                                     ), #end sidebarPanel
                        
                        mainPanel(h2("Exploring Countywide Irrigation and Evapotranspiration Data", align = "center"),
                                  plotlyOutput(outputId = 'counties_plot')
                                 ) ### end mainPanel

                                    ) # end sidebarLayout
                      ), # end tabPanel 'Counties'

             
            tabPanel("ET by Crop Type",
                      sidebarPanel(
                        tags$head(tags$style(type = "text/css", paste0(".vscomp-dropbox {
                                                        position: absolute !important;
                                                        bottom: 100% !important;
                                                        top: auto !important;
                                                     }}"))),
                                   div(style='height:50px'),

                     virtualSelectInput(inputId = 'pick_et',
                                 label = 'Select Variable',
                                 choices = c( "Agricultural ET (cm/yr)" = "ag_ET",
                                             "Simulated Natural ET (cm/yr)" = "ET_pred"),
                                 showValueAsTags = TRUE,
                                 search = TRUE,
                                 multiple = TRUE,
                                 selected = c("ag_ET", "ET_pred")
                                 ), # end selectInput,

                  

                     awesomeCheckboxGroup(
                       inputId = "select_crop",
                       label = "Select Crop Types",
                       choices = unique(et_crops$cropnames),
                       selected = c("Fallow", "Citrus and subtropical", "Deciduous fruits and nuts",
                                    "Field crops", "Grain and hay crops", "Pasture", "Rice", "Truck, nursery, and berry crops",
                                    "Vineyards", "Young Perennial")
                     ), # end of crop type selectInput
                     
                     radioButtons(inputId = 'pick_crop_photo',
                                  label = "Select to view photo of crop type",
                                  choices = c("Citrus and subtropical" = "citrus",
                                              "Deciduous fruit and nuts" = "nuts",
                                              "Fallow" = "fallow",
                                              "Field crops" = "field",
                                              "Grain and hay crops" = "grain",
                                              "Pasture" = "pasture",
                                              "Rice" = "rice",
                                              "Truck, nursery, and berry crops" = "berry",
                                              "Vineyards" = "vineyard",
                                              "Young Perennial" = "perennial"
                                              ) # end of radioButtons
                     ) # end of radioButtons
                     
                                 ), #end sidebarPanel

                     
                     mainPanel(h2("Exploring Evapotranspiration Data by Crop Type", align = "center"),
                               plotlyOutput(outputId = 'crop_graph'),
                               plotlyOutput(outputId = 'crop_pics')
                               

                               ) ### end mainPanel


                    ), #end crop type tabPanel
             ) #end navbarPage
) # end of fluidPage


### Create the server function
server <- function(input, output, session){
  
### Tab 1 (Rachel)
  
  map_fill <- reactive({
    final_sf %>%
      filter(var == input$pick_variable_map)
  })

  var_color <- reactive({
    color_list %>%
      pluck(input$pick_variable_map)
  })
  
  legend_name <- reactive ({
    legend_list %>% 
      pluck(input$pick_variable_map)
    
  })
  
  output$ca_map <- renderPlotly({
    
  california_map <-
    ggplot() + 
      geom_sf(data = map_fill(), aes(fill = values, geometry = geometry, text = text),
              color = 'black', size = 0.1) +
      theme_void() +
      scale_fill_gradientn(colors = var_color(), na.value = "white")+
      labs(fill = legend_name())
  
  ggplotly(california_map, tooltip = "text") %>% 
    style(hoveron = 'fill')

      
  })
  
### Tab 2 (Ashley)

  observeEvent(input$pick_variable,  {
    input_select_county <- final_sf %>%
      filter(var %in% input$pick_variable, !is.na(values)) %>% 
      select(name)
    updateVirtualSelect("select_county", label = "Select Counties", choices = input_select_county,
                        session = shiny::getDefaultReactiveDomain())
  })

  
  counties_plot_fill <- reactive({
    final_sf %>%
      filter(name %in% input$select_county, var %in% input$pick_variable)
  })
  
  var_color_tab2 <- reactive({
    color_list_2 %>%
      pluck(input$pick_variable)
  })

  
  y_axis <- reactive ({
    legend_list %>% 
      pluck(input$pick_variable)
  })

  output$counties_plot <- renderPlotly({
    counties_map <- ggplot(data = counties_plot_fill(),
                            aes(x = name, y = values, fill = values, text = text),
                            alpha = .6) +
      geom_col(fill = var_color_tab2()) +
      labs(x = "County", y = y_axis()) +
      theme_classic()+
      theme(legend.position = "none")
    
    ggplotly(counties_map, tooltip = "text")
  })
  
### Tab 3 (Syd)
  
  crop_fill <- reactive({
    et_crops_no_observed %>%
      filter(type %in% input$pick_et, cropnames %in% input$select_crop)
  })
  
  
  
  output$crop_graph <- renderPlotly({
    
    # Try with plotly instead of ggplot
    scalar = 1.2 #-- multiply ET by this if you want to convert to cm
    # decided to change back to mm for consistency across tabs
    ggplot() + 
      geom_col(data = crop_fill(), aes(x = cropnames, y = ET, fill = type), alpha = .6) +
      scale_fill_manual(values=c(ag_ET="seagreen", ET_pred="goldenrod4"), 
                        breaks=c("ag_ET","ET_pred"), labels = c("Agricultural ET", "Simulated natural ET")) +
      ylab("mm/year") + 
      theme_classic() + 
      labs(fill='') + 
      theme(axis.text.x = element_text(angle = 30, hjust=1), 
            axis.title.x=element_blank(), 
            axis.ticks.x=element_blank(), 
            legend.position = "top", 
            legend.direction="horizontal", 
            legend.title=element_blank())
    

  })

  crop_photo <- reactive({
    if(input$habitat_type_photo == TRUE){
      image(src = "image_habitat")
    }
    else{
    }
  }
  )
  
  #   ### Define habitat images
  output$image_habitat <- renderImage({
    ### When input$n is 'Urban', filename is ./photos/image_Urban.jpg
    filename <- here('./photos',paste('image_', input$habitat_type_photo, '.jpg', sep=''))
    list(src = filename,
         alt = paste("Habitat type: ", input$habitat_type_photo), width = 500, height = 350)
    
  }, deleteFile = FALSE)
  
  output$crop_pics <- renderImage({
    ### When input$n is 'Urban', filename is ./photos/image_Urban.jpg
    filename <- here('./photos',paste('image_', input$habitat_type_photo, '.jpg', sep=''))
    list(src = filename,
         alt = paste("Crop: ", input$habitat_type_photo), width = 500, height = 350)
    
  }, deleteFile = FALSE)
  })
  
  
  
  
  
} ### end of server function

### Combine these into an app
shinyApp(ui = ui, server = server)

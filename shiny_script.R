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
# et_crops <- read_csv(here("data", "bardata.csv"))
# et_crops_no_observed <- et_crops %>%
#   filter(type != 'ET')










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


### setting up colors and legend for map on overview tab

color_list <- list(mm_year = c('red', 'orange', 'yellow'),
                   et_mm_year = c('green', 'blue', 'purple'),
                   ag_et_mm_year = c('cyan', 'blue', 'purple'),
                   pred_et_mm_year = c('purple', 'pink', 'red'),
                   irrigation_efficiency = c('cyan', 'blue', 'midnightblue'))


legend_list <- list(mm_year = c("Irrigation (mm/yr)"),
                    et_mm_year = c("Total ET (mm/yr)"),
                    ag_et_mm_year = c("Agricultural ET (mm/yr)"),
                    pred_et_mm_year = c("Simulated Natural ET (mm/yr)"),
                    irrigation_efficiency = c("Irrigation Efficiency"))
# 
# legend_list$value <- unlist(legend_list$value)

### (### THIS IS SPACE FOR RACHEL TO CODE ###)



















### End of Rachel's section!

### creating regions and region column
cv = c('Butte', 'Colusa', 'Fresno', 'Glenn', 'Kern', 'Kings', 'Madera', 'Merced', 'Placer', 'San Joaquin', 'Sacramento', 'Shasta', 'Solano', 'Stanislaus', 'Sutter', 'Tehama', 'Tulare', 'Yolo', 'Yuba')
nc = c('Alameda', 'Alpine', 'Amador', 'Calaveras', 'Contra Costa', 'Del Norte', 'El Dorado', 'Humboldt', 'Inyo', 'Lake', 'Lassen', 'Marin', 'Mariposa', 'Mendocino', 'Modoc', 'Mono', 'Monterey', 'Napa', 'Nevada', 'Placer', 'Plumas', 'San Benito', 'San Mateo', 'Santa Clara', 'Santa Cruz', 'Sierra', 'Siskiyou', 'Sonoma', 'Trinity', 'Tuolumne')
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
                      
                      fluidRow(
                        h3("Background"
                        ) ### end of h2
                      ), ### end of fluidRow 
                      fluidRow(
                        "My second row"
                      ), ### end of fluidRow
                      
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
                                     ), # End of Overview sidebarPanel
                        
                        mainPanel("Put my map here!",
                                   plotlyOutput(outputId = 'ca_map')
                                  ) ### end mainPanel
                        
                                    ), #end of Overview sideBarLayout
                      ), #End "Overview" tabPanel
             
             
             
             tabPanel("Counties",
                      sidebarLayout(
                        sidebarPanel(
                          "WIDGETS",
                          tags$head(tags$style(type = "text/css", paste0(".vscomp-dropbox {
                                                        position: absolute !important;
                                                        bottom: 100% !important;
                                                        top: auto !important;
                                                     }}"))),
                          div(style='height:300px'),
                          selectInput(inputId = 'pick_variable',
                                      label = 'Select Variable(s)',
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
                        
                        mainPanel("Put my graph here!",
                                  plotOutput(outputId = 'counties_plot')#,
                                  #tableOutput(outputId = 'counties_table')
                                 ) ### end mainPanel
                        
                                    ) # end sidebarLayout
                      ), # end tabPanel 'Counties'
             
             
             # tabPanel("ET by Crop Type",
             #          sidebarPanel(
             # 
             #          virtualSelectInput(inputId = 'pick_et',
             #                      label = 'Select Variable:',
             #                      choices = c( "Agricultural ET (cm/yr)" = "ag_ET",
             #                                  "Simulated Natural ET (cm/yr)" = "ET_pred"),
             #                      showValueAsTags = TRUE,
             #                      search = TRUE,
             #                      multiple = TRUE,
             #                      selected = c("ag_ET", "ET_pred")
             #                      ), # end selectInput,
             # 
             #          # Not sure how to make all options visible; they currently disappear under the title
             #          # virtualSelectInput(inputId = "select_crop",
             #          #                    label = "Select Crops",
             #          #                    choices = unique(et_crops$cropnames),
             #          #                    showValueAsTags = TRUE,
             #          #                    search = TRUE,
             #          #                    multiple = TRUE
             #          #
             #          #                    ),
             # 
             # 
             # 
             #          # Checkbox instead!
             # 
             #          awesomeCheckboxGroup(
             #            inputId = "select_crop",
             #            label = "Select Crop Types",
             #            choices = unique(et_crops$cropnames),
             #            selected = c("Fallow", "Citrus and subtropical", "Deciduous fruits and nuts",
             #                         "Field crops", "Grain and hay crops", "Pasture", "Rice", "Truck, nursery, and berry crops",
             #                         "Vineyards", "Young Perennial")
             #          ), # end of crop type selectInput
             #                      ), #end sidebarPanel
             # 
             #          mainPanel("Put my graph here!",
             #                    plotlyOutput(outputId = 'crop_graph'),
             # 
             # 
             #                    ) ### end mainPanel
             # 
             # 
             #         ), #end crop type tabPanel
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
    
  
    ggplot() + 
      geom_sf(data = map_fill(), aes(fill = values, geometry = geometry),
              color = 'black', size = 0.1) +
      theme_void() +
      scale_fill_gradientn(colors = var_color(), na.value = "white")+
      labs(fill = legend_name())
      
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
      filter(name %in% input$select_county, var%in% input$pick_variable)
  })
  
  # title <- reactive({
  #   sprintf("Name, Variable: ",
  #           input$name,
  #           input$var)
  # })

  output$counties_plot <- renderPlot({
    
    ggplot() +
      geom_col(data = counties_plot_fill(), aes(x = name, y = values, fill = var), alpha = .6) +
           scale_color_manual(values = c(mm_year = "blue3", et_mm_year = "sandybrown", ag_et_mm_year = "seagreen", pred_et_mm_year = "goldenrod4", 	
                                         irrigation_efficiency = "deepskyblue3"),
                              breaks=c("mm_year", "et_mm_year", "ag_et_mm_year", "pred_et_mm_year", "irrigation_efficiency"),
                              labels = c("Irrigation (mm/yr)", "Total ET (mm/yr)", "Agricultural ET (mm/yr)", "Simulated Natural ET (mm/yr)", "Irrigation Efficiency")) +
      labs(x = "County", y = "{Reactive Variable}") +
      theme_classic()+
      theme(legend.position = "none")
  })
  
### Tab 3 (Syd)
  
  crop_fill <- reactive({
    et_crops_no_observed %>%
      filter(type %in% input$pick_et, cropnames %in% input$select_crop)
  })
  
  
  
  output$crop_graph <- renderPlotly({
    
    # Try with plotly instead of ggplot
    scalar = 1.2
    ggplot() + 
      geom_col(data = crop_fill(), aes(x = cropnames, y = ET*scalar, text = paste("ET:", ET*scalar), fill = type), alpha = .6) +
      scale_fill_manual(values=c(ag_ET="seagreen", ET_pred="goldenrod4"), breaks=c("ag_ET","ET_pred"), labels = c("Agricultural ET", "Simulated natural ET")) +
      ylab("cm/year") + 
      theme_classic() + 
      labs(fill='') + 
      theme(axis.text.x = element_text(angle = 30, hjust=1), 
            axis.title.x=element_blank(), 
            axis.ticks.x=element_blank(), 
            legend.position = "top", 
            legend.direction="horizontal", 
            legend.title=element_blank())
    
  
    
   
    
   
    
    
      
                        
  
  })

  
  
  
  
  
  
  
  
} ### end of server function

### Combine these into an app
shinyApp(ui = ui, server = server)

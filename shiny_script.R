library(shiny)
library(tidyverse)
library(here)
library(dplyr)
library(janitor)

### Create the user interface (shiny uses camelCase)
counties <- read_csv(here("data","counties_irrigation.csv"))

counties_clean <- counties %>% 
  clean_names()

# creating regions and region column
cv = c('Butte', 'Colusa', 'Fresno', 'Glenn', 'Kern', 'Kings', 'Madera', 'Merced', 'Placer', 'San Joaquin', 'Sacramento', 'Shasta', 'Solano', 'Stanislaus', 'Sutter', 'Tehama', 'Tulare', 'Yolo', 'Yuba')
nc = c('Alameda', 'Alpine', 'Amador', 'Calaveras', 'Contra Costa', 'Del Norte', 'El Dorado', 'Humboldt', 'Inyo', 'Lake', 'Lassen', 'Marin', 'Mariposa', 'Mendocino', 'Modoc', 'Mono', 'Monterey', 'Napa', 'Nevada', 'Placer', 'Plumas', 'San Benito', 'San Francisco', 'San Mateo', 'Santa Clara', 'Santa Cruz', 'Sierra', 'Siskiyou', 'Sonoma', 'Trinity', 'Tuolumne')
sc = c('Imperial', 'Los Angeles', 'Orange', 'Riverside', 'San Bernardino', 'San Diego', 'San Luis Obispo', 'Santa Barbara', 'Ventura')

counties_mod <- counties_clean %>% 
  mutate(region = case_when(name %in% cv ~ "Central Valley",
                            name %in% nc ~ "Northern California",
                            name %in% sc ~ "Southern California"))


ui <- fluidPage(
  titlePanel("I am adding a title!"),
  sidebarLayout(
    sidebarPanel("Put my widgets here!",
                 radioButtons(inputId = 'crop', ### make sure variable name is correct
                              label = "Choose crop type!",
                              choices = c("x", "y", "z")),
                 "Choose a color",
                 selectInput(inputId = 'pt_color',
                             label = 'choose your favorite color!',
                             choices = c('Awesome Red' = 'red',
                                         'pretty purple' = 'purple',
                                         'OOOOOrange' = 'orange'))
    ), ### end sidebarPanel
    mainPanel("Put my graph here!",
              plotOutput(outputId = 'penguin_plot'),
              tableOutput(outputId = 'penguin_table')
    ) ### end mainPanel
  ) ### end sidebarLayout here
) ### end fluidPage here

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
}

### Combine these into an app
shinyApp(ui = ui, server = server)

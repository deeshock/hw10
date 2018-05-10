library(shiny)
library(dplyr)
library(tidyverse)
library(tidycensus)

census_api_key("")
##Please type in your api key above
ui <- fluidPage(
  
  titlePanel("American Community Survey"),
  
  
  sidebarLayout(
    
    
    sidebarPanel(
      
      selectInput("State", "State",
                  choices = state.abb,
                  selected = "NJ"),
      
      radioButtons("Type", "Type",
                   choices = list("median_gross_rent",
                                  "median_household_income",
                                  "ratio"), 
                   selected = "ratio")
    ),
    
    
    mainPanel(plotOutput("Plot"))
  )
)


server <- function(input, output) {
  
  reduced_df <- reactive({
    get_acs(
      geography = "tract",
      variables = c(median_gross_rent = "B25064_001" , median_household_income = "B19013_001"),
      state = input$State,
      geometry = TRUE
    )%>%
      .[, -5]%>%
      data.frame()%>% 
      spread(key = variable, value = estimate)%>% 
      mutate(ratio = median_gross_rent / median_household_income)
  })
  
  output$Plot <- renderPlot({
    reduced_df() %>% 
      ggplot(aes_string(fill = input$Type)) + geom_sf()
  })
  
}


shinyApp(ui = ui, server = server)
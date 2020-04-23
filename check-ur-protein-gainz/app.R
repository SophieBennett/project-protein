# libs ----------------------------------------------------

library(shiny)
library(shinyWidgets)
library(dplyr)
library(magrittr)
library(readr)
library(formattable)

# data ----------------------------------------------------

proteins <- read_csv("data/protein-content-food.csv") %>% 
  select("Food Type" = class,
         "Food" = food,
         "Protein (g) per 100g" = protein_per_100g)

# set-up ui -----------------------------------------------
ui <- fluidPage(
  theme = shinythemes::shinytheme("flatly"),
  
                
  # Application title
  headerPanel(strong("Gains Checker")),
  
  sidebarLayout(
    sidebarPanel(
      helpText("Enter a food type to check protein content per 100g."),
      
      textInput("food",
                label = "Enter a food here:"),
      
      selectInput("category",
                  label = "Select a food category here:",
                  choices = c("All", proteins$`Food Type`),
                  selected = "All")
    ),
    
    mainPanel(
      formattableOutput("proteinTable")
    )
  )
)

# define server logic --------------------------------
server <- function(input, output) {
  
  output$proteinTable <- renderFormattable({
    req(input$food)
    
    if(input$category == "All") {
      
      proteins %>% 
        filter(stringr::str_detect(tolower(Food), tolower(input$food))) %>% 
        select(`Food`, `Food Type`, `Protein (g) per 100g`) %>% 
        formattable::formattable(
          align = "l",
          list(`Protein (g) per 100g` = color_tile("#ffe1e1", "#ff9d9d")))
      
    } else {
    
    proteins %>% 
      filter(stringr::str_detect(tolower(Food), tolower(input$food))) %>% 
      filter(`Food Type` == input$category) %>% 
      select(`Food`, `Food Type`, `Protein (g) per 100g`) %>% 
      formattable::formattable(
        align = "l",
        list(`Protein (g) per 100g` = color_tile("#ffe1e1", "#ff9d9d")))
    }
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)



## Should table be hidden before items have been searched?

## Add a way to select top n from list?
## Add a way to filter by class?
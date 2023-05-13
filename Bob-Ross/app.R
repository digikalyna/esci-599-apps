## -------------------------------------------
## Bob Ross App v.0.1.2
## KD 2023-5-12
## test app
## load and wrangle data
## user chooses a painting
## painting is displayed
## -------------------------------------------

##------------------------------------------------------------------------------
## packages and data
library(shiny)
library(tidyverse)

bob_ross_col <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')
bob_ross_obj <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

##------------------------------------------------------------------------------
## R code
## tidy the data

bob_ross_obj <- bob_ross_obj %>% 
  select(-EPISODE, -TITLE)

bob_ross <- bind_cols(bob_ross_col, bob_ross_obj) %>% 
  rename_with(str_to_lower)

##------------------------------------------------------------------------------
## Shiny App
## Define UI 
ui <- fluidPage(
  
  selectInput("painting", 
              label = "Which painting would you like to see?", 
              choices = bob_ross$painting_title),
  
  textOutput("paint_pryvit"),
  htmlOutput("show_painting")
  
)

## Define server logic 
server <- function(input, output, session) {
  
  img_url <- reactive(
    which(bob_ross$painting_title == input$painting)
    )
  
  output$paint_pryvit <- renderText({
    paste0("Hello ", input$painting, "!")
    })
  
  output$show_painting <- renderText({
    c('<img src="', bob_ross$img_src[img_url()],'">')
    })
}

## Run the application 
shinyApp(ui = ui, server = server)

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
library(shinythemes)
library(tidyverse)

bob_ross_col <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2023/2023-02-21/bob_ross.csv')
bob_ross_obj <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2019/2019-08-06/bob-ross.csv")

##------------------------------------------------------------------------------
## R code
##------------------------------------------------------------------------------
##-------------------------------------------
## tidy the data
##-------------------------------------------

bob_ross_obj <- bob_ross_obj %>% 
  select(-EPISODE, -TITLE)
## put together dfs, col lower case, make logicals
bob_ross <- bind_cols(bob_ross_col, bob_ross_obj) %>% 
  rename_with(str_to_lower) %>% 
  mutate(across(28:94, as.logical))

##-------------------------------------------
## functions
## edit 2023-05-17: make list of func to write;
## wrote 2 functions
##-------------------------------------------

## get the index number for a particular painting
get_row_from_title <- function(title) {
  return(which(bob_ross$painting_title == title))
}

## returns a list of colors that are in the painting
get_colors_from_index <- function(index) {
  bob_ross %>% 
    select(10:27) %>% 
    slice(index) %>%
    keep(function(x) {return(x == TRUE)}) %>% 
    colnames() %>% 
    str_replace_all("_", " ") %>% 
    str_to_title %>% 
    sort
}

## returns a list of objects that are in the painting
get_objs_from_index <- function(index) {
  bob_ross %>% 
    select(28:94) %>% 
    slice(index) %>%
    keep(function(x) {return(x == TRUE)}) %>% 
    colnames() %>% 
    str_replace_all("_", " ") %>% 
    str_to_title %>% 
    sort
}

## returns TRUE if painting has given color
does_idx_have_color <- function(index, color) {
  ## TODO
}

## returns TRUE if painting has given object
does_idx_have_obj <- function(index, obj) {
  ## TODO
}

## other ideas: random button, search by frame?

##------------------------------------------------------------------------------
## Shiny App

##-------------------------------------------
## Define UI 
## addition 2023-5-17: title, layout stuff
## note- can't have fluidrows with tabsets :(
##-------------------------------------------
ui <- fluidPage(theme = shinytheme("sandstone"),
  titlePanel("Happy Accidents"),
  fluidRow(
    column(4,
           selectInput(
             "title",
             label = "Which painting would you like to see?",
             choices = bob_ross$painting_title),
    ),
    
    column(8, 
           textOutput("paint_pryvit"),
           htmlOutput("show_painting")
    ),
  ),
  
  fluidRow(
    column(6,
           textOutput("title_colors"),
           textOutput("list_of_colors")
    ),
    column(6,
           textOutput("title_objs"),
           textOutput("list_of_objs")
    ),
  )
)

##-------------------------------------------
## Define server logic 
##-------------------------------------------
server <- function(input, output, session) {
  
  row_index <- reactive(
    get_row_from_title(input$title)
  )
  
  img_colors <- reactive(
    get_colors_from_index(row_index())
  )
  
  img_objs <- reactive(
    get_objs_from_index(row_index())
  )
  
  output$title_colors <- renderText({
    paste("This painting features the following colors: ")
  })
  
  output$list_of_colors <- renderText({
    paste(img_colors(), collapse = ", ")
  })
  
  output$title_objs <- renderText({
    paste0("This painting features the following objects: ")
  })
  
  output$list_of_objs <- renderText({
    paste(img_objs(), collapse = ", ")
  })
  
  output$show_painting <- renderText({
    c('<img src="', bob_ross$img_src[row_index()],'">')
  })
}

##-------------------------------------------
## Run the application 
##-------------------------------------------
shinyApp(ui = ui, server = server)

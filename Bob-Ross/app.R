## -------------------------------------------
## Bob Ross App 
## KD 2023-5-12
## test app
## load and wrangle data
## user chooses a painting
## painting is displayed
## 2023-05-14: 2 functions
## 2023-05-17: extra info about paintings
## 2023-06-05: random button; start of attribute panel
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

## create named vector of colors
col_bob_ross <- names(bob_ross[, 10:27])
names(col_bob_ross) <- col_bob_ross %>% 
  str_replace_all("_", " ") %>% 
  str_to_title

## create named vector of colors
obj_bob_ross <- names(bob_ross[, 28:94])
names(obj_bob_ross) <- obj_bob_ross %>% 
  str_replace_all("_", " ") %>% 
  str_to_title
  

##-------------------------------------------
## functions
##-------------------------------------------

## get random painting title
get_random_title <- function() {
  sample(bob_ross$painting_title, 1)
}

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
##-------------------------------------------
ui <- navbarPage("Happy Accidents", theme = shinytheme("sandstone"),
  tabPanel("By Painting",
    fluidRow(
      column(4,
        selectInput(
          "title_of_painting",
          label = "Which painting would you like to see?",
          choices = bob_ross$painting_title
        ),
        actionButton(
          "random_title_button", 
          "Choose a painting for me", 
          class = "btn-block"
        ),
      ),
      
      column(8,
        textOutput("paint_pryvit"),
        htmlOutput("show_painting")
      ),
    ),
    
    fluidRow(style = "padding-top: 20px;",
      column(6,
        htmlOutput("title_colors"),
        textOutput("list_of_colors")
      ),
      column(6,
        htmlOutput("title_objs"),
        textOutput("list_of_objs")
      ),
    )
  ),
  
  tabPanel("By Attributes",
    fluidRow(
      column(6,
        selectInput(
          "color_title",
          multiple = TRUE,
          label = "What color(s) would you like to explore?",
          choices = col_bob_ross
        ),
      ),   
      column(6,
        selectInput(
          "object_title",
          multiple = TRUE,
          label = "What object(s) would you like to explore?",
          choices = obj_bob_ross
        ),
      ),
    ),
    fluidRow(
      column(6, offset = 3,
        textOutput("col_obj_vector")
      )
    )
  )
)

##-------------------------------------------
## Define server logic 
##-------------------------------------------
server <- function(input, output, session) {
  
  ## by painting tab

  row_index <- reactive(
    get_row_from_title(input$title_of_painting)
  )
  
  img_colors <- reactive(
    get_colors_from_index(row_index())
  )
  
  img_objs <- reactive(
    get_objs_from_index(row_index())
  )
  
  output$title_colors <- renderText({
    HTML("<strong>This painting features the following colors: </strong>")
  })
  
  output$list_of_colors <- renderText({
    paste(img_colors(), collapse = ", ")
  })
  
  output$title_objs <- renderText({
    HTML("<strong>This painting features the following objects: </strong>")
  })
  
  output$list_of_objs <- renderText({
    paste(img_objs(), collapse = ", ")
  })
  
  output$show_painting <- renderText({
    c('<img src="', bob_ross$img_src[row_index()],'">')
  })
  
  ## To save dynamic randomized title
  rv <- reactiveValues()
  
  ## randomized button
  ## from https://stackoverflow.com/questions/75490869/
  ## r-shiny-actionbutton-to-generate-random-value-from-input-values-and-render-plot
  observeEvent(input$random_title_button, {
    ## Select a random title
    rv$title_of_painting <- get_random_title()
    ## Update the value of dropdown with random title selected
    updateSelectInput(session, inputId = "title_of_painting",
                      label = "Which painting would you like to see?",
                      choices = bob_ross$painting_title, 
                      selected = rv$title_of_painting)
  })
  
  ## by attributes tab
  
  col_obj_vector <- reactive(
    c(unname(input$color_title), input$object_title)
  )
  
  output$col_obj_vector <- renderText({
    col_obj_vector()
  })
  
  
}

##-------------------------------------------
## Run the application 
##-------------------------------------------
shinyApp(ui = ui, server = server)

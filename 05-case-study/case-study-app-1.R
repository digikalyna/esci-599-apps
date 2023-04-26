## -------------------------------------------
## Assignment 5 - Case study app v 1.0
## KD 2023-4-25
## Reproduce code from Mastering Shiny, Ch. 4
## -------------------------------------------

## -------------------------------------------
## packages
library(dplyr)
library(ggplot2)
library(forcats)
library(vroom)
library(shiny)

## -------------------------------------------
## check to see if data exists
if (!exists("injuries")) {
  injuries <- vroom::vroom("injuries.tsv.gz")
  products <- vroom::vroom("products.tsv")
  population <- vroom::vroom("population.tsv")
}

## -------------------------------------------
## Begin UI
ui <- fluidPage(
  
  
  ## first row
  fluidRow(
    ## product drop down
    ## wishlist: switch diagnosis and product?
    
    column(8,
           selectInput("code", "Product",
                       choices = setNames(products$prod_code, 
                                          products$title),
                       width = "100%"
           )
    ),
    ## y axis drop down
    column(2, selectInput("y", "Y axis", c("rate", "count")))
  ),
  
  ## second row: three tables
  ## how would I get product as table output?
  fluidRow(
    column(4, tableOutput("diag")),
    column(4, tableOutput("body_part")),
    column(4, tableOutput("location")),
  ),
  ## third row: plot
  fluidRow(
    column(12, plotOutput("age_sex"))
  ),
  ## fourth row: tell me a story button
  fluidRow(
    column(2, actionButton("story", "Once upon a time...")),
    column(10, textOutput("narrative"))
  )

)

## -------------------------------------------
## function count_top: truncate tables
## convert the variable to a factor, 
## order by the frequency of the levels, 
## lump together all levels after the top 5
count_top <- function(df, var, n = 5) {
  df %>%
    mutate({{ var }} := fct_lump(fct_infreq({{ var }}), n = n)) %>%
    group_by({{ var }}) %>%
    summarise(n = as.integer(sum(weight)))
}

## -------------------------------------------
## Begin SERVER
server <- function(input, output, session) {
  
  ## reactive statement
  selected <- reactive(injuries %>% 
                         filter(prod_code == input$code))
  
  ## table outputs
  ## wishlist: figure out how to change column names
  output$diag <- renderTable(count_top(selected(), diag), 
                             width = "100%")
  output$body_part <- renderTable(count_top(selected(), body_part), 
                                  width = "100%")
  output$location <- renderTable(count_top(selected(), location), 
                                 width = "100%")

  ## reactive statement
  summary <- reactive({
    selected() %>%
      count(age, sex, wt = weight) %>%
      left_join(population, by = c("age", "sex")) %>%
      mutate(rate = n / population * 1e4)
  })
  
  ## plot
  output$age_sex <- renderPlot({
    if (input$y == "count") {
      summary() %>%
        ggplot(aes(age, n, colour = sex)) +
        geom_line() +
        scale_color_manual(values = c("#ec8b00", "#0f859f")) +
        labs(y = "Estimated number of injuries") +
        theme_minimal()
    } else {
      summary() %>%
        ggplot(aes(age, rate, colour = sex)) +
        geom_line(na.rm = TRUE) +
        scale_color_manual(values = c("#ec8b00", "#0f859f")) +
        labs(y = "Injuries per 10,000 people") +
        theme_minimal()
    }
  }, res = 96)

  ## tell me a story button
  narrative_sample <- eventReactive(
    list(input$story, selected()),
    selected() %>% 
      pull(narrative) %>% sample(1)
  )
  output$narrative <- renderText(narrative_sample())
}

## -------------------------------------------
## Run the app
shinyApp(ui, server)

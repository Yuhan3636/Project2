# app.R

library(shiny)
source("api.R")   # or source("R/api.R") if you put api.R inside an R/ folder

all_breeds <- get_all_breeds()$breed

ui <- fluidPage(
  titlePanel("Dog CEO Explorer"),
  sidebarLayout(
    
    sidebarPanel(
      selectInput("breed", "Choose a breed:", choices = all_breeds),
      uiOutput("sub_ui"),
      numericInput("n", "Number of images:", 1, min = 1, max = 10),
      actionButton("go", "Fetch images")
    ),
    
    mainPanel(uiOutput("images"))
  )
)

server <- function(input, output, session) {
  
  output$sub_ui <- renderUI({
    subs <- get_sub_breeds(input$breed)
    if (length(subs)) selectInput("sub_breed", "Choose sub-breed:", subs)
  })
  
  imgs <- eventReactive(input$go, {
    if (!is.null(input$sub_breed)) {
      get_n_images_by_sub_breed(input$breed, input$sub_breed, input$n)
    } else {
      get_n_images_by_breed(input$breed, input$n)
    }
  })
  
  output$images <- renderUI({
    req(imgs())
    tags$div(
      lapply(imgs()$url, function(u) {
        tags$img(src = u, style = "max-width:200px; margin:4px;")
      })
    )
  })
}

shinyApp(ui, server)

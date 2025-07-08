# app.R

#–– load packages and your API functions
library(shiny)
library(tibble)
source("R/dog_api.R")   

#–– grab the list of breeds on startup
all_breeds <- get_all_breeds()$breed

ui <- fluidPage(
  titlePanel("Dog CEO Image Explorer"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput(
        "breed", "Choose a breed:", 
        choices = all_breeds, selected = all_breeds[1]
      ),
      
      # placeholder for sub‐breed selector
      uiOutput("sub_ui"),
      
      numericInput(
        "n", "Number of images:", 
        value = 1, min = 1, max = 10, step = 1
      ),
      
      actionButton("go", "Fetch images")
    ),
    
    mainPanel(
      uiOutput("images")  # we’ll render the <img> tags here
    )
  )
)

server <- function(input, output, session) {
  
  # Dynamically show a sub‐breed dropdown if this breed has sub‐breeds
  output$sub_ui <- renderUI({
    subs <- get_sub_breeds(input$breed)
    if (length(subs) == 0) return(NULL)
    
    selectInput("sub_breed", "Choose a sub‐breed:", choices = subs)
  })
  
  # Fetch the data when the button is clicked
  fetched <- eventReactive(input$go, {
    # if a sub‐breed was chosen, hit that endpoint
    if (!is.null(input$sub_breed) && input$sub_breed != "") {
      get_n_images_by_sub_breed(input$breed, input$sub_breed, input$n)
    } else {
      # no sub‐breed: use the breed‐only endpoints
      if (input$n == 1) {
        get_random_image_by_breed(input$breed)
      } else {
        get_n_images_by_breed(input$breed, input$n)
      }
    }
  })
  
  # Render the images as HTML <img> tags
  output$images <- renderUI({
    df <- fetched()
    # produce one <img> per URL
    imgs <- lapply(df$url, function(u) {
      tags$img(src = u, style = "max-width: 200px; margin: 5px;")
    })
    # wrap in a div
    do.call(tags$div, imgs)
  })
}

shinyApp(ui, server)

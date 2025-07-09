# app.R

library(shiny)
library(DT)
library(dplyr)
library(ggplot2)
source("api.R")   

# Preload breed list
all_breeds <- get_all_breeds()$breed

ui <- navbarPage(
  "Dog CEO Explorer",
  
  ## ── 1) About Tab ──────────────────────────────────────────────────────────────
  tabPanel("About",
           fluidPage(
             
             # a little header row with logo + title/description
             fluidRow(
               column(3,
                      tags$img(
                        src    = "logo.png",      # make sure www/logo.png exists
                        alt    = "Dog CEO logo",
                        height = "120px",
                        style  = "display:block; margin:auto;"
                      )
               ),
               column(9,
                      h2("Dog CEO Explorer"),
                      p(
                        "Dog CEO Explorer is an R/Shiny application that lets you browse",
                        "random dog images by breed and sub-breed.  Under the hood it uses",
                        a("Dog CEO’s Dog API", href = "https://dog.ceo/dog-api/", target = "_blank"),
                        "– the world’s largest open-source collection of dog photos."
                      )
               )
             ),
             
             hr(),
             
             # tell the user about each tab
             h3("App Structure"),
             tags$ul(
               tags$li(strong("About:"), "You’re here—learn what the app does, where the data comes from, and how to navigate."),
               tags$li(strong("Data Download:"), "Select a breed (and sub-breed if available), fetch raw image URLs or images, subset rows/columns, and download a CSV of your selection."),
               tags$li(strong("Data Exploration:"), "View summary tables and four kinds of plots (histogram, bar–ranked by sub‐breed count, boxplot, and heatmap) with dynamic faceting and bin controls.")
             )
             
           )
  ),
  

  
  ## ── 2) Data Download Tab ─────────────────────────────────────────────────────
  tabPanel("Data Download",
           sidebarLayout(
             sidebarPanel(
               selectInput("dd_breed", "Breed:", choices = all_breeds),
               uiOutput("dd_sub_ui"),
               numericInput("dd_n", "How many images?", 1, min = 1, max = 10),
               actionButton("dd_go", "Fetch Data"),
               hr(),
               # Column chooser
               uiOutput("col_ui"),
               # Download button
               downloadButton("download_data", "Download CSV")
             ),
             mainPanel(
               DTOutput("raw_table")
             )
           )
  ),
  
  ## ── 3) Data Exploration Tab ──────────────────────────────────────────────────
  tabPanel("Data Exploration",
           sidebarLayout(
             sidebarPanel(
               selectInput("plot_type",
                           "Plot type:",
                           c("Histogram","Bar","Boxplot","Heatmap")),
               selectInput("facet_var",
                           "Facet by:",
                           c("None","first_letter","has_sub")),
               # Numeric input for histogram bins
               conditionalPanel(
                 "input.plot_type == 'Histogram'",
                 sliderInput("bins", "Bins:", min=5, max=50, value=20)
               )
             ),
             mainPanel(
               plotOutput("explore_plot")
             )
           )
  )
)

server <- function(input, output, session) {
  
  ## -- About tab has no server logic --
  
  ## -- Data Download tab logic --
  output$dd_sub_ui <- renderUI({
    subs <- get_sub_breeds(input$dd_breed)
    if (length(subs))
      selectInput("dd_sub_breed", "Sub-breed:", choices = subs)
  })
  
  # Fetch raw tibble
  dd_data <- eventReactive(input$dd_go, {
    if (!is.null(input$dd_sub_breed)) {
      get_n_images_by_sub_breed(input$dd_breed, input$dd_sub_breed, input$dd_n)
    } else {
      if (input$dd_n == 1) {
        get_random_image_by_breed(input$dd_breed)
      } else {
        get_n_images_by_breed(input$dd_breed, input$dd_n)
      }
    }
  })
  
  # Column chooser (though we only have one column here)
  output$col_ui <- renderUI({
    cols <- colnames(dd_data())
    checkboxGroupInput("cols", "Columns to keep:", choices = cols, selected = cols)
  })
  
  # Render the table
  output$raw_table <- renderDT({
    req(dd_data())
    df <- dd_data()[, input$cols, drop = FALSE]
    datatable(df, options = list(pageLength = 5))
  })
  
  # Download handler
  output$download_data <- downloadHandler(
    filename = function() {
      paste0("dog_images_", Sys.Date(), ".csv")
    },
    content = function(file) {
      write.csv(dd_data()[, input$cols, drop = FALSE], file, row.names = FALSE)
    }
  )
  
  ## -- Data Exploration tab logic --
  # Recompute a summary data frame on demand
  summary_df <- reactive({
    df <- get_all_breeds() %>%
      mutate(
        first_letter = substr(breed, 1, 1),
        sub_count    = lengths(sub_breeds),
        has_sub      = sub_count > 0
      )
    df
  })
  
  output$explore_plot <- renderPlot({
    df <- summary_df()  
    
    # Build the base plot
    p <- switch(input$plot_type,
                "Histogram" = ggplot(df, aes(x = sub_count)) +
                  geom_histogram(bins = input$bins, color = "grey30") +
                  labs(title = "Histogram of Sub‐Breed Counts",
                       x = "Number of sub‐breeds", y = "Number of breeds"),
                "Bar"       = df %>%
                  arrange(desc(sub_count)) %>%
                  slice_head(n = 15) %>%
                  ggplot(aes(x = reorder(breed, sub_count), y = sub_count)) +
                  geom_col(fill = "steelblue") + coord_flip() +
                  labs(title = "Top 15 Breeds by Number of Sub‐Breeds",
                       x = "Breed", y = "Number of Sub‐Breeds"),
                "Boxplot"   = ggplot(df, aes(x = "", y = sub_count)) +
                  geom_boxplot(fill = "lightgreen") +
                  labs(title = "Boxplot of Sub‐Breed Counts",
                       x = NULL, y = "Sub‐Breed Count"),
                "Heatmap"   = {
                  hm <- df %>% count(first_letter, has_sub)
                  ggplot(hm, aes(x = first_letter, y = has_sub, fill = n)) +
                    geom_tile(color = "white") +
                    geom_text(aes(label = n)) +
                    labs(title = "Breeds by Letter & Sub‐Breed Presence",
                         x = "First Letter", y = NULL)
                }
    )
    
    # Only facet if the user chose something other than "None"
    if (input$facet_var != "None") {
      fac_formula <- as.formula(paste0("~", input$facet_var))
      p <- p + facet_wrap(fac_formula)
    }
    
    # Print the ggplot object
    print(p)
  })
  
}

shinyApp(ui, server)

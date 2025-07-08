# app.R

library(shiny)
library(DT)
library(dplyr)
source("api.R")   # or "R/dog_api.R"

# Preload breed list
all_breeds <- get_all_breeds()$breed

ui <- navbarPage(
  "Dog CEO Explorer",
  
  ## ── 1) About Tab ──────────────────────────────────────────────────────────────
  tabPanel("About",
           fluidPage(
             h2("About Dog CEO Explorer"),
             p("This app lets you browse random dog images by breed and sub-breed, using the free Dog CEO API."),
             p("Data source: ", a("https://dog.ceo/dog-api/", href="https://dog.ceo/dog-api/", target="_blank")),
             p("**Tabs:**"),
             tags$ul(
               tags$li(strong("Data Download:"), "Pick breed/sub-breed, fetch & view the raw data, subset and download as CSV."),
               tags$li(strong("Data Exploration:"), "Choose summary statistics and plot types (histogram, bar, boxplot, heatmap).")
             ),
             img(src="https://dog.ceo/assets/img/logo.svg", height="100px")  # Dog CEO logo!
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
    
    # Apply faceting if requested
    facet_expr <- switch(input$facet_var,
                         "first_letter" = facet_wrap(~first_letter),
                         "has_sub"      = facet_wrap(~has_sub),
                         NULL
    )
    
    p <- switch(input$plot_type,
                "Histogram" = ggplot(df, aes(x = sub_count)) +
                  geom_histogram(bins = input$bins, color = "grey30") +
                  labs(title = "Histogram of Sub‐Breed Counts",
                       x = "Number of sub‐breeds", y = "Number of breeds"),
                "Bar"       = df %>%
                  count(breed) %>%
                  top_n(15, n) %>%
                  ggplot(aes(reorder(breed, n), n)) +
                  geom_col(fill = "steelblue") + coord_flip() +
                  labs(title = "Top 15 Breeds by Count", x = "", y = "Count"),
                "Boxplot"   = ggplot(df, aes(y = sub_count, x = "")) +
                  geom_boxplot(fill = "lightgreen") +
                  labs(title = "Boxplot of Sub‐Breed Counts", y = "Count", x = ""),
                "Heatmap"   = {
                  hm <- df %>%
                    count(first_letter, has_sub)
                  ggplot(hm, aes(x = first_letter, y = has_sub, fill = n)) +
                    geom_tile() + geom_text(aes(label = n)) +
                    labs(title = "Breeds by Letter & Sub‐Breed Presence",
                         x = "First Letter", y = "")
                }
    )
    
    # Add facet if needed
    if (!is.null(facet_expr)) p <- p + facet_expr
    p
  })
}

shinyApp(ui, server)

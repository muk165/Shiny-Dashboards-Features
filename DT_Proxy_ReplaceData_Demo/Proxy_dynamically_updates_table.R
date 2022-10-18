library(shiny)
library(tidyverse)
library(DT)

df <- dplyr::tibble(Height = "185", Weight = "95")

ui <- fluidPage(
  
  # App title ----
  titlePanel("DT + Proxy + Replace Data"),
  
  # Sidebar layout with input and output definitions ----
  sidebarLayout(
    
    # Sidebar panel for inputs ----
    sidebarPanel(
      
      # Input: Slider for the number of bins ---- 
      shiny::textInput(inputId = "height", label = "height"),
      shiny::textInput(inputId = "weight", label = "weight"),
      
      shiny::actionButton(inputId = "add", label = "Add"),
      
      shiny::selectInput(inputId = "remove_row", label = "Remove Row",
                         choices = 1:nrow(df)),
      
      shiny::actionButton(inputId = "remove", label = "Remove")
      
    ),
    
    # Main panel for displaying outputs ----
    mainPanel(
      
      # Output: Histogram ----
      DT::DTOutput(outputId = "table")
      
    )
  )
)



# Define server logic required to draw a histogram ----
server <- function(input, output, session) {
  
  mod_df <- shiny::reactiveValues(x = df)
  
  output$table <- DT::renderDT({
    
    isolate(mod_df$x)
    
  })
  
  shiny::observe({
    shiny::updateSelectInput(session, inputId = "remove_row",
                             choices = 1:nrow(mod_df$x))
  })
  
  shiny::observeEvent(input$add, {
    
    mod_df$x <- mod_df$x %>%
      dplyr::bind_rows(
        dplyr::tibble(Height = input$height,
                      Weight = input$weight)
      )
    
  })
  
  shiny::observeEvent(input$remove, {
    
    mod_df$x <- mod_df$x[-as.integer(input$remove_row), ]
    
  })
  
  proxy <- DT::dataTableProxy('table')
  shiny::observe({
    
    DT::replaceData(proxy, mod_df$x)
    
  })
  
  
}

shinyApp(ui, server)
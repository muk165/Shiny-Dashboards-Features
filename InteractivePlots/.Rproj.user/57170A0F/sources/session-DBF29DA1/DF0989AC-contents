library(shiny)
library(ggplot2) 
library(tidyverse)
library(lubridate)
library(plotly)
library(shinythemes)
library(DT)
library(shiny) # shiny features
library(shinydashboard) # shinydashboard functions
library(DT)  # for DT tables
library(dplyr)  # for pipe operator & data manipulations
library(plotly) # for data visualization and plots using plotly 
library(ggplot2) # for data visualization & plots using ggplot2
library(ggtext) # beautifying text on top of ggplot
library(maps) # for USA states map - boundaries used by ggplot for mapping
library(ggcorrplot) # for correlation plot
library(shinycssloaders) # to add a loader while graph is populating
library(bslib) # have options for dashboard theme
##########################
##### User interface #####
##########################
ui <- fluidPage(
  theme = shinytheme("sandstone"),
  navbarPage(
    ##########
    ## Page 1
    ##########
    "Interactivity in Visualizations",
    tabPanel(
      "Home",
      sidebarLayout(
        sidebarPanel(
          h3(
            "Exploring Interactivity Possibilities between different visualization and tables", 
            style = "padding-bottom: 20px font: bold;"
          )
        ),
        mainPanel(
          a(
            href="https://figshare.shef.ac.uk/articles/dataset/Hadfield_Green_Roof_5-year_Dataset/11876736", 
            "Click here for data!"
          ),
        )
      )
    ),
    ##########
    ## Page 2
    ##########
    tabPanel(
      "Interactive Scatter Plots",
      navlistPanel(
        "Two Versions",
        tabPanel("Select any row in the Table",
                 box(fluidRow(
                   
                   column(6, DT::dataTableOutput('x1')),
                   column(6, plotOutput('x2', height = 500))
                 ), width =12
                 )
        ),
        tabPanel("Select a region in Scatter plot", 
                 plotOutput(outputId = "graph", brush = "plot_brush"), # brush ID is plot_brush
                 # fluidRow(
                 #   h1(""),
                 #   column(width = 4, tags$b(tags$i("Actual Dataset")), tableOutput("data"))
                 # ),
                 fluidRow(
                   column(width=12, tags$b(tags$i("Rows corresponding to datapoints under brush")),
                          tableOutput("data_brush"), offset = 2)
                 )
        ),
      )
    ),
    ##########
    ## Page 3
    ##########
    
    tabPanel(
      "Filter Box Plot",
      sidebarLayout(
        sidebarPanel(
          h4("Filtering the data using box plot. Select the region of that you want to filter")
        ),
        mainPanel(
          # brush argument will enable the brush, sends the data point information to the server side
          # at the server side the data points under the brush related information can be read through input$BRUSHID
          plotOutput(outputId = "boxplot", brush = "plot_brush_"), # brush ID is plot_brush, brush argument enables the brush
          fixedRow(
            # left side is the actual dataset and right side the rows for datapoints selected by brush
            # defined the width of each column and also some styling (bold & italics) using tags
            column(width= 5, tags$b(tags$i("Original Dataset")),  tableOutput("data1")),
            column(width = 5, tags$b(tags$i("Filtered Dataset")), tableOutput("data2"), offset = 2)
          )
        )
      )
    ),
    ############
    ## Nav menu
    ############
    navbarMenu(
      "More",
      tabPanel(
        "Nonthing yet",
        sidebarLayout(
          sidebarPanel(
          ),
          mainPanel(
          
          )
        )
      )
    )
  )
)


###########################
##### Server function #####
###########################
server <- function(input, output, session) {
  ###############Interactivity function with box plot
  ## using the mtcars dataset
  mtcars1 = mtcars %>% 
    select('mpg','cyl','disp')
  mtcars1$cyl = as.factor(mtcars1$cyl)
  mt <- reactiveValues(data=mtcars1) # making the dataset reactiveValues so that any changes in mt$data later could be reflected throughout
  
  
  # Create boxplot object plotOutput function is expecting
  output$boxplot <- renderPlot({
    ggplot(mt$data, aes(cyl, mpg))+
      geom_boxplot(outlier.colour = "red") +# outlier.colour = "red" makes the outlier points red in color
      ylab('Miles per Gallon')+
      xlab('Number Of Cylinders')+
      coord_flip()
  })
  
  ## Returns the actual dataset
  output$data1 <- renderTable({
    mtcars1
  })
  
  ## Returns the updated state of the dataset
  output$data2 <- renderTable({
    mt$data # using the reactive object
  })
  
  
  # Observe function
  # listening to brushing event
  # input$plot_brush_ fetches the information (rows) corresponding to data points under the brush
  # using the reactive object mt$data so that any changes done on it reflects to other reactive functions where it is used
  observe({
    df = brushedPoints(mt$data, brush = input$plot_brush_, allRows = TRUE) 
    mt$data = df[df$selected_== FALSE,  ] #Taking only those data points where the selected_ value is FALSE (alternatively ignoring rows with selected_ = TRUE status)
  })
  
  
  
  
  #####################Interactivity function with Scatter plots
  ###scatter plot 1
  #interactive scatter plot
  output$x1 = DT::renderDataTable(cars, server = FALSE)
  
  # highlight selected rows in the scatterplot
  output$x2 = renderPlot({
    s = input$x1_rows_selected
    # cars %>% 
    #   ggplot(aes(speed,dist)) +
    #   geom_point()
    plot(cars)
    if (length(s)) {
      points(cars[s,,drop = FALSE], pch = 19, cex = 2)
    }
  })
  
  
  ###scatter plot 2
  # scatter plot the mtcars dataset - mpg vs hp
  output$graph <- renderPlot({
    ggplot(data = mtcars, aes(x = mpg, y = hp)) +
      geom_point()
  })
  
  
  # # To display the mtcars dataset on the left side in the app
  # output$data <- renderTable({
  #  head(mtcars)
  # })
  #brushed points
  output$data_brush <-  renderTable({
    n = nrow(brushedPoints(mtcars, brush = input$plot_brush)) # row count will be 0 when no selection made by the brush
    if(n==0)  
      return()
    else
      brushedPoints(mtcars, brush = input$plot_brush) # return rows
    # argument allRows = TRUE can also be used
    ## It will add another column (selected_) to the actual dataset. True indicates that data point 
    # corresponding to that row was under the brush. False means data corresponding to that row wasn't selected by brush
  })
  
}

##################################
##### Call shinyApp function #####
##################################
shinyApp(ui = ui, server = server)
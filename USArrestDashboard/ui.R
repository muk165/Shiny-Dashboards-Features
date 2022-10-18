## Shiny UI component for the Dashboard

#title color change
dashBoard_main_title <- tags$a(href='',
                               icon("tachometer-alt"),
                              "Shiny Dashboard",target = "_blank",
                              style = "color:white;")


dashboardPage(
  skin = "purple",
  dashboardHeader( title=dashBoard_main_title, 
                  #titleWidth = 0, 
                  tags$li(class="dropdown",tags$a(href="https:.youtube.com/", icon("youtube"), 
                                                  "My Channel", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://www.linkedin.com/in/mukteshg/" ,
                                                  icon("linkedin"), "My Profile", target="_blank")),
                  tags$li(class="dropdown",tags$a(href="https://github.com/", icon("github"), 
                                                  "Source Code", target="_blank"))
  ),
  
  dashboardSidebar(
    # div(class='',selectInput(inputId = "var1" , label ="Select the Variable" , choices = c("Hi","test"))),
    sidebarMenu(id = "sidebar",
                textOutput("counter"),
                menuItem("Dataset", tabName = "data", icon = icon("database")),
                menuItem("Visualization", tabName = "viz", icon=icon("chart-line")),
                
                # Conditional Panels for conditional widget appearance
                # Filter should appear only for the visualization menu and selected tabs within it
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'distro'",
                                 selectInput(inputId = "var1" , label ="Select the Variable" , 
                                  choices = c1)),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'trends' ",
                                 selectInput(inputId = "var2" , label ="Select the Arrest type" , 
                                             choices = c2)),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ",
                                 selectInput(inputId = "var3" , label ="Select the X variable" ,
                                             choices = c1, selected = "Rape")),
                conditionalPanel("input.sidebar == 'viz' && input.t2 == 'relation' ", 
                  selectInput(inputId = "var4" , label ="Select the Y variable" , choices = c1, 
                              selected = "Assault")),
                menuItem("Choropleth Map", tabName = "map", icon=icon("map")),
                menuItem("Interactive Scatter Plot",tabName = 'scater_table'),
                menuItem("Interactive Scatter Plot - 2 ",tabName = 'brushpoints')
    )
  ),
  
  
  dashboardBody(
    tags$head(tags$style(HTML(
      '.myClass { 
        font-size: 20px;
        line-height: 50px;
        text-align: left;
        font-family: "Helvetica Neue",Helvetica,Arial,sans-serif;
        padding: 0 15px;
        overflow: hidden;
        color: white;
      }
    '))),
    tags$script(HTML('
      $(document).ready(function() {
        $("header").find("nav").append(\'<span class="myClass"> Exploring US Arrest Data - 1973 </span>\');
      })
     ')),
    tabItems(
      ## First tab item
      tabItem(tabName = "data", 
              tabBox(id="t1", width = 12, 
                     tabPanel("About", icon=icon("address-card"),
                              fluidRow(
                                column(width = 8, tags$img(src="crime.jpg", width =500 , height = 300),
                                       tags$br() , 
                                       tags$a("Photo by Campbell Jensen on Unsplash"), align = "center"),
                                column(width = 4, tags$br() ,
                                       tags$p("This data set comes along with base R and contains statistics, in arrests per 100,000 residents for assault, murder, and rape in each of the 50 US states in 1973. Also, given is the percent of the population living in urban areas.")
                                )
                              )
                              
                              
                     ), 
                     tabPanel("Data", dataTableOutput("dataT"), icon = icon("table")), 
                     tabPanel("Structure", verbatimTextOutput("structure"), icon=icon("uncharted")),
                     tabPanel("Summary Stats", verbatimTextOutput("summary"), icon=icon("chart-pie"))
              )
              
      ),  
      
      # Second Tab Item
      tabItem(tabName = "viz", 
              tabBox(id="t2",  width=12, 
                     tabPanel("Crime Trends by State", value="trends",
                              fluidRow(tags$div(align="center", box(tableOutput("top5"), title = textOutput("head1") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE)),
                                       tags$div(align="center", box(tableOutput("low5"), title = textOutput("head2") , collapsible = TRUE, status = "primary",  collapsed = TRUE, solidHeader = TRUE))
                                       
                              ),
                              withSpinner(plotlyOutput("bar"))
                     ),
                     tabPanel("Distribution", value="distro",
                              # selectInput("var", "Select the variable", choices=c("Rape", "Assault")),
                              withSpinner(plotlyOutput("histplot", height = "350px"))),
                     tabPanel("Correlation Matrix", id="corr" , withSpinner(plotlyOutput("cor"))),
                     tabPanel("Relationship among Arrest types & Urban Population", 
                              radioButtons(inputId ="fit" , label = "Select smooth method" , choices = c("loess", "lm"), selected = "lm" , inline = TRUE), 
                              withSpinner(plotlyOutput("scatter")), value="relation"),
                     side = "left"
              ),
              
      ),
      
      
      # Third Tab Item
      tabItem(
        tabName = "map",
        box( selectInput("crimetype","Select Arrest Type", choices = c2, selected="Rape", 
                         width = 250),
             h3("Arrests per 100,000 residents by state"),
                  withSpinner(plotOutput("map_plot")), width = 12)
      ),
      tabItem(
        tabName = "scater_table",
        h4("Select a row to highlight points in scatter plot"),
        box(fluidRow(
          
          column(6, DT::dataTableOutput('x1')),
          column(6, plotOutput('x2', height = 500))
        ), width =12
        )
      ),
      
      tabItem(
        tabName = "brushpoints",
        plotOutput(outputId = "graph", brush = "plot_brush"), # brush ID is plot_brush
        # fluidRow(
        #   h1(""),
        #   column(width = 4, tags$b(tags$i("Actual Dataset")), tableOutput("data"))
        # ),
        fluidRow(
          column(width=12, tags$b(tags$i("Rows corresponding to datapoints under brush")),
                 tableOutput("data_brush"), offset = 2)
        )
        
      )
      
      
    )
  )
)




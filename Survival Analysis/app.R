source("global.r") 
#defines Ui
ui <- shinyUI(fluidPage( 
    #Css styling
   tags$head(tags$style(
     HTML('
          #sidebar {
             background-color: white;
             text-align: center;
             margin:0;
             padding:0;
          }
          #mainpanel{
          text-align: center;
          margin:0;
          padding-right:4px;
          padding-left: 0px;
          }
        ')
    )),
  
    # Application title
    titlePanel(
      h2("Oncology Dashboard",
      style = "font-weight: bold; color: #3e9fb5; text-align: center; margin-bottom:0px;
      padding-bottom:0px")),

   fluidRow(width=12, 
   sidebarLayout( 
      column(width=3,
        sidebarPanel(id="sidebar", width = 12,
           div( style = "box-shadow: rgba(0, 0, 0, 0.35) 0px 5px 15px;",
             h3("KPI Selection", style = "font-size: 20px; background-color: #5f9da0; 
               color: white; height: 30px"),
             h3("Kaplan Meier Curve", style = "font-weight: bold; font-size: 13px; background-color: #5f9da0; 
                color: white; height: 15px; margin-top: 0px"),
            div( style = "text-align: left; margin-left: 20px; margin-right:0px;",
              radioButtons("kpi_selection", "", kaplan_metric_choices, "Gender")
           )),
           div( style = "box-shadow: rgba(0, 0, 0, 0.35) 0px 5px 15px;",
            h3("Disease Stage", style = "font-weight: bold; font-size: 13px; background-color: #5f9da0; 
               color: white; height: 15px; margin-top: 8px"),
           div(style = "text-align: left; margin-left: 20px; margin-right:0px; height: 300px;",
             checkboxGroupInput('disease_stage',"",c(1,2,3,4), selected = c(1,2,3,4))
           ))
           
        ),
      ),
       
      column(width = 9,
        mainPanel( id="mainpanel", width = 12,
          div(style = "box-shadow: rgba(0, 0, 0, 0.35) 0px 5px 15px; height: 264px",
              h3("Kaplan Meieir Curve (Survival Analysis)",
              style = "font-size: 20px; background-color: #5f9da0; margin-top: 20px;
                       color: white; height: 30px"),
            plotlyOutput("km_plot")
          ),
          div(style = "box-shadow: rgba(0, 0, 0, 0.35) 0px 5px 15px; height: 350px",
              h3("Drug Efficacy Analysis",
                 style = "font-size: 20px; background-color: #5f9da0; margin-top: 20px;
                       color: white; height: 30px"),
             plotlyOutput("swim_plot")
          )
        )
      )
   )
  )
))

# server logic has 2 observe event and 2 render plots outputs
server <- function(input, output) {

  #reactive values to be updated when there is change in filters
  km_model <- reactiveValues(data = NULL)  #for kpi selection filter
  swim_data <- reactiveValues(data = NULL)  # for disease stage filter
  swim_reshaped <- reactiveValues(data = NULL) # for disease stage filter
  
  #observe event for KPI Selection
  observeEvent(input$kpi_selection,
               { #fitting kaplan model conditionally based on the kpi selection input filter
                 if(input$kpi_selection == "Gender"){
                   km_model$data <- survfit(Surv(period,censor) ~ Gender,
                                            data = modified_clinical, 
                                            type="kaplan-meier")
                 }else if(input$kpi_selection == "Race"){
                   km_model$data <- survfit(Surv(period,censor) ~ Race,
                                            data = modified_clinical, 
                                            type="kaplan-meier")
                 }else if(input$kpi_selection == "Ethnicity"){
                   km_model$data <- survfit(Surv(period,censor) ~ Ethnicity,
                                            data = modified_clinical, 
                                            type="kaplan-meier")
                 }else if(input$kpi_selection == "Prior Treatment"){
                   km_model$data <- survfit(Surv(period,censor) ~ modified_clinical$`Prior Treatment`,
                                            data = modified_clinical, 
                                            type="kaplan-meier")
                 }else if(input$kpi_selection == "Prior Malignancy"){
                   km_model$data <- survfit(Surv(period,censor) ~ modified_clinical$`Prior Malignancy`,
                                            data = modified_clinical, 
                                            type="kaplan-meier")
                 }
               })
  
  observeEvent(input$disease_stage,
               { #filter on Stage
                 swim_data$data <- swimmer_data %>% 
                   filter(Stage %in% input$disease_stage )
                 
                 #reshaped_data
                 swim_reshaped$data <- melt(swim_data$data %>% 
                                select(Subject,Months, Complete,Partial, Durable),
                              id.var = c("Subject","Months"), na.rm = F)
               })
 
  
#kaplan plot output
 output$km_plot <- renderPlotly({
   plt <- ggsurvplot(km_model$data,conf.int = 'F',xlab = "Time (in Days)" )
   ggplotly(plt[[1]]) %>% layout(height = 220, width = 950)
 })
 
 #swimmer plot output
 output$swim_plot <- renderPlotly({
   swim_g_plot <- ggplot( swim_data$data, aes(Subject, Months)) +
     geom_bar(stat="identity", aes(fill=factor(Stage)), width=0.8) +
     geom_point(data=swim_reshaped$data, 
                aes(Subject, value, colour=variable, size = 5,shape = variable)) +
     guides(size = FALSE)+
     geom_segment(data= swim_data$data %>% filter(Continued==1), 
                  aes(x=Subject, xend=Subject, y=Months + 0.1, yend=Months + 1),
                  pch=15, size=0.8, arrow=arrow(type="closed", length=unit(0.1,"in"))) +
     coord_flip() +
     scale_colour_manual(values=c(hcl(seq(15,375,length.out=3)[1:2],100,30),"black")) +
     scale_y_continuous(limits=c(-1,20), breaks=0:20) +
     labs(fill="Disease Stage",x="Subjects on Treatment Drug A") +
     theme_bw() +
     theme(panel.grid.minor=element_blank(),
           panel.grid.major=element_blank(),
           axis.text.y=element_blank(),
           axis.ticks.y=element_blank())+
     guides(shape=guide_legend(title=""))
  
   #handling object not found error when stage 4 is not selected as continued column will not have 1 as a value
   #removing geom_segment layer
   if("4" %!in% input$disease_stage){
     #swim_g_plot[[2]] <- NULL
     swim_g_plot <- ggplot( swim_data$data, aes(Subject, Months)) +
       geom_bar(stat="identity", aes(fill=factor(Stage)), width=0.8) +
       geom_point(data=swim_reshaped$data, 
                  aes(Subject, value, colour=variable, size = 5,shape = variable)) +
       guides(size = FALSE)+
       coord_flip() +
       scale_colour_manual(values=c(hcl(seq(15,375,length.out=3)[1:2],100,30),"black")) +
       scale_y_continuous(limits=c(-1,20), breaks=0:20) +
       labs(fill="Disease Stage", x="Subjects on Treatment Drug A") +
       theme_bw() +
       theme(panel.grid.minor=element_blank(),
             panel.grid.major=element_blank(),
             axis.text.y=element_blank(),
             axis.ticks.y=element_blank())+
       guides(shape=guide_legend(title=""))
   }
   
   ggplotly(swim_g_plot)  %>% layout(height = 310, width = 950) #to render as a ggplotly object in UI
   })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

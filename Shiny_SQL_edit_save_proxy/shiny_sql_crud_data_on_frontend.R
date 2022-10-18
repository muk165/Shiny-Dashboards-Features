#libraries required
library(tidyverse)
library(rvest)
library(janitor)
library(RSQLite)
library(DBI)
library(shiny)
library(shinyWidgets)
library(DT)


 #Purpose: For coaches to leave comments in the nba matches table
#creating connection
db_con <- dbConnect(
  drv = RSQLite::SQLite(),
  here::here("D:/RShniy/Shiny-SQL-Integeration/nba_playoffs.db")
)

dbListTables(db_con) #list the tables in the database
# ##disconnect
# dbDisconnect(db_con)

games_available <- tbl(db_con, "game_ids") %>% 
  pull(game_info)


### Shiny app with an auto refresh
ui <- fluidPage(
  
  title = "2021 Playoffs",
  
  sidebarPanel(
    
    pickerInput( 
      inputId = "game_selection",
      label = "Select Game:",
      choices = games_available
    ),
  ),
  
  mainPanel(
    DTOutput(outputId = "pbp_table"),
    uiOutput(outputId = "commit_button_display")
  )
  
)


server <- function(input, output, session){
  
  displayedData <- reactiveValues()
  
  observeEvent(input$game_selection,{
    req(input$game_selection)
    
    ## connect to database
    db_con <- dbConnect(
      drv = RSQLite::SQLite(),
      here::here("D:/RShniy/Shiny-SQL-Integeration/nba_playoffs.db")
    )
    
    ##disconnect when reactive finishes
    on.exit(dbDisconnect(db_con))
    
    #fetching ga,e id  for the game selection
    game_id <- tbl(db_con, "game_ids") %>%  #to fetch the gameid for the game selected
      filter( game_info == local(input$game_selection)) %>% 
      pull(game_id)
    
    comments <- tbl(db_con, paste0("game_comments_",game_id)) #to fetch the comments table for that game id
    
    pbp <- tbl(db_con, paste0("game_pbp_",game_id)) %>% # to fetch the matches record
      left_join(comments, by = "play_id_num") %>%  #left join with the comments table so that a comment column is created with Nulls/ if values were provided earlier
      arrange(play_id_num)
  #  browser()
    #reactive values to be used in other reactive calls
    displayedData$game_id <- game_id
    displayedData$comments <- comments %>% collect() 
    displayedData$pbp <- pbp %>% collect() 
    
    displayedData$comment_commit_up_to_date <- TRUE # to keep a flag that a commit is in place
    
  })
  
  
  ##the table that is outputted
  output$pbp_table <- renderDT({
    displayedData$pbp
  },
  selection = 'none',
  rownames = FALSE,
  editable = TRUE)
  
  ## when updated, 
  proxy = dataTableProxy('pbp_table')
  
  
  #dt + proxy + reactivity
  observeEvent(input$pbp_table_cell_edit,{
    
    info = input$pbp_table_cell_edit
    i = info$row
    j = info$col + 1  # column index offset by 1
    v = info$value
    
    ## only comment column can be edited
    if(colnames(displayedData$pbp)[j] == "comment"){
      
      play_num <- displayedData$pbp$play_id_num[i]
      
      displayedData$comments <- bind_rows(
        displayedData$comments[!displayedData$comments$play_id_num == play_num,],
        data.frame(
          play_id_num = play_num, 
          comment = v
        )
      )
      
      displayedData$pbp <- displayedData$pbp %>% 
        select(-comment) %>% 
        left_join(displayedData$comments, by = "play_id_num") %>% 
        arrange(play_id_num)
      
      displayedData$comment_commit_up_to_date <- FALSE
      
    }
    
    replaceData(proxy, displayedData$pbp, resetPaging = FALSE, rownames = FALSE)
    
  })
  
  output$commit_button_display <- renderUI({
    if(!displayedData$comment_commit_up_to_date){
      actionButton("commit","click to save comments")
    }
  })
  
  ## commit updates and share comments
  observeEvent(input$commit,{
    
    db_con <- dbConnect(
      drv = RSQLite::SQLite(),
      here::here("D:/RShniy/Shiny-SQL-Integeration/nba_playoffs.db")
    )
    
    ##disconnect when reactive finishes
    on.exit(dbDisconnect(db_con))
    
    dbWriteTable(
      db_con,
      name = paste0("game_comments_", displayedData$game_id),
      as.data.frame(displayedData$comments),
      overwrite = TRUE
    )
    
    displayedData$comment_commit_up_to_date <- TRUE
    
    
  })
  
}


shinyApp(ui, server)
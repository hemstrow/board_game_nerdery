library(shiny)

# badlist <- c("Pandemic Legacy: Season 1", "Blood Rage", "7 Wonders")

load("data/games_info.rda")

ui <- fluidPage(
  sliderInput("playernumber", value = 4, label = "Number of Players", min = 1, max = 10),
  actionButton("go", "Find Games"),
  
  DT::dataTableOutput("table"),
  uiOutput("issues")
  
  
)


server <- function(input, output, session) {
  game_tab <- eventReactive(eventExpr = input$go, {
    hit_games[which(input$playernumber >= hit_games$MinPlayers & input$playernumber <= hit_games$MaxPlayers),]
  })
  
  output$table <- DT::renderDataTable({
    DT::datatable(game_tab(), class = 'cell-border stripe', escape = FALSE)
  })
  
  output$issues <- renderUI({
    tagList("Some games available on BGA may not appear due to mismatched names. This and other issues can be reported to the app github", a("Issues Page.", href="https://github.com/hemstrow/board_game_nerdery/issues"))
  })
}
shinyApp(ui, server)



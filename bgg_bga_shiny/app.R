library(shiny)

# badlist <- c("Pandemic Legacy: Season 1", "Blood Rage", "7 Wonders")

load("data/games_info.rda")

ui <- fluidPage(
  sliderInput("playernumber", value = 4, label = "Number of Players", min = 1, max = 10),
  actionButton("go", "Find Games"),
  
  dataTableOutput("table")
)


server <- function(input, output, session) {
  game_tab <- eventReactive(eventExpr = input$go, {
    hit_games[which(input$playernumber >= hit_games$MinPlayers & input$playernumber <= hit_games$MaxPlayers),]
  })
  
  output$table <- renderDataTable({
    data.table::as.data.table(game_tab())
  }, escape = FALSE)
}
shinyApp(ui, server)



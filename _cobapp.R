
# global.R ----

library(shiny)
library(shinydashboard)


# ui.R ----

ui <- dashboardPage(
  dashboardHeader(),
  dashboardSidebar(),
  dashboardBody()
)


# server.R ----

server <- function(input, output){
  
}

# khusus app.R ----

shinyApp(ui = ui, server = server)
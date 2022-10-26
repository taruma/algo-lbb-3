# -> RUBRIK LBB 3 <-
# - (1 Point) Tahapan data pre-processing menggunakan dplyr 1 > filter/arange/mutate/groupby...
# - (1 Point) Plot yang ditampilkan pada dashboard sudah interaktif > cukup pakai plotly atau ggplotly(...)
# - (1 Point) Setiap plot yang ditampilkan menampilkan informasi yang relevan dari dashboard > informasinya apa?

library(shiny)

ui <- shiny::fluidPage(
  
  # App title
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

}

# Run the application 
shinyApp(ui = ui, server = server)

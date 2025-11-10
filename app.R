library(shiny)
source("ui.R")
if (!file.exists("server.R")) {
  server <- function(input, output, session) { }
} else {
  source("server.R") # defines server
}
shinyApp(ui = ui, server = server)
#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

shinyApp(
  ui = fluidPage(
    includeHTML(rmarkdown::render("index.Rmd"))),
  server = function(input, output) {
  }
)

#to deploy app:
#library(rsconnect)
#rsconnect::deployApp()
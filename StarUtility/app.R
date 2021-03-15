#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(shinythemes)
library(tidyverse)
library(janitor)
library(lubridate)
library(png)
library(shinydashboard)

utility <- read_csv("data/example.csv") %>% 
  clean_names() %>% 
  mutate(production=al_well_1+al_well_2+al_well_3+al_well_4+post_reservoir_meter+canyon_lake) %>% 
  mutate(date=as.Date(date)) %>% 
  mutate(wur=wur_well_1+wur_well_2+wur_well_3+wur_well_4+wur_well_5+wur_well_6) %>% 
  mutate(asp = al_well_1+al_well_2+al_well_3+al_well_4)

# Define UI for application
ui <- fluidPage(
  
  theme = shinytheme("flatly"),
  #Application title
  titlePanel("Star Water Utility"),
  
  navbarPage(" ",
             
             tabPanel("Summary",
                      h2("What does this App explore?"),
                      p("The MESM class of 2020 is 70% women and 30% men. Does this lead to 70% participation by women and 30% by men? Our app explores how gender affects participation in Bren core classes in the Fall of 2018. We visualize changes in participation by gender as the quarter progresses, estimate the probabilty of women and men participating under different circumstances, and calculate probabilities of different gender identities being called on based on the professor's gender."),
                        img(src= "star_smaller.png", align = "center")
                      )
                      
             ),
  
#)
             
             tabPanel("Daily Participation",
                      #sidebar with input widgets
                      sidebarLayout(
                        sidebarPanel(
                         dateRangeInput("date_range",
                                        label = "Select Date Range",
                                         start = "2012-01-01",
                                         end = "2012-12-31",
                                         min = "2012-01-01",
                                         max = "2012-12-31"))),
                   #       ),
                    #      checkboxGroupInput("time", 
                     #                        label = "Select Student Gender Preference",
                      #                       choices = list("Man" = "m", "Woman" = "w"),
                       #                      selected = "m"
                        #  )
                   #     ),
                       # main panel
                        mainPanel( 
                          h2("Student Participation in Core Classes"),
                          plotOutput("time_plot")
                        )
                      #)
                      
             ))

server <- function(input, output) {
  
  #reaction for time graph widgets
  datareact_time <- reactive({
    utility %>% 
      mutate(date = mdy(date))
      select(date, production) %>% 
      filter(date >= input$date_range[1], date <= input$date_range[2]) 
  })
  
  # time series panel
  output$time_plot <- renderPlot(
    {
      ggplot(datareact_time(), aes(x = date, y=production))+
        geom_line() +
       # scale_fill_manual(limits = c("m", "w"), values = c("royalblue4", "darkolivegreen4"), name = "Student Gender Preference", labels = c("Man", "Woman")) +
        theme_classic() +
        scale_y_continuous(expand = c(0,0), limits = c(0,80), breaks = seq(0,80, by = 5)) +
        scale_x_date(breaks = as.Date(c("2012-01-01", "2012-04-01", "2012-07-01", "2012-10-01", "2012-12-31"))) +
        theme(axis.text.x = element_text(angle = 90, hjust = 1))+
        labs(x = "Date", y = "Production (MG)") 
    }
  )
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
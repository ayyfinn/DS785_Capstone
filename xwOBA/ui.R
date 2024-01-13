#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinydashboard)

load("data_shiny.RData")

shinyUI(fluidPage(

    # Application title
    titlePanel("Projected Batter/Pitcher Matchup xwOBA (2023 Season) from Statcast Variables"),

    fluidRow(
      column(3,
          selectInput(inputId ='name',
                      label = "Choose Batter",
                      choices = sort(unique(nn_garson_stack$Player)),
                      selected = NULL
          )),
      column(3,
          selectInput(inputId ='name2',
                      label = "Choose Pitcher",
                      choices = sort(transpose_final.df$Pitchers),
                      selected = NULL
          )
        )),
    

        mainPanel(
            plotOutput("distPlot",width = "100%")
        ),
    
    )
    
)

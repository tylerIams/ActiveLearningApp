
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(glmnet)
options(shiny.maxRequestSize = 1000*1024^2)

shinyUI(fluidPage(

  # Application title
  titlePanel("Welcome to Active Learning"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    sidebarPanel(
      tags$h4(radioButtons("init", "Are images featurized properly?",
                           c("Yes", "No"), "")),
      uiOutput("file"),
      uiOutput("showImg"),
      tableOutput("table"),
      textOutput("detectImages"),
      uiOutput("featurize"),
      uiOutput("Featurize"),
      uiOutput("continue"), 
      hr(),
      uiOutput("label"),
      uiOutput("image"),
      tags$h4(textOutput("labelTextInfo")),
      tags$h4(textOutput("imageTextInfo")),
      uiOutput("genMod")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(id = "tabactice", type = "tabs",
                    tabPanel("Plot", 
                             tags$h3(textOutput("acc")),
                             plotOutput("round"),
                             uiOutput("slider"),
                             hr(),
                             uiOutput("afterPlot")
                    ),
                    tabPanel("Least Confident Data",
                             tableOutput("getDatToLab"),
                             uiOutput("numToExport"),
                             uiOutput("exportNeedLabs")),
                    tabPanel("Image", 
                             tags$h4(textOutput("canYouLabel")),
                             uiOutput("img"),
                             uiOutput("applyLabel"),
                             uiOutput("saveLabel"),
                             tags$h3(textOutput("saveSuccessful")),
                             tags$h4(textOutput("goToNextRound"))),
                    tabPanel("View Dataset",
                             tableOutput("getView"), 
                             tags$h4(textOutput("dfSummary"))
                             ),
                    tabPanel("View Featurized dataset",
                             tableOutput("getViewfeaturized")
                             )
        )
      )
    )

    )
)


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
      uiOutput("continue"), 
      hr(),
      uiOutput("label"),
      uiOutput("image"),
      tags$h4(textOutput("labelTextInfo")),
      tags$h4(textOutput("imageTextInfo")),
      uiOutput("confirmSelection")
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
        tabsetPanel(id = "tabactice", type = "tabs",
                    tabPanel("Plot", 
                             uiOutput("finished_now_Export"),
                             uiOutput("finalize"),
                             uiOutput("genMod"),
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
                             hr(),
                             uiOutput("cheatLabeling"),
                             hr(),
                             uiOutput("applyLabel"),
                             uiOutput("saveLabel"),
                             tags$h3(textOutput("saveSuccessful")),
                             tags$p(textOutput("numLabelsInfo")),
                             hr(),
                             uiOutput("goToNextRound")),
                    tabPanel("View Dataset",
                             tableOutput("getView"),
                             hr(),
                             tags$h4(textOutput("dfSummary")),
                             hr(),
                             hr(),
                             uiOutput("continueTolabel")
                             ),
                    tabPanel("View Featurized dataset",
                             tableOutput("getViewfeaturized")
                             ),
                    tabPanel("Size of Labeled Data",
                             tags$h4(textOutput("sizeCheck")),
                             uiOutput("sizeInfo"),
                             hr(),
                             hr(),
                             uiOutput("StartLabeling"),
                             uiOutput("StartActice")
                    ),
                    tabPanel("Active Learning Labeling",
                             tags$h4("Can you label this image?"),
                             uiOutput("showImage"),
                             uiOutput("applyAL_Label"),
                             uiOutput("saveAL_Label"),
                             uiOutput("ALsaveSuccessful")
                             # tags$h4(textOutput("goToNextRound"))
                    )
        )
      )
    )

    )
)


# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(glmnet)
options(shiny.maxRequestSize = 30*1024^2)

shinyUI(fluidPage(

  # Application title
  titlePanel("Welcome to Active Learning"),

  # Sidebar with a slider input for number of bins
  sidebarLayout(
    
    sidebarPanel(
      tags$h4(radioButtons("init", "Do you have featurized data?",
                           c("Yes", "No"), "")),
      uiOutput("file")
    ),
    
    
    # Show a plot of the generated distribution
    mainPanel(
      tableOutput("table"),
      textOutput("dfSummary"),
      uiOutput("continue"),
      uiOutput("label"),
      uiOutput("image"),
      textOutput("labelTextInfo"),
      textOutput("imageTextInfo"),
      uiOutput("genMod"),
      tableOutput("round")
    )
  
    
    )
))

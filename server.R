
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)

shinyServer(function(input, output) {

  # Renders the initial input
  output$file <- renderUI({
    req(input$init)
    if (input$init == "Yes") {
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))  
    } else if (input$init == "No") {
      fileInput("file1", "Choose Directory with Images",
                multiple = TRUE,
                accept = c("image/.png", "image/.jpeg")) 
    }
    
  })
  
  # Creates the table
  output$table <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath)
    return(df[1:10,])
  })
  
  output$label <- renderUI({
    req(input$file1)
    df <- read_csv(input$file1$datapath)
    selectInput("label", "Please Select the Column Containing Labels: ", 
                choices = colnames(df))
  })
  
  output$labelTextInfo <- renderText({
    req(input$label)
    df <- read_csv(input$file1$datapath)
    return(str_c("You have selected column ", input$label, " as your labels."))
  })
  
  
})

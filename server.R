
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)

df <- NULL
label <- NULL
active_set <- NULL
candidate_set <- NULL

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
    df <<- read.csv(input$file1$datapath)
    return(df[1:10,])
  })
  
  output$label <- renderUI({
    req(input$file1)
    selectInput("label", "Please Select the Column Containing Labels: ", 
                choices = colnames(df))
  })
  
  output$labelTextInfo <- renderText({
    req(input$label)
    sel <- df %>% select(input$label) %>% na.omit()
    num <- nrow(sel)
    return(str_c("You have selected column ", input$label, " as your labels.  This has ", num, " labels."))
  })
  
  output$genMod <- renderUI({
    req(input$label)
    label <- str_c(input$label)
    active_set <<- df %>% na.omit()
    candidate_set <<- df %>% filter(is.na(label) == TRUE)
    actionButton("mod", "Generate Model")
  })
  
  output$round <- renderTable({
    req(input$mod)
    x_train <- model.matrix(~ ., select(active_set, -label))
    y_train <- active_set$label
    active_model <- glmnet(x_train, y_train, alpha=0.0, 
                            lambda=0.1,
                            family="multinomial")
    x_test <- model.matrix(~ ., select(candidate_set, -label))
    category_prob <- predict(active_model, newx=x_test, type="response")
    max_probs <- apply(category_prob, 1, FUN = max)
    candidate_set <- cbind(candidate_set, max_probs)
    candidate_set <- candidate_set %>% arrange(max_probs)
    return(candidate_set[1:10,])
  })
  
})

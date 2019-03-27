
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(png)
source("modeling_functions.R")

df <- NULL
label <- NULL
active_set <- NULL
candidate_set <- NULL
imCol <- NULL
labCol <- NULL



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
    num <- ncol(df)
    sec <- num -1
    return(df[1:5,sec:num])
  })
  
  output$dfSummary <- renderText({
    req(input$file1)
    pt1 <- str_c("Your data contains ", nrow(df), " images with ", ncol(df), " features.")
    pt2 <- NULL
    if ("image" %in% colnames(df)) {
      pt2 <- str_c("Your data contains an image column called image.")
      imCol <<- TRUE
    } else {
      pt2 <- str_c("Your data does not contain an image column called image.")
    }
    pt3 <- NULL
    if ("label" %in% colnames(df)) {
      pt3 <- str_c("Your data contains a column of labels called label.")
      labCol <<- TRUE
    }
    else {
      pt3 <- str_c("Your data does not contain a column of labels called label.")
    }
    fin <- str_c(pt1, pt2, pt3, sep = " ")
    return(fin)
  })
  
  output$continue <- renderUI({
    req(input$file1)
    actionButton("continue", "Continue")
  })
  
  output$label <- renderUI({
    req(input$continue)
    if (labCol == FALSE) {
      selectInput("label", "Please Select the Column Containing Labels: ", 
                  choices = colnames(df))
    } else {
      selectInput("label", "Column Containing Labels: ", 
                  choices = "label")
    }
  })
  
  output$image <- renderUI({
    req(input$continue)
    if (imCol == FALSE) {
      selectInput("image", "Please select the column containing the image filenames", 
                  choices = colnames(df))
    } else {
      selectInput("image", "Image filenames", 
                  choices = "image")
    }
  })
  
  output$labelTextInfo <- renderText({
    req(input$continue)
    sel <- df %>% select(input$label) %>% na.omit()
    num <- nrow(sel)
    return(str_c("Column ", input$label, " will be used as your labels.  
                 This has ", num, " labels."))
  })
  
  output$imageTextInfo <- renderText({
    req(input$continue)
    sel <- df %>% select(input$image) %>% na.omit()
    num <- nrow(sel)
    return(str_c("Column ", input$image, " has your image files.  
                 This has ", num, " images."))
  })
  
  output$genMod <- renderUI({
    req(input$continue)
    label <- str_c(input$label)
    active_set <<- df %>% na.omit()
    candidate_set <<- df %>% filter(is.na(label) == TRUE)
    actionButton("mod", "Generate Model")
  })
  
  output$round <- renderPlot({
    req(input$mod)
    tab <- createModels(active_set)
    tab <- tab %>% mutate(ROUND = factor(ROUND),
                          FOLD = factor(FOLD))
    #Here is where you could incorporate past data
    plot <- ggplot(data = tab, aes(x = ROUND, y = ACCURACY, color = FOLD)) + geom_point()
    return(plot)
  })
  
  output$afterPlot <- renderUI({
    req(input$mod)
    actionButton("cont", "Get Data To Label")
  })
  
  output$getDatToLab <- renderTable({
    req(input$cont)
    candidate_set <- findDataToLabel(candidate_set)
    candidate_set_table <- candidate_set %>% select(image, max_probs)
    return(candidate_set_table[1:10,])
  })
  
  output$canYouLabel <- renderText({
    req(input$cont)
    return(str_c("Can you label this image?: "))
  })
  
  output$img <- renderUI({
    req(input$cont)
    img_file <- str_c("malaria_images/", candidate_set$image[1])
    tags$img(src = img_file, height=250, width=250)
  })
  
})


# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(png)
library(shinyjs)
source("modeling_functions.R")

df <- NULL
label <- NULL
active_set <- NULL
candidate_set <- NULL
imCol <- NULL
labCol <- NULL
RND <- 0
tab <- tibble(ROUND = NA, ACCURACY = NA)
files <- NULL


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
      files <<- list.files("www/images")
      if (length(files) > 0) {
        selectInput("images", str_c("Is this one of your images: "),
                    choices = c("Yes", "No"))  
      } else {
        return("No Image Files Detected in folder www/images")
      }
    }
  })
  
  output$showImg <- renderUI({
    req(input$init == "No")
    img_file <- str_c("images/", files[[1]])
    tags$img(src = img_file, height=250, width=250)
  })
  
  #####
  ##### SECTION 1: IF (or once) THEY HAVE FEATURIZED DATA 
  #####
  
  # Creates the sidebar table of first five featurized datapoints
  output$table <- renderTable({
    req(input$file1)
    df <<- read.csv(input$file1$datapath)
    if ("label" %in% colnames(df)) {
        df <<- df %>% mutate(label = factor(label))
      }
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
      imCol <<- FALSE
    }
    pt3 <- NULL
    if ("label" %in% colnames(df)) {
      pt3 <- str_c("Your data contains a column of labels called label.")
      labCol <<- TRUE
    }
    else {
      pt3 <- str_c("Your data does not contain a column of labels called label.")
      labCol <<- FALSE
    }
    fin <- str_c(pt1, pt2, pt3, sep = " ")
    return(fin)
  })
  
  output$continue <- renderUI({
    req(input$file1)
    actionButton("continue", "Continue")
  })
  
  observeEvent(input$continue, {
    removeUI(
      selector = "#dataSummary"
    )
  })
  
  observeEvent(input$continue, {
    removeUI(
      selector = "#file"
    )
  })
  
  observeEvent(input$continue, {
    removeUI(
      selector = "#init"
    )
  })
  
  observeEvent(input$continue, {
    removeUI(
      selector = "#table"
    )
  })
  
  output$label <- renderUI({
    req(input$continue)
    RND <<- RND + 1
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
        return(str_c("Use: ", input$label, "  This has ", num, " labels."))
  })
  
  output$imageTextInfo <- renderText({
    req(input$continue)
        sel <- df %>% select(input$image) %>% na.omit()
        num <- nrow(sel)
        return(str_c("Column ", input$image, " will reference your image files.  
                 This has ", num, " images."))
  })
  
  output$genMod <- renderUI({
    req(input$continue)
    label <- str_c(input$label)
    actionButton("mod", "Generate Model")
  })
  
  output$slider <- renderUI({
    req(input$mod)
    sliderInput("lambda",
                "Choose Lambda:",
                min = .01,
                max = .75,
                value = .05)
  })
  
  output$round <- renderPlot({
    req(input$mod)
    colnames(df)[which(colnames(df)==input$label)] <<- "label"
    colnames(df)[which(colnames(df)==input$image)] <<- "image"
    active_set <<- df %>% na.omit()
    candidate_set <<- df %>% filter(is.na(label) == TRUE)
    temp <- createModels(active_set, input$lambda, RND)
    tab <<- tab %>% filter(ROUND < RND)
    tab <<- rbind(tab, temp) %>% na.omit()
    plot <- ggplot(data = tab, aes(x = ROUND, y = ACCURACY)) + geom_line() + geom_point()
    return(plot)
  })
  
  output$afterPlot <- renderUI({
    req(input$mod)
    actionButton("cont", "Get Data To Label")
  })
  
  output$getDatToLab <- renderTable({
    req(input$cont)
    candidate_set <<- findDataToLabel(candidate_set)
    candidate_set_table <- candidate_set %>% select(image, max_probs)
    return(candidate_set_table[1:10,])
  })
  
  output$exportNeedLabs <- renderUI({
    req(input$cont)
    actionButton("exportDNL", "Export")
  })
  
  output$numToExport <- renderUI({
    req(input$cont)
    selectInput("exportNum", "Please select the number of images you'd like to label", 
                choices = c(1:nrow(candidate_set)))
  })
  
  observeEvent(input$exportDNL, {
    write_csv(candidate_set[1:input$exportNum,], "Data_Needs_Labs.csv")
  })
  
  output$canYouLabel <- renderText({
    req(input$cont)
    return(str_c("Can you label this image? "))
  })
  
  output$img <- renderUI({
    req(input$cont)
    img_file <- str_c("images/", candidate_set$image[1])
    tags$img(src = img_file, height=250, width=250)
  })
  
  output$applyLabel <- renderUI({
    req(input$cont)
    selectInput("newLab", "Please Select a label: ", choices = levels(df$label))
  })
  
  output$saveLabel <- renderUI({
    req(input$newLab)
    actionButton("save", "Apply Label")
  })
  
  output$saveSuccessful <- renderText({
    req(input$save)
    img_file <- str_c(candidate_set$image[1])
    df$label[df$image == img_file] <<- input$newLab
    write_csv(df, "input$file1.csv")
    return("Label Saved Successfully")
  })
  
  output$goToNextRound <- renderText({
    req(input$save)
    return("Press Continue (on left side bar) to enter next round, then Generate Model to
           generate a new model with newly labeled data.")
  })
  
  
  ####
  #### SECTION 2: IF THEY DON'T HAVE FEATURIZED DATA
  ####
  
  output$detectImages <- renderText({
    req(input$images)
    if (input$images == "Yes") {
      numImg <- length(files)
      return(str_c("You have ", numImg, " images"))  
    } else {
      return("Please check directory folder for www/images path and add images to
             www/images folder.")
    }
  })
  
  output$featurize <- renderUI({
    req(input$images == "Yes")
    actionButton("featurize", "Featurize")
  })
  
  output$info <- renderUI({
    req(input$images)
    
  })
  
})

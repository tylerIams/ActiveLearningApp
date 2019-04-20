
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(tidyverse)
library(png)
library(DBI)
library(RSQLite)
library(shinyjs)
source("modeling_functions.R")
library(reticulate)
##use_python(" /Users/user/anaconda3/bin")
use_condaenv("tensorflow")
source_python("FeaturizeImages.py")

df <- NULL
df_labeled <- NULL
df_unlabeled <- NULL
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
      hideTab(inputId = "tabactice", target = "Plot")
      hideTab(inputId = "tabactice", target = "Least Confident Data")
      hideTab(inputId = "tabactice", target = "Image") 
      hideTab(inputId = "tabactice", target = "View Featurized dataset") 
      hideTab(inputId = "tabactice", target = "Size of Labeled Data") 
      showTab(inputId = "tabactice", target = "View Dataset") 
      
      removeUI(
        selector = "#featurize"
      )
      removeUI(
        selector = "#detectImages"
      )
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))  
    } else if (input$init == "No") {
      # removeUI(
      #   selector = "#tabactice"
      # )
      hideTab(inputId = "tabactice", target = "Plot")
      hideTab(inputId = "tabactice", target = "Least Confident Data")
      hideTab(inputId = "tabactice", target = "Image")
      hideTab(inputId = "tabactice", target = "View Dataset") 
      hideTab(inputId = "tabactice", target = "Size of Labeled Data")
      showTab(inputId = "tabactice", target = "View Featurized dataset") 
      files <<- list.files("www/UNLABELED")
      if (length(files) > 0) {
        selectInput("images", str_c("Is this one of your images: "),
                    choices = c("Yes", "No"))  
      } else {
        return("No Image Files Detected in folder www/UNLABELED")
      }
    }
  })
  
  output$showImg <- renderUI({
    req(input$init == "No")
    img_file <- str_c("UNLABELED/", files[[1]])
    tags$img(src = img_file, height=250, width=200)
  })
  
  #####
  ##### SECTION 1: IF (or once) THEY HAVE FEATURIZED DATA   ##### 
  #####
  
  # Creates the sidebar table of first five featurized datapoints
  
  output$getView <- renderTable({
    req(input$file1)
    df <<- read.csv(input$file1$datapath)
    df<<- df[-1]
    
    if ("label" %in% colnames(df)) {
      df <<- df %>% mutate(label = factor(label))
      
    }
    df_unlabeled <<- subset(df, is.na(df$label))
    df_labeled <<- subset(df, !(is.na(df$label)))
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
    fin <- str_c(pt1, pt2, pt3, sep = "\n")
    return(fin)
  })
  output$continueTolabel <- renderUI({
    req(input$file1)
    actionButton("continue", "Continue")
  })
  
  
  observeEvent(input$continue, {
    hideTab(inputId = "tabactice", target = "View Dataset") 
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
        num <- ncol(sel)
        return(str_c("Use: ", input$label, "  This has ", num, " labels."))
  })
  
  output$imageTextInfo <- renderText({
    req(input$continue)
        sel <- df %>% select(input$image) %>% na.omit()
        num <- nrow(sel)
        return(str_c("Column ", input$image, " will reference your image files.  
                 This has ", num, " images."))
  })
  
  output$confirmSelection <- renderUI({
    req(input$continue)
    label <- str_c(input$label)
    actionButton("confirm", "Confirm Selection")
  })
  
  observeEvent(input$confirm, {
    showTab(inputId = "tabactice", target = "Size of Labeled Data")
    
    
    removeUI(
        selector = "#labelTextInfo"
      )
    removeUI(
      selector = "#confirmSelection"
    )
    removeUI(
      selector = "#imageTextInfo"
    )
    removeUI(
      selector = "#image"
    )
    removeUI(
      selector = "#label"
    )
  })
  if (TRUE) {
    output$sizeInfo <- renderText({
    req(input$confirm)
    
    return(str_c("You need at least 170 labaled images to begin the Active Selection"))
    })} else{
      output$StartActice <- renderUI({
        actionButton("startLabel", "Start Labeling")
      })
  }
  
  output$StartLabeling <- renderUI({
    req(input$confirm)
    actionButton("startLabel", "Start Labeling")
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
                min = .001,
                max = 1,
                value = .005)
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
  observeEvent(input$startLabel, {
    hideTab(inputId = "tabactice", target = "Size of Labeled Data")
    #showTab(inputId = "tabactice", target = "Image")
    img_file_name <<- str_c("UNLABELED/", str_replace(as.character(df_unlabeled$image[1]),"www/UNLABELED/",""))
    
  })
  
  
  showimages <- function(inState) {
  
    observeEvent(inState, {
        showTab(inputId = "tabactice", target = "Image")
    })
    
    output$canYouLabel <- renderText({
      req(inState)
      return(str_c("Can you label this image? "))
    })
    
    output$img <- renderUI({
      req(inState)
      tags$img(src = img_file_name, height=250, width=250)
    })
    output$cheatLabeling <- renderText({
      req(inState)
      return(img_file_name)
    })
    output$applyLabel <- renderUI({
      req(inState)
      selectInput("newLab", "Please Select a label: ", choices = levels(df$label))
    })
    
    output$saveLabel <- renderUI({
      req(input$newLab)
      actionButton("save", "Apply Label")
    })
    
    output$saveSuccessful <- renderText({
      req(input$save)
      img_file <- str_c("www/", img_file_name)
      df$label[df$image == img_file] <<- input$newLab
      df_unlabeled <<- subset(df, is.na(df$label))
      df_labeled <<- subset(df, !(is.na(df$label)))
      write_csv(df, "final_data_test.csv")
      return("Label Saved Successfully")
    })
    observeEvent(input$save, {
      removeUI(
        selector = "#save"
      )
    })

    output$goToNextRound <- renderUI({
      req(input$save)
      actionButton("nextLabel", "Label Next Image")
    })
  } 
  observeEvent(input$startLabel, {
    showimages(input$startLabel)
    
  })
  
  
    observeEvent(input$nextLabel, {
      # while (TRUE ){
        #   removeUI(
        #     selector = "#img"
        #   )
        # removeUI(
        #   selector = "#applyLabel"
        # )
        # removeUI(
        #   selector = "#goToNextRoun"
        # )
        # removeUI(
        #   selector = "#saveSuccessful"
        # )
        # removeUI(
        #   selector = "#saveLabel"
        # )
        # removeUI(
        #   selector = "#saveLabel"
        # )
        hideTab(inputId = "tabactice", target = "Image")
        img_file_name <<- str_c("UNLABELED/", str_replace(as.character(df_unlabeled$image[1]),"www/UNLABELED/",""))
        showimages(input$nextLabel)
        #}
    })

  
  
  ####
  #### SECTION 2: IF THEY DON'T HAVE FEATURIZED DATA   #### 
  ####

  
  output$detectImages <- renderText({
    req(input$images)
    if (input$images == "Yes") {
      numImg <- length(files)
      return(str_c("You have ", numImg, " unlabeled images"))  
    } else {
      return("Please check directory folder for www/UNLABELED path and add images to
             www/UNLABELED folder.")
    }
  })
  
  output$featurize <- renderUI({
    req(input$images == "Yes")
    actionButton("featurize", "Featurize")
  })
  
  observeEvent(input$featurize, {
    data_featurized <<- feat_data()
  })
  output$getViewfeaturized <- renderTable({
    req(input$featurize)
    return(data_featurized[1:10,(ncol(data_featurized)-2):ncol(data_featurized)])
  })
  
  output$info <- renderUI({
    req(input$images)
    
  })
  
})

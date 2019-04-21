
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
CALL <<- 0
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
      hideTab(inputId = "tabactice", target = "Active Learning Labeling")
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
      hideTab(inputId = "tabactice", target = "Active Learning Labeling")
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
  
  #####                                                       #####
  #####                                                       #####
  #####        SECTION 1: IF THEY HAVE FEATURIZED DATA        ##### 
  #####                                                       #####
  #####                                                       #####
  
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
    #RND <<- RND + 1
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
        return(str_c("Use: ", input$label, "  This has ", num, " labeled images."))
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
  
    output$sizeInfo <- renderText({
    req(input$confirm)
      num <- nrow(df_labeled)
      numLeft <- 170 - num
      if (num < 170) {
        return(str_c("You need at least 170 labaled images to begin the Active Selection. Since you have " 
                    , num, " labaled images, you will need to label ", numLeft, " more images."))
      } else{
        return(str_c("You need at least 170 labaled images to begin the Active Selection. Since you have ",
                     num, " images. You may begin the Actice Selection."))
      }
    })
    
    output$StartLabeling <- renderUI({
      req(input$confirm)
      num <- nrow(df_labeled)
      if (num < 170) {
        actionButton("startLabel", "Start Labeling")
    
    } else{
        actionButton("beginActiveLearning", "Run Active Selection")
      
  }
  
    }) 
    ####                                                          ####
    ####                                                          ####
    ####   LABELING PORTION - USER HAS UNDER 170 LABELED IMAGES   ####
    ####                                                          ####
    ####                                                          ####  
  
    showimages <- function(inState) {
      
      observeEvent(inState, {
        showTab(inputId = "tabactice", target = "Image")
      })
      
      output$canYouLabel <- renderText({
        req(inState)
        return(str_c("Please label this image "))
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
        values <<- values - 1
        print(values)
        write_csv(df, "final_data_test.csv")
        return("Label Saved Successfully")
      })
      
      
      output$goToNextRound <- renderUI({
        req(input$save)
        if (values > 0){
          actionButton("nextLabel", "Label Next Image")
        } else {
          actionButton("beginActiveLearning", "Run Active Selection")
        }
        
      })
    }  
    
    
    observeEvent(input$startLabel, {
      hideTab(inputId = "tabactice", target = "Size of Labeled Data")
      #showTab(inputId = "tabactice", target = "Image")
      values <<- 170 - nrow(df_labeled)
      img_file_name <<- str_c("UNLABELED/", str_replace(as.character(df_unlabeled$image[1]),"www/UNLABELED/",""))
      showimages(input$startLabel)
    })
    
    
    observeEvent(input$nextLabel, {
      hideTab(inputId = "tabactice", target = "Image")
      img_file_name <<- str_c("UNLABELED/", str_replace(as.character(df_unlabeled$image[1]),"www/UNLABELED/",""))
      showimages(input$nextLabel)
      
    })
    
   
    #####                                                           #####
    #####                                                           #####
    ##### MODELING PORTION -- ONCE USER HAS OVER 170 LABELED IMAGES #####
    #####                                                           #####
    #####                                                           #####
    
    showActiveImage <- function(inState) {
      
      output$showImage <- renderUI({
        req(inState)
        print(str_c("Image name: ", img_name))
        img_file <- str_c("UNLABELED/", img_name)
        tags$div(style = 'padding: 50px',
                 tags$img(src = img_file, height=250, width=250)
        )
      })
      
      output$cheatLabeling2 <- renderText({
        req(inState)
        return(img_name)
      })
      
      output$applyAL_Label <- renderUI({
        req(inState)
        selectInput("newLab", "Please Select a label: ", choices = levels(df$label))
      })
      
      output$saveAL_Label <- renderUI({
        req(input$newLab)
        actionButton("saveAL_LABEL", "Apply Label")
      })
      
      output$ALsaveSuccessful <- renderUI({
        req(input$saveAL_LABEL)
        img_fle <- str_c(candidate_set$image[1])
        candidate_set <<- candidate_set[-1,]
        df$label[df$image == img_fle] <<- input$newLab
        write_csv(df, "final_data_test.csv")
        tags$div(
          tags$h3("Label Saved Successfully")
        )
      })
      
      output$goToNextRound2 <- renderUI({
        req(input$saveAL_LABEL)
        CALL <<- CALL + 1
        print(str_c("CALL ", CALL))
        if (CALL < 12 ){
          actionButton("continueLabel", "Label Next Image")
        } else {
          actionButton("continueActiveLearning", "Run Active Selection")
        }
        
      })
    }
    
    
    
    
    Actively_Learn <- function(state) {

      print("IN THE REQUIRED MODULO SHIT")
      
      
      
      output$slider <- renderUI({
        req(state)
        sliderInput("lambda",
                    "Choose Lambda:",
                    min = .001,
                    max = 1,
                    value = .005)
      })
      
      output$round <- renderPlot({
        req(state)
        RND <<- RND + 1
        colnames(df)[which(colnames(df)==input$label)] <<- "label"
        colnames(df)[which(colnames(df)==input$image)] <<- "image"
        active_set <<- df %>% na.omit()
        candidate_set <<- df %>% filter(is.na(label) == TRUE)
        print("Calling create models")
        temp <- createModels(active_set, input$lambda, RND)
        tab <<- tab %>% filter(ROUND < RND)
        tab <<- rbind(tab, temp) %>% na.omit()
        print(tab)
        plot <- ggplot(data = tab, aes(x = ROUND, y = ACCURACY)) + geom_line() + geom_point()
        return(plot)
      })
      
      output$afterPlot <- renderUI({
        req(state)
        actionButton("cont", "Get Data To Label")
      })
      
      observeEvent(input$cont, {
        hideTab(inputId = "tabactice", target = "Plot")
        showTab(inputId = "tabactice", target = "Least Confident Data")
        
        
      })
      
      
      output$getDatToLab <- renderTable({
        req(input$cont)
        candidate_set <<- findDataToLabel(candidate_set)
        candidate_set_table <- candidate_set %>% select(image, max_probs)
        return(candidate_set_table[1:12,])
      })
      
      output$exportNeedLabs <- renderUI({
        req(input$cont)
        actionButton("exportDNL", "Export")
      })
      
      # output$numToExport <- renderUI({
      #   req(input$cont)
      #   selectInput("exportNum", "Please select the number of images you'd like to label", 
      #               choices = c(1:nrow(candidate_set)))
      # })
      
      observeEvent(input$exportDNL, {
        write_csv(candidate_set[1:12,], "Data_Needs_Labs.csv")
        hideTab(inputId = "tabactice", target = "Least Confident Data")
        showTab(inputId = "tabactice", target = "Active Learning Labeling")
        
      })
    }
    
    observeEvent(input$beginActiveLearning, {
      hideTab(inputId = "tabactice", target = "Image")
      hideTab(inputId = "tabactice", target = "Size of Labeled Data")
      showTab(inputId = "tabactice", target = "Plot")
      Actively_Learn(input$beginActiveLearning)
      img_name <<- str_c(str_replace(as.character(candidate_set$image[1]),"www/UNLABELED/",""))
      showActiveImage(input$beginActiveLearning)
    })
    
    observeEvent(input$continueActiveLearning, {
      hideTab(inputId = "tabactice", target = "Image")
      hideTab(inputId = "tabactice", target = "Size of Labeled Data")
      showTab(inputId = "tabactice", target = "Plot")
      hideTab(inputId = "tabactice", target = "Active Learning Labeling")
      Actively_Learn(input$continueActiveLearning)
      CALL <<- 0
      img_name <<- str_c(str_replace(as.character(candidate_set$image[1]),"www/UNLABELED/",""))
      showActiveImage(input$continueActiveLearning)
    })   
    observeEvent(input$continueLabel, {
        hideTab(inputId = "tabactice", target = "Plot")
        hideTab(inputId = "tabactice", target = "Least Confident Data")
        #showTab(inputId = "tabactice", target = "Active Learning Labeling")
        img_name <<- str_c(str_replace(as.character(candidate_set$image[1]),"www/UNLABELED/",""))
        showActiveImage(input$continueLabel)
    })
    
    
    # output$slider <- renderUI({
    #   req(input$beginActiveLearning)
    #   sliderInput("lambda",
    #             "Choose Lambda:",
    #             min = .001,
    #             max = 1,
    #             value = .005)
    # })
    # 
    # output$round <- renderPlot({
    #   req(input$beginActiveLearning)
    #   colnames(df)[which(colnames(df)==input$label)] <<- "label"
    #   colnames(df)[which(colnames(df)==input$image)] <<- "image"
    #   active_set <<- df %>% na.omit()
    #   candidate_set <<- df %>% filter(is.na(label) == TRUE)
    #   temp <- createModels(active_set, input$lambda, RND)
    #   tab <<- tab %>% filter(ROUND < RND)
    #   tab <<- rbind(tab, temp) %>% na.omit()
    #   plot <- ggplot(data = tab, aes(x = ROUND, y = ACCURACY)) + geom_line() + geom_point()
    #   return(plot)
    # })
    # 
    # output$afterPlot <- renderUI({
    #   req(input$beginActiveLearning)
    #   actionButton("cont", "Get Data To Label")
    # })
    # observeEvent(input$cont, {
    #   hideTab(inputId = "tabactice", target = "Plot")
    #   showTab(inputId = "tabactice", target = "Least Confident Data")
    #   
    #   
    # })
    # 
    # 
    # output$getDatToLab <- renderTable({
    #   req(input$cont)
    #   candidate_set <<- findDataToLabel(candidate_set)
    #   candidate_set_table <- candidate_set %>% select(image, max_probs)
    #   return(candidate_set_table[1:10,])
    # })
    # 
    # output$exportNeedLabs <- renderUI({
    #   req(input$cont)
    #   actionButton("exportDNL", "Export")
    # })
    # 
    # output$numToExport <- renderUI({
    #   req(input$cont)
    #   selectInput("exportNum", "Please select the number of images you'd like to label", 
    #               choices = c(1:nrow(candidate_set)))
    # })
    # 
    # observeEvent(input$exportDNL, {
    #   write_csv(candidate_set[1:input$exportNum,], "Data_Needs_Labs.csv")
    # })
    # 
    # 
    
    
    
      
      
    
    
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
      # removeUI(
      #   selector = "#featurize"
      # )
    })
    output$getViewfeaturized <- renderTable({
      req(input$featurize)
      return(data_featurized[1:10,(ncol(data_featurized)-2):ncol(data_featurized)])
    })
    
    output$info <- renderUI({
      req(input$images)
      
    })
    
  })

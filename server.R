
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
#use_python("/Users/TylerIams/anaconda3/bin")
use_condaenv("tensorflow")
#source_python("FeaturizeImages.py")

df <- NULL
df_labeled <- NULL
df_unlabeled <- NULL
label <- NULL
active_set <- NULL
candidate_set <- NULL
imCol <- NULL
labCol <- NULL
RND <- 0
INNERRND <- 0
tab <- tibble(ROUND = NA, ACCURACY = NA)
files <- NULL
CALL <- 11
training_set <- NULL


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
      
      fileInput("file1", "Choose CSV File",
                multiple = FALSE,
                accept = c("text/csv",
                           "text/comma-separated-values,text/plain",
                           ".csv"))
      
    } else if (input$init == "No") {
      
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
    sec <- num - 1
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
      selectInput("image", "Image filenames column: ", 
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
  
  output$sizeInfo <- renderUI({
    num <- nrow(df_labeled)
    if (num < 170) { 
       tags$div(
         tags$p(str_c("You need at least 170 labaled images to begin the Active Selection")), 
         actionButton("startLabel", "Start Labeling")
       )
      }
    else{
      tags$div(
        tags$p(str_c("You have ", num, " labeled images, and are ready to begin Active Learning")),
        actionButton("beginActiveLearning", "Begin Active Learning")
      )
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
      print(img_file_name)
      img_file <- str_c("www/", img_file_name)
      print(img_file)
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
  
    output$numLabelsInfo <- renderText({
      req(input$save)
      num = nrow(df_labeled)
      res = ""
      if (num >= 170) {
        res = "You may begin active learning"
      } else {
        numLeft = 170 - num
        res = str_c("You need ", numLeft, " more labeled images to begin active learning.")
      }
      result = str_c("You now have ", num, " labeled images. ", res) 
    })
    
    output$goToNextRound <- renderUI({
      req(input$save)
      num = nrow(df_labeled)
      if (num >= 170) {
        actionButton("beginActiveLearning", "Begin Active Learning")
      } else {
        actionButton("nextLabel", "Label Next Image")  
      }
    })
  } 
    observeEvent(input$startLabel, {
      hideTab(inputId = "tabactice", target = "Size of Labeled Data")
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
    
    
    
    observeEvent(input$beginActiveLearning, {
      CALL <<- CALL + 1
      RND <<- RND + 1
      showTab(inputId = "tabactice", target = "Plot")
      hideTab(inputId = "tabactice", target = "Image")
      hideTab(inputId = "tabactice", target = "Size of Labeled Data")
      Actively_Learn()
    })
    
    observeEvent(input$continueActiveLearning, {
      CALL <<- CALL + 1
      if (CALL == 12) {
        RND <<- RND + 1
        showTab(inputId = "tabactice", target = "Plot")
        hideTab(inputId = "tabactice", target = "Least Confident Data")
        hideTab(inputId = "tabactice", target = "Active Learning Labeling")
      } else {
        hideTab(inputId = "tabactice", target = "Plot")
        hideTab(inputId = "tabactice", target = "Least Confident Data")
        showTab(inputId = "tabactice", target = "Active Learning Labeling")
      }
      Actively_Learn()
    })
    
    Actively_Learn <- function() {
      
      if (CALL == 12) {
        CALL <<- 0
        INNERRND <<- INNERRND + 1
        output$genMod <- renderUI({
          label <- str_c(input$label)
          tags$div( style = 'padding: 20px',
                    actionButton("mod", str_c("Begin Round ", RND))  
          )
        })
        
        output$slider <- renderUI({
          req(input$mod & INNERRND == RND)
          sliderInput("lambda",
                      "Choose Lambda:",
                      min = .001,
                      max = 1,
                      value = .005)
        })
        
        output$round <- renderPlot({
          if (!is.null(rv$selected)) {
            req(input$mod & INNERRND == RND)
            colnames(df)[which(colnames(df)==input$label)] <<- "label"
            colnames(df)[which(colnames(df)==input$image)] <<- "image"
            active_set <<- df %>% na.omit()
            candidate_set <<- df %>% filter(is.na(label) == TRUE)
            rem_data <- nrow(candidate_set)
            print(str_c("Remaining Unlabeled data: ", rem_data))
            print("Calling create models")
            temp <- createModels(active_set, input$lambda, RND)
            tab <<- tab %>% filter(ROUND < RND)
            tab <<- rbind(tab, temp) %>% na.omit()
            print(tab)
            plot <- ggplot(data = tab, aes(x = ROUND, y = ACCURACY)) + geom_line() + geom_point()
            return(plot)
          }
        })
        
        output$afterPlot <- renderUI({
          req(input$mod & INNERRND == RND)
          rem_data <- df %>% filter(is.na(label) == TRUE) %>% count()
          if (rem_data == 0) {
            tags$div(
              tags$p("You have no more remaining unlabeled data!"),
              actionButton("finish", "Finish Active Learning")
            )
          } else {
            tags$div(
              actionButton("cont", "Get Data To Label")
            )
          }
        })
        
        observeEvent(input$cont, {
          showTab(inputId = "tabactice", target = "Least Confident Data")
          showTab(inputId = "tabactice", target = "Active Learning Labeling")
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
      
        } ## END IF CALL - 1 %% 12 STATEMENT
      
      output$showImage <- renderUI({
        req(input$cont)
        img_name <- candidate_set$image[1]
        if (is.null(training_set)) {
          training_set <<- tibble(Image = img_name)
        } else {
          training_set <<- add_row(training_set, Image = img_name)
        }
        print(str_c("CALL ", CALL))
        print(str_c("Image name: ", img_name))
        img_file <- str_c("UNLABELED/", str_replace(as.character(img_name), "www/UNLABELED/", ""))
        tags$div(style = 'padding: 50px',
                 tags$img(src = img_file, height=350, width=350)
        )
      })
      
      output$applyAL_Label <- renderUI({
        req(input$cont)
        selectInput("newLab", "Please Select a label: ", choices = levels(df$label))
      })
      
      output$saveAL_Label <- renderUI({
        req(input$newLab)
        actionButton("saveAL_LABEL", "Apply Label")
      })
      
      output$ALsaveSuccessful <- renderUI({
        req(input$saveAL_LABEL)
        img_file <- str_c(candidate_set$image[1])
        candidate_set <<- candidate_set[-1,]
        df$label[df$image == img_file] <<- input$newLab
        write_csv(df, "final_data_test.csv")
        tags$div(
          tags$h3("Label Saved Successfully"),
          actionButton("continueActiveLearning", "Continue")
        )
      })
      
    } ### END OF ACTIVELY LEARN FUNCTION
    
    
    #####                                                      #####
    #####  THE REACTIVE FOR ACTIVE LEARNING TO WORK CORRECTLY  #####
    #####                                                      #####
    
    # Define reactiveValue
    rv <- reactiveValues(selected = NULL)
    
    # 1. Pass value of input$mod to Reactive Value
    observe( {
      rv$selected <- input$mod
    })
    
    # 2. clear input$mod after continueActiveLearning is pressed
    observeEvent(input$continueActiveLearning, {
      rv$selected <- NULL
    })
    
    #####                                                      #####
    #####                                                      #####
    

    #####                                                      #####
    #####              EXPORT THE TRAINING SET                 #####
    #####                                                      #####
    
    output$exportTraining <- renderUI({
      req(input$mod)
      actionButton("exportTrainingSet", "Export Training Set")
    })
    
    observeEvent(input$exportTrainingSet, {
          output$exportedTraining <- renderUI({
            req(input$mod)
            req(input$exportTrainingSet)
            if (!is.null(training_set)) {
              write_csv(training_set, "TRAINING_IMAGES.csv")
              tags$div(
                tags$h4("Training Images Exported Successfully")
              )
            }
            else {
              tags$div(
                tags$h4("Your Training set is Empty, try again after a round of active learning")
              )
            }
          })
    })
    
    
    #####                                                      #####
    #####       FINISH THE PROCESS - ALL DATA LABELED          #####
    #####                                                      #####
    
    observeEvent(input$finish, {
      hideTab(inputId = "tabactice", target = "Least Confident Data")
      hideTab(inputId = "tabactice", target = "Active Learning Labeling")
      removeUI(
        selector = "#genMod"
      )
      finalize()
    })
    
    finalize <- function() {
     
       output$finished_now_Export <- renderUI({
        tags$div(style = 'padding : 40px',
                 tags$h3("Would you like to export your final plot, table and data?"),
                 actionButton("exportAll", "Export All")
                 )
      })
      
       output$finalize <- renderUI({
         req(input$exportAll)
         write_csv(df, "FINAL_ACTIVE_LEARNING_DATA.csv")
         write_csv(tab, "FINAL_ACCURACY_TABLE.csv")
         plot <- ggplot(data = tab, aes(x = ROUND, y = ACCURACY)) + geom_line() + geom_point()
         ggsave("FINAL_PLOT.png", plot = plot, width = 7, height = 5, units = "cm")
         tags$div(style = 'padding : 40px',
                  tags$h3("Export Successful"),
                  tags$p("Please check your project directory for 3 files:"),
                  tags$p("1. FINAL_ACTIVE_LEARNING_DATA.csv"),
                  tags$p("2. FINAL_ACCURACY_TABLE.csv"),
                  tags$p("3. FINAL_PLOT.png")
         )
       })
       
       observeEvent(input$exportAll, {
         removeUI(
           selector = "#round"
         )
         removeUI(
           selector = "#acc"
         )
         removeUI(
           selector = "#slider"
         )
         removeUI(
           selector = "#afterPlot"
         )
       })
    }
    

    #####                                                      #####
    #####                                                      #####
    
    
    
  ####                                                 ####
  ####                                                 ####
  #### SECTION 2: IF THEY DON'T HAVE FEATURIZED DATA   #### 
  ####                                                 ####
  ####                                                 ####
  
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

library(shiny)
library(shinyalert)
library(here)
library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(rlang)





Ui_QcViewerMod <- function(id){
  
  
  ns <- NS(id)

  tagList(  
    fluidPage(
      titlePanel(textOutput(outputId = "Ui_Display_SelectedSurvey")),
      h4("Quality Control Tests Viewer"),
      
      HTML("<hr>"),
      
      tabPanel("QC tests",
               
               uiOutput(ns("Ui_DynamicInput_InputDfs")),
    
               actionButton(ns("Ui_Input_RunButton"),
                            "Run / refresh",
                            icon = icon("refresh")),
               
               checkboxInput(ns("Ui_Input_ShowOnlyQcConcerns"),
                             "Show only those QC results where there are concerns:"),
               
               br(),
               br(),
               uiOutput(ns("Ui_OutputDisplay_QcTestResults")),
               shinyjs::useShinyjs()
               )
      )
    )
}   

####Function: send notification of error####
notifyError <- function(...){
  showNotification(ui = paste0(...),
                   type = "error",
                   duration = Inf)
}

####Module server
Server_QcViewerMod <- function(id, appWd){
  
  moduleServer(id, function(input, output, session){
    
    ns <- session$ns

    
    #Extract directories of input datasets:
    #Filter with a regex to get files and not folder names:
    InputXlsx_reactVals <- reactiveValues()
    
    
    #Set up input data folder file path reactive:
    FilePath_InputDataFolder_react <- reactiveVal(NULL)
    
    #Update reactive if app environment file path is specified:
    observe({
      
      FilePath_InputDataFolder_react(
        here(appWd,
             "1. Input"
             )
        )
      

    })
  
    observe({
      
      req(FilePath_InputDataFolder_react())
      
      InputXlsx_reactVals$FileNames <- 
           list.files(FilePath_InputDataFolder_react(),
                      pattern = "^.+\\..+$")
      
      InputXlsx_reactVals$FileNames
      })
    
  
    
    output$Ui_DynamicInput_InputDfs <-
      renderUI({
        selectInput(inputId = ns("Ui_Input_InputDf"),
                    label = "Select country office dataset for QC",
                    choices = InputXlsx_reactVals$FileNames)
        })
    
    
  
    #Set up dataset reactive:
    SelectedInputDf_react <- reactiveVal(NULL)
    
  
    
    #Import dataset with corresponding filepath:
    
    observe({
      
      req(FilePath_InputDataFolder_react())
      
      req(input$Ui_Input_InputDf)
      
      
     tryCatch(
       {SelectedInputDf_react({
         read_excel(path = here(FilePath_InputDataFolder_react(),
                                input$Ui_Input_InputDf),
                    guess_max = Inf
                    )
         })
         
         }, error = function(e){
        
        print("Try again")
        print(e)
         }
      
     )
      
  
    })
    
    
    #Initialise reference object reactive:
    
    QcReferenceDf_react <- reactiveVal(NULL)
    
    
    
    
    #Run tests and update tests reference object:
    observeEvent(input$Ui_Input_RunButton, {
      
      req(SelectedInputDf_react())
      
      QcReferenceDf_react <- NULL
      
      shinyalert(
                title = "Loading data and analysis...",
                text = "This should just take a few seconds.",
                type = "",
                closeOnEsc = TRUE,
                closeOnClickOutside = FALSE,
                html = FALSE,
                showCancelButton = FALSE,
                showConfirmButton = FALSE,
                inputType = "text",
                inputValue = "",
                inputPlaceholder = "",
                confirmButtonText = "",
                confirmButtonCol = "#AEDEF4",
                cancelButtonText = "Cancel",
                timer = 0,
                animation = TRUE,
                imageUrl = NULL,
                imageWidth = 100,
                imageHeight = 100,
                className = "",
                callbackR = NULL,
                callbackJS = NULL,
                inputId = "QcLoadingData",
                size = "s",
                immediate = FALSE,
                session = session
              )
                 
      
      updateCheckboxInput(session, inputId = "Ui_Input_ShowOnlyQcConcerns", value = FALSE)
        
    
      #Import the runQcTests function, which returns a reference object:
        source(here(appWd,
                    "deps",
                    "FUNCTION - Run QC tests.R")
               )
      
      
      QcReferenceDf_react({
        
        runQcTests(inputDf = SelectedInputDf_react() #Add varying  parameters, such as dates, as arguments.
                   )
        
        })
      
      
      print(QcReferenceDf_react(), n = Inf)
      
      })
    
    
      
    ####Render datatables and text according to reference reactive object:####
    output$Ui_OutputDisplay_QcTestResults <- renderUI({
      
      req(QcReferenceDf_react())
      
        ui_list <- lapply(1:nrow(QcReferenceDf_react()), function(i){
            tagList(
                textOutput(outputId = ns(QcReferenceDf_react()$QcTest_DescUiId[i])),
                
                tags$br(id = ns(QcReferenceDf_react()$QcTest_HtmlBr1Id[i])),
                
                
                uiOutput(outputId = ns(QcReferenceDf_react()$QcConcernsFound_StatusUiId[i])),
                
                DT::DTOutput(outputId = ns(QcReferenceDf_react()$QcTest_DtUiId[i])),
                
                actionButton(inputId = ns(QcReferenceDf_react()$ExportDfQcTest_UiId[i]),
                             label = "Export as Excel workbook",
                             icon = icon("table")),
                            
                tags$br(id = ns(QcReferenceDf_react()$QcTest_HtmlBr2Id[i])),
                
                tags$hr(id = ns(QcReferenceDf_react()$QcTest_HtmlHrId[i]))
                

                         
            )
          
        })
        
    
        do.call(tagList, ui_list)
        
        
        
        
    })
    
    
    observe({
      
      req(QcReferenceDf_react())
    
        # Lapply over the rows of QcReferenceDf_react
        lapply(1:nrow(QcReferenceDf_react()), function(i){
            
            # Render text outputs
            output[[QcReferenceDf_react()$QcTest_DescUiId[i]]] <- renderText({
              
                return(QcReferenceDf_react()$Description[i])
            })
            
            
            #Status message
            output[[QcReferenceDf_react()$QcConcernsFound_StatusUiId[i]]] <- renderUI({
                
    
                if(QcReferenceDf_react()$QcConcernsFound[i]) {
                   tags$span("QC issues found", style = "color: red; font-weight: bold;")
    
                }else{
                   tags$span("No issues found", style = "color: green; font-weight: bold;")
                }
            })
            
            # Render DT tables
            output[[QcReferenceDf_react()$QcTest_DtUiId[i]]] <- DT::renderDT({
                return(QcReferenceDf_react()$QcTestDf[[i]])
            })
            
            
          
                       
      closeAlert()


    #For each button, create an observeEvent
    observeEvent(input[[QcReferenceDf_react()$ExportDfQcTest_UiId[i]]], {
        
      wb <- createWorkbook()
      
      FocusSheetName <- paste("QC test focus", i)
      
      
      addWorksheet(wb, FocusSheetName)

      
      writeData(wb,
                FocusSheetName,
                QcReferenceDf_react()$Description[i],
                startCol = 1,
                startRow = 1)

      writeDataTable(wb,
                     FocusSheetName,
                     QcReferenceDf_react()$QcTestDf[[i]], 
                     startCol = 1,
                     startRow = 3)
      
      
      addWorksheet(wb, "All data")
      
            
      writeDataTable(wb,
                     "All data",
                     SelectedInputDf_react())
      
      
      CoFileNameWoExtension <- 
        str_replace(input$Ui_Input_InputDf, ".xlsx", "")
      
      SysTimeForFileName <-
        format(Sys.time(), "%Y-%m-%d_%H%M%S")
      
      saveWorkbook(wb,
                   file = here(appWd,
                               "2. Output",
                               paste0(CoFileNameWoExtension,
                                      "_", 
                                      SysTimeForFileName, 
                                      ".xlsx")
                               ), 
                   overwrite = FALSE)

      

        
    })
    
  }
  )
    

})

    
    
      observe({
        req(QcReferenceDf_react())
        
        QcReferenceDf_NoQcConcerns <- 
          QcReferenceDf_react() |> 
          filter(QcConcernsFound == FALSE)
          
        
        if(input$Ui_Input_ShowOnlyQcConcerns == TRUE) {
       
          lapply(1:nrow(QcReferenceDf_NoQcConcerns), function(i){
            shinyjs::hide(QcReferenceDf_NoQcConcerns$QcTest_DescUiId[i])
            shinyjs::hide(QcReferenceDf_NoQcConcerns$QcTest_DtUiId[i])
            shinyjs::hide(QcReferenceDf_NoQcConcerns$QcConcernsFound_StatusUiId[i])
            shinyjs::hide(QcReferenceDf_NoQcConcerns$QcTest_HtmlBr1Id[i])
            shinyjs::hide(QcReferenceDf_NoQcConcerns$QcTest_HtmlBr2Id[i])
            shinyjs::hide(QcReferenceDf_NoQcConcerns$QcTest_HtmlHrId[i])
            shinyjs::hide(QcReferenceDf_NoQcConcerns$ExportDfQcTest_UiId[i])
            
          })
          
        }else{
          lapply(1:nrow(QcReferenceDf_NoQcConcerns), function(i){
            shinyjs::show(QcReferenceDf_NoQcConcerns$QcTest_DescUiId[i])
            shinyjs::show(QcReferenceDf_NoQcConcerns$QcTest_DtUiId[i])
            shinyjs::show(QcReferenceDf_NoQcConcerns$QcConcernsFound_StatusUiId[i])
            shinyjs::show(QcReferenceDf_NoQcConcerns$QcTest_HtmlBr1Id[i])
            shinyjs::show(QcReferenceDf_NoQcConcerns$QcTest_HtmlBr2Id[i])
            shinyjs::show(QcReferenceDf_NoQcConcerns$QcTest_HtmlHrId[i])
            shinyjs::show(QcReferenceDf_NoQcConcerns$ExportDfQcTest_UiId[i])

          })
        }
      })
      
    
    })}
  


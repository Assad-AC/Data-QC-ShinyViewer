library(shiny)
library(here)
library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(rlang)
library(openxlsx)
library(shiny)
library(shinyalert)
library(here)
library(readxl)
library(stringr)
library(dplyr)
library(purrr)
library(rlang)
library(rjson)


             
####UI####
ui <- 
  navbarPage(title = "QC Tool",
             
             tabPanel("Stage 0. Insert QC tool folder path",
                      h3("Stage 0. Insert the path to the \"QC tool\" folder, which is the working directory of this application."),
                      h4("Stage 0 needs to be filled in for any and all stages of this application."),
                      textInput("Ui_AppWd",
                                label = "Copy-paste the path to the \"QC tool\" folder",
                                placeholder = "C:\\Users\\username\\Documents\\QC tool")
                      ),
             
             
             tabPanel(title = "Quality Control Tests Viewer",
                      
                      uiOutput("Ui_Output_DisplayModule")
                      )
             )
                    

####Function: send notification of error####
notifyError <- function(...){
  showNotification(ui = paste0(...),
                   type = "error",
                   duration = Inf)
}


####Server####
server <- function(input, output, session){
  
  #Import memory JSON file if it exists
  WdOnLaunch <- getwd()
  
  MemoryJsonFilePath <- here(WdOnLaunch, "Memory_QcApp.json")
  
  if(file.exists(MemoryJsonFilePath)){
    
     MemoryListForJson <- fromJSON(file = MemoryJsonFilePath)
    
     
  }else{
    
    #Create and export memory JSON file it it does not exist
    MemoryListForJson <- list()
    
  }


  observe({
    
    print(MemoryListForJson[["AppWd_FilePath"]])
    
    if(!is_empty(MemoryListForJson)){
      
      updateTextInput(session,
                      "Ui_AppWd",
                      value = MemoryListForJson[["AppWd_FilePath"]])
    }
    
  })

  observe({
    
    req(input$Ui_AppWd)
    
    if(file.exists(input$Ui_AppWd)){
      
      showNotification(ui = "The file path is valid.",
                       type = "message",
                       duration = 6)
      
      write(toJSON(list(AppWd_FilePath = c(input$Ui_AppWd))),
            MemoryJsonFilePath)
            
      
      
      }else{       
        
        notifyError("The file path in Stage 0 does not exist.")
        
        }
      
    
    tryCatch({
    
      source(here(input$Ui_AppWd,
                  "deps",
                  "MODULE - QC.R")
             )
      
    }, error = function(e){
      
      notifyError("Error when importing module: ", e)
      
    })
    
    
    
    
    tryCatch({
      
      output$Ui_Output_DisplayModule <-
        renderUI(Ui_QcViewerMod(id = "QcViewer"))
      

    }, error = function(e){
      
      notifyError("Error when rendering module UI: ", e)
      

    })
    
    
  
    tryCatch({
      
      Server_QcViewerMod(id = "QcViewer",
                         appWd = input$Ui_AppWd)
    
    }, error = function(e){
      
      notifyError("Error when calling module server: ", e)
      
    })


    
  })
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)

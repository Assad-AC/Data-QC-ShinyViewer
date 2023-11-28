runQcTests <- function(inputDf){
  
  library(lubridate)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(rlang)

    
    QcTestRef_ColNames <-
      c("QcTestName",
        "Description",
        "QcTestDf",
        "QcConcernsFound",
        "QcConcernsFound_StatusUiId",
        "QcTest_DescUiId",
        "QcTest_DtUiId",
        "QcTest_HtmlHrId",
        "ExportDfQcTest_UiId")
    
    QcReferenceDf <-
      # Initialize an empty tibble with the correct structure but no rows
        tibble(
          QcTestName = character(0),
          Description = character(0),
          QcTestDf = list(),
          QcConcernsFound = logical(0),
          QcConcernsFound_StatusUiId = character(0),
          QcTest_DescUiId = character(0),
          QcTest_DtUiId = character(0),
          QcTest_HtmlHrId = character(0),
          ExportDfQcTest_UiId = character(0)
      
    )
    
  

  

  addQcTestToRefDf <- function(qcTestDf, qcTestDesc, currentRefObj = QcReferenceDf){
    
    #Names of dataframes must end with _Qc
    QcTestName <- rlang::as_string(substitute(qcTestDf)) |> 
      str_extract("^.+(?=_Qc$)")
    
    if(nrow(qcTestDf) == 0){
      AnythingToReport <- FALSE
    } else {
      AnythingToReport <- TRUE
    }
    
    QcConcernsFound_StatusUiId <- paste0("Ui_OutputText_", QcTestName, "status")
    QcTest_DescUiId <- paste0("Ui_OutputText_", QcTestName)
    QcTest_DtUiId <- paste0("Ui_OutputDt_", QcTestName)
    QcTest_HtmlHrId <- paste0(QcTestName, "HtmlHrId")
    QcTest_HtmlBr1Id <- paste0(QcTestName, "HtmlHBr1Id")
    QcTest_HtmlBr2Id <- paste0(QcTestName, "HtmlHBr2Id")
    ExportDfQcTest_UiId <- paste0(QcTestName, "_Export")
    
    new_entry <- tibble(
      QcTestName = QcTestName,
      Description = qcTestDesc,
      QcTestDf = list(qcTestDf),
      QcConcernsFound = AnythingToReport,
      QcConcernsFound_StatusUiId = QcConcernsFound_StatusUiId,
      QcTest_DescUiId = QcTest_DescUiId,
      QcTest_DtUiId = QcTest_DtUiId,
      QcTest_HtmlHrId = QcTest_HtmlHrId,
      QcTest_HtmlBr1Id = QcTest_HtmlBr1Id,
      QcTest_HtmlBr2Id = QcTest_HtmlBr2Id,
      ExportDfQcTest_UiId = ExportDfQcTest_UiId
    )
    
    updatedRefObj <- bind_rows(currentRefObj, new_entry)
    return(updatedRefObj)
  }

  
  
  
  
####Code to create the QC tests below####
  
  
#####1. Check whether assessment and survey times align in terms of date (rather than date-time)#####
  
  StartingDateAlignment_Qc <-
    inputDf |>
    filter(mdy(assessmentDate) != as_date(mdy_hms(surveyTime))) |>
    select(Response_ID, assessmentDate, surveyTime)
    
 
  QcReferenceDf <- 
    addQcTestToRefDf(qcTestDf = StartingDateAlignment_Qc,
                     qcTestDesc = "Are assessment dates and survey times misaligned date-wise?")
    
  
  

####End of QC tests code####
  return(QcReferenceDf)
  
}

 


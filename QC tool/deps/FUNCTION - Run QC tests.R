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
        "ExportDfQcTest_UiId",
        "SelectDfForExport_UiId")
    
    QcReferenceDf <-
      # Initialize an empty tibble with the correct structure but no rows
        tibble(
          QcTestName = character(0),
          QcTestCode = character(0), #Brief name pf max 31 characters
          Description = character(0),
          QcTestDf = list(),
          QcConcernsFound = logical(0),
          QcConcernsFound_StatusUiId = character(0),
          QcTest_DescUiId = character(0),
          QcTest_DtUiId = character(0),
          QcTest_HtmlHrId = character(0),
          ExportDfQcTest_UiId = character(0),
          SelectDfForExport_UiId = character(0)
      
    )
    
  

  

  addQcTestToRefDf <- function(qcTestDf, qcTestDesc, qcTestCode, currentRefObj = QcReferenceDf){
    
    #Names of dataframes must end with _Qc
    QcTestName <- rlang::as_string(substitute(qcTestDf)) |> 
      str_extract("^.+(?=_Qc$)")
    
    
    if(nchar(qcTestCode) > 31){stop("User-assigned QC test code is too long.")}
    
    
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
    SelectDfForExport_UiId <- paste0(QcTestName, "_SelectForExport")
    
    newEntry <- tibble(
      QcTestName = QcTestName,
      QcTestCode = qcTestCode,
      Description = qcTestDesc,
      QcTestDf = list(qcTestDf),
      QcConcernsFound = AnythingToReport,
      QcConcernsFound_StatusUiId = QcConcernsFound_StatusUiId,
      QcTest_DescUiId = QcTest_DescUiId,
      QcTest_DtUiId = QcTest_DtUiId,
      QcTest_HtmlHrId = QcTest_HtmlHrId,
      QcTest_HtmlBr1Id = QcTest_HtmlBr1Id,
      QcTest_HtmlBr2Id = QcTest_HtmlBr2Id,
      ExportDfQcTest_UiId = ExportDfQcTest_UiId,
      SelectDfForExport_UiId = SelectDfForExport_UiId
    )
    
    updatedRefObj <- bind_rows(currentRefObj, newEntry)
    
    return(updatedRefObj)
  }


                            
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


 


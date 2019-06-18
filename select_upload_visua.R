## Purpose of script: server.R code - select and upload data intro text and functionalities
##
## Author: Anne Lyngholm Sørensen
##
## Date Created: 2019-06-13
##
## Copyright (c) Anne Lyngholm Soerensen, 2019
## Email: lynganne@gmail.com


### Intro to the app
output$steps <- renderText({
  "To perform scale validation using this webpage, you need to do the following steps: 
    <br><ol><li>Upload your data in the box below <b>OR</b> choose one of the existing datasets</li>
    <li>Ensure that the structure of the data is correct using the \"Visualization of a subset of data\"-box</li>
    <li>Investigate questions of interests using the tabs in the bottom of the page</li>
    <li>If you want to understand the theory, look under the \"Theory\" tab to the left</li></ol>"
})

# data upload help text
output$dataLoad <- renderText({
  "Please insert data in either .txt or .csv file format.<br>Notice that the upload may give you an error, 
    if the the separator is incorrect.
    Try either looking at your data in e.g. Notepad or an equivalent software before uploading the data
    and choose the correct separator for your data."
})

# data select help text
output$dataChooseIntro <- renderText({
  "Please select data fitting to your problem. <br>There are three different datasets to select from."
})

# data select visualization help text
output$dataChoose <- renderText({
  "Use the visulization of your data to ensure that you are using the correct data."
})

# data upload visualization help text
output$dataLoadText <- renderText({
  "Use the visulization of your data to ensure that your data is read correctly. <br>
    The data should be divided into columns. If not try to select another separator in the 'Choose separator' dropdown."
})

# R code examples: data import
output$RcodeImport <- renderText({
  "# import .txt file:\n data <- read.table(\"path\", sep = \" \", header = TRUE)\n# import .csv file:\n data <- read.csv(\"path\", sep = \",\", header= TRUE)"})



# hide and show the data upload/data select boxes based on action buttons
observe({
  shinyjs::hide(id = "uploadData")
  shinyjs::hide(id = "showUploadData")
  shinyjs::hide(id = "selectData")
  shinyjs::hide(id = "showSelectData")
})

observeEvent(input$uploadButton, {
  if(input$uploadButton %% 2 != 1){
    shinyjs::hide(id = "uploadData")
    shinyjs::hide(id = "showUploadData")
  } else {
    shinyjs::show(id = "uploadData")
    shinyjs::show(id = "showUploadData")
    shinyjs::hide(id = "selectData")
    shinyjs::hide(id = "showSelectData")
    if(exists("irt_data")){
      rm(irt_data, pos=".GlobalEnv")
    }}
    dataChoice <<- "upload"
  })

observeEvent(input$selectButton, {
  if(input$selectButton %% 2 != 1){
    shinyjs::hide(id = "selectData")
    shinyjs::hide(id = "showSelectData")
  } else {
    shinyjs::show(id = "selectData")
    shinyjs::show(id = "showSelectData")
    shinyjs::hide(id = "uploadData")
    shinyjs::hide(id = "showUploadData")
  }
  dataChoice <<- "select"
  })

# select data code
datasetInput <- reactive({
  switch(input$dataset,
         "HADS" = HADS,
         "scqol" = scqol,
         "cortisol" = scqol)
  
})


# visualize selected data code
output$contentsData <- renderTable({
  head(datasetInput())
})

# upload data and visualize uploaded data code
output$contents <- renderTable({
  
  # input$file1 will be NULL initially. After the user selects
  # and uploads a file, head of that data file by default,
  # or all rows if selected, will be shown.
  
  #if(exists("irt_data")){ # removed alle
  #  rm(irt_data, pos=".GlobalEnv")
  #}
  
  req(input$file1)
  
  # when reading semicolon separated files,
  # having a comma separator causes `read.csv` to error
  tryCatch(
    {
      irt_data <<- read.csv(input$file1$datapath,
                            header = input$header,
                            sep = input$sep,
                            na.strings = input$na)
    },
    error = function(e) {
      # return a safeError if a parsing error occurs
      stop(safeError(e))
    }
  )

  return(head(irt_data))
  
})

## Purpose of script: (server.R) Shiny app for Item Response Theory (IRT)
##
## Author: Anne Lyngholm Sørensen
##
## Date Created: 2019-05-28
##
## Copyright (c) Anne Lyngholm Soerensen, 2019
## Email: lynganne@gmail.com

dt_output = function(title, id) {
  fluidRow(column(
    12, h1(paste0('Table ', sub('.*?([0-9]+)$', '\\1', id), ': ', title)),
    hr(), DTOutput(id)
  ))
}

render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}

server <- function(input, output, session){
  
  HADS <<- read.table("data/HADS.txt", header = T)
  scqol <<- read.table("data/scqol.txt", header = T, na.strings = ".")
  
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
      dataChoice <<- "upload"
      if(exists("ttest_data")){
        rm(ttest_data, pos=".GlobalEnv")
      }
    }})
  
  observeEvent(input$selectButton, {
    if(input$selectButton %% 2 != 1){
      shinyjs::hide(id = "selectData")
      shinyjs::hide(id = "showSelectData")
    } else {
      shinyjs::show(id = "selectData")
      shinyjs::show(id = "showSelectData")
      shinyjs::hide(id = "uploadData")
      shinyjs::hide(id = "showUploadData")
      dataChoice <<- "select"
    }})
  
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
    
    #if(exists("ttest_data")){ # removed alle
    #  rm(ttest_data, pos=".GlobalEnv")
    #}
    
    req(input$file1)
    
    # when reading semicolon separated files,
    # having a comma separator causes `read.csv` to error
    tryCatch(
      {
        ttest_data <<- read.csv(input$file1$datapath,
                                header = input$header,
                                sep = input$sep,
                                na.strings = input$na)
      },
      error = function(e) {
        # return a safeError if a parsing error occurs
        stop(safeError(e))
      }
    )
    
    return(head(ttest_data))
    
  })
  
  # updating the data set:
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set with the appropriate name
    dat <- get(input$dataset)
    colnames <- names(dat)
    
    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose items:", 
                       choices  = colnames,
                       selected = colnames,
                       inline = TRUE)
  })
  
  output$data_table <- renderTable({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()
    
    # Get the data set
    dat <- get(input$dataset)
    
    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    if (is.null(input$columns) || !(input$columns %in% names(dat)))
      return()
    
    # Keep the selected columns
    dat <- dat[, input$columns, drop = FALSE]
    
    # Return first 20 rows
    head(dat, 2)
  })
  
  dimVals <- eventReactive(input$clusterSet, {
    as.integer(unlist(strsplit(input$cluster,",")))
  })
  
  output$itemcluster <- renderTable({
    # If missing input, return to avoid error later in function
    if(is.null(input$cluster))
      return()
    
    # Get the data set
    # dat <- get(input$dataset)
    
    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    # if (is.null(input$columns) || !(input$columns %in% names(dat)))
    #  return()
    
    # Get the data set
    # dat <<- dat[, input$columns, drop = FALSE]
    
    # Keep the selected columns
    clusteritem <- data.table(dim = dimVals())
    
    tclusteritem <- transpose(clusteritem)
    colnames(tclusteritem) <- colnames(dat)
    
    return(tclusteritem)
  })
  
  
  ### T-test
  v <- reactiveValues(doTest = FALSE)
  ttestMethod <<- "oneSample"
  dataChoice <<- "select"
  
  # item selection introduction text
  output$itemSelector <- renderUI({
    HTML("Choose the items you wish to use for the detect function. All columns in the data set is selected by default. Remove columns by checking off the specific boxes. <br>The first two rows of the items selected is visualized below. Ensure that you have choosen the items of interest.")
  })
  
  # dim selection introduction text
  output$dimSelector <- renderUI({
    HTML("Enter the dimentionality of interest of the items. The number of dimensions must be equal or less to the number of  items.<br> If there is only one dimension, enter the same number (e.g. 1) as many times as there is items and comma delimited.")
  })
  
  # score selection introduction text
  output$scoreSelector <- renderUI({
    HTML("Choose the score of interest. The score is an ability estimate e.g. sum score or mean score. <br>The sum score is
         choosen by default.")
  })
  
  # The assumptions per test
  observeEvent(input$oneSample, 
               {output$assumptions <- renderText({paste("<h4>You have choosen to compute the DETECT index for the 
                                                        following:</h4><br>
                                                        &emsp;&emsp;1. The following items:",
                                                        paste(colnames(dat), collapse =", "), "<br>
                                                        &emsp;&emsp;2. With the following belonging dimensions: ",
                                                        paste(dimVals(), collapse =", "), "<br>
                                                        &emsp;&emsp;2. And lastly to use the sum score as ability estimate<br><br>
                                                        To calculate the DETECT index press the following button:")})
               ttestMethod <<- "oneSample"})
  
  observeEvent(input$twoSample, 
               {output$assumptions <- renderText({"<h4>You have choosen to compute the DETECT index for the following:</h4><br>
                 &emsp;&emsp;1. The two samples are mutually independent<br>
                 &emsp;&emsp;2. Observations from each sample are independent<br>
                 &emsp;&emsp;3. To use the mean score as ability estimate<br><br>
                 To calculate the DETECT index press the following button:"})
               ttestMethod <<- "twoSample"})
  
  # the understand and assumptions fulfilled button
  observe({
    shinyjs::hide("understand")
    
    if(input$oneSample | input$twoSample){
      shinyjs::show("understand")
    }
  })
  
  if(ttestMethod == "twoSample"){
    observeEvent(input$oneSample, {
      v$doTest <- FALSE
    })}
  
  if(ttestMethod == "oneSample"){
    observeEvent(input$twoSample, {
      v$doTest <- FALSE
    })}
  
  
  observeEvent(input$understand,
               {if(dataChoice == "upload" & exists("ttest_data")){
                 output$class <- renderText({if(v$doTest == FALSE) return()
                   isolate({"Please supply the needed information below:"})})
                 if(ttestMethod=="oneSample"){
                   output$headArg <- renderText({if(v$doTest == FALSE) return()
                     isolate({"<h4>Arguments in the <font color='#0080ff'>one sample</font> t-test:</h4>"})})
                   output$arguments1 <- renderUI({if(v$doTest == FALSE) return()
                     isolate({selectInput("varInterest1", "Sample of interest:", colnames(ttest_data),
                                          selected = NULL, multiple = FALSE,
                                          selectize = TRUE, width = 200, size = NULL)})})
                   output$arguments2 <- renderUI({if(v$doTest == FALSE) return()
                     isolate({numericInput("varInterest2", "Test value:", 0, width=200)})
                   })
                   output$result <- renderPrint({if(v$doTest == FALSE) return()})
                   session$sendCustomMessage(type = "scrollCallback", 1)
                 } else if(ttestMethod=="twoSample"){
                   output$headArg <- renderText({if(v$doTest == FALSE) return()
                     isolate({"<h4>Arguments in the <font color='#0080ff'>two sample</font> t-test:</h4>"})})
                   output$arguments1 <- renderUI({if(v$doTest == FALSE) return()
                     isolate({selectInput("varInterest1", "Sample 1 of interest:", colnames(ttest_data),
                                          selected = NULL, multiple = FALSE,
                                          selectize = TRUE, width = 200, size = NULL)})})
                   output$arguments2 <- renderUI({if(v$doTest == FALSE) return()
                     isolate({selectInput("varInterest2", "Sample 2 of interest:", colnames(ttest_data),
                                          selected = NULL, multiple = FALSE,
                                          selectize = TRUE, width = 200, size = NULL)})})
                   output$result <- renderPrint({if(v$doTest == FALSE) return()})
                   session$sendCustomMessage(type = "scrollCallback", 1)
                 } 
               } else if(dataChoice == "select"){
                 output$class <- renderText({if(v$doTest == FALSE) return()
                   isolate({"Please supply the needed information below:"})})
                 if(ttestMethod=="oneSample"){
                   output$headArg <- renderText({if(v$doTest == FALSE) return()
                     isolate({"<h4>Arguments in the <font color='#0080ff'>one sample</font> t-test:</h4>"})})
                   output$arguments1 <- renderUI({if(v$doTest == FALSE) return()
                     isolate({selectInput("varInterest1", "Sample of interest:", colnames(datasetInput()),
                                          selected = NULL, multiple = FALSE,
                                          selectize = TRUE, width = 200, size = NULL)})})
                   output$arguments2 <- renderUI({if(v$doTest == FALSE) return()
                     isolate({numericInput("varInterest2", "Test value:", 0, width=200)})
                   })
                   output$result <- renderPrint({if(v$doTest == FALSE) return()})
                   session$sendCustomMessage(type = "scrollCallback", 1)
                 } else if(ttestMethod=="twoSample"){
                   output$headArg <- renderText({if(v$doTest == FALSE) return()
                     isolate({"<h4>Arguments in the <font color='#0080ff'>two sample</font> t-test:</h4>"})})
                   output$arguments1 <- renderUI({if(v$doTest == FALSE) return()
                     isolate({selectInput("varInterest1", "Sample 1 of interest:", colnames(datasetInput()),
                                          selected = NULL, multiple = FALSE,
                                          selectize = TRUE, width = 200, size = NULL)})})
                   output$arguments2 <- renderUI({if(v$doTest == FALSE) return()
                     isolate({selectInput("varInterest2", "Sample 2 of interest:", colnames(datasetInput()),
                                          selected = NULL, multiple = FALSE,
                                          selectize = TRUE, width = 200, size = NULL)})})
                   output$result <- renderPrint({if(v$doTest == FALSE) return()})
                   session$sendCustomMessage(type = "scrollCallback", 1)
                 } 
               }
                 else {
                   output$class <- renderText({if(v$doTest == FALSE) return()
                     isolate({"Data is nessecary to compute a t-test. Load the data in the 'Upload data' box"})})
                   output$result <- renderText({if(v$doTest == FALSE) return()})
                   session$sendCustomMessage(type = "scrollCallback", 1)
                 }}
  )
  
  observeEvent(input$understand, {
    v$doTest <- input$understand})
  
  observe({
    if(v$doTest == FALSE){
      shinyjs::hide("startTTest")}
    
    if(input$understand & v$doTest){
      isolate({shinyjs::show("startTTest")})
    }
  })
  
  
  
  testReact1 <- reactive({
    input$varInterest1
  })
  
  testReact2 <- reactive({
    input$varInterest2
  })
  
  
  outputText <- "P-value of: "
  observeEvent(input$startTTest,
               {
                 if(ttestMethod == "oneSample"){
                   myval1 <- testReact1()
                   myval2 <- testReact2()
                   if(dataChoice == "upload"){
                     vec <- eval(parse(text=paste("ttest_data$",myval1,sep="")))
                   } else {
                     vec <- eval(parse(text=paste("datasetInput()$",myval1,sep="")))
                   }
                   pval <- t.test(x=vec,mu=myval2)$p.value
                   if(pval < 0.0001) {pval <- "< 0.0001"
                   } else {
                     pval <- round(pval,4)
                   }
                   intConf <- t.test(x=vec,mu=myval2)$conf.int
                   output$result <- renderText({if(v$doTest == FALSE) return()
                     isolate({paste("<br><br><h4>Results</h4><hr>",
                                    outputText, pval, ". And a 95% confidence interval of ", round(intConf[1],4), "; ", 
                                    round(intConf[2],4), ".", sep="")})})
                   session$sendCustomMessage(type = "scrollCallback", 1)
                 } else if(ttestMethod=="twoSample"){
                   myval1 <- testReact1()
                   myval2 <- testReact2()
                   if(dataChoice == "upload"){
                     vec1 <- eval(parse(text=paste("ttest_data$",myval1,sep="")))
                     vec2 <- eval(parse(text=paste("ttest_data$",myval2,sep="")))
                   } else {
                     vec1 <- eval(parse(text=paste("datasetInput()$",myval1,sep="")))
                     vec2 <- eval(parse(text=paste("datasetInput()$",myval2,sep="")))
                   }
                   pval <- t.test(x=vec1,y=vec2)$p.value
                   if(pval < 0.0001) {pval <- "< 0.0001"
                   } else {
                     pval <- round(pval,4)
                   }
                   intConf <- t.test(x=vec1,y=vec2)$conf.int
                   output$result <- renderText({if(v$doTest == FALSE) return()
                     isolate({paste("<br><br><h4>Results</h4><hr>",outputText, round(pval,4), ". And a 95% confidence interval of ",
                                    round(intConf[1],4), 
                                    "; ", round(intConf[2],4), ".", sep="")})})
                   session$sendCustomMessage(type = "scrollCallback", 1)
                 } 
               })
  
  
  ### Wilcoxon-Mann-Whitney test
  
  #intro text to Wilcoxon-Mann-Whitney
  output$introWilcox <- renderText({
    "This is an introduction to Wilcox - and it is not very good."
  })
  
  ### Examples
  output$testExamples <- renderText({
    "We will present examples of the types of tests possible to compute using this app"
  })
  
  
               }

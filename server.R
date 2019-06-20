## Purpose of script: (server.R) Shiny app for Item Response Theory (IRT)
##
## Author: Anne Lyngholm Soerensen
##
## Date Created: 2019-05-28
##
## Copyright (c) Anne Lyngholm Soerensen, 2019
## Email: lynganne@gmail.com

server <- function(input, output, session){
  
  # source select, upload and visualize data code
  source("module.R", local = TRUE)
  
  # source select, upload and visualize data code
  source("select_upload_visua.R", local = TRUE)
  
  #updating the data set:
  output$choose_columns <- renderUI({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()

    # Get the data set with the appropriate name
    colnames <- names(datasetInput())

    # Create the checkboxes and select them all by default
    checkboxGroupInput("columns", "Choose items:",
                       choices  = colnames,
                       selected = colnames,
                       inline = TRUE)
  })

  
  
  dat <- eventReactive(input$columns,
                       {tmp <- datasetInput()
                       tmp[,names(tmp) %in% input$columns]})
  
  
  output$data_table <- renderTable({
    # If missing input, return to avoid error later in function
    if(is.null(input$dataset))
      return()

    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    if (is.null(input$columns) || !(input$columns %in% names(dat())))
      return()

    # Return first 20 rows
    head(dat(), 5)
  })

  dimVals <- eventReactive(input$clusterSet, {
    as.integer(unlist(strsplit(input$cluster,",")))
  })
  
  #observeEvent(input$selectButton,{
  #  if(exists(input$dataset)& !exists("dat")){
  #    dat2 <- get(input$dataset)
  #    updateTextInput(session, "cluster", value = paste(rep(1,dim(dat2)[2]), collapse = ","))
  # } else {
  #    updateTextInput(session, "cluster", value = paste(rep(1,dim(dat)[2]), collapse = ","))
  #  }
  #})
  
  
  output$itemcluster <- renderTable({
    # If missing input, return to avoid error later in function
    if(is.null(input$cluster))
      return()
    
    # Get the data set
    # dat <- get(input$dataset)
    
    # Make sure columns are correct for data set (when data set changes, the
    # columns will initially be for the previous data set)
    if (is.null(input$columns) || !(input$columns %in% names(dat())))
      return()
    
    # Get the data set
    # dat <<- dat[, input$columns, drop = FALSE]
    
    # Keep the selected columns
    clusteritem <- data.table(dim = dimVals())
    
    tclusteritem <- transpose(clusteritem)
    colnames(tclusteritem) <- colnames(dat())
    
    return(tclusteritem)
  })
  
  # Detect index
  #v <- reactiveValues(doTest = FALSE) # evt slet
  #ttestMethod <<- "sumScore" # evt slet
  
  # item selection introduction text
  output$itemSelector <- renderUI({
    HTML("Choose the items you wish to use for the detect function. All columns in the data set is selected by default. Remove columns by checking off the specific boxes. <br>The first five rows of the items selected is visualized below. Ensure that you have choosen the items of interest.")
  })
  
  # dim selection introduction text
  output$dimSelector <- renderUI({
    HTML("Enter the dimentionality of interest of the items. The number of dimensions must be equal or less to the number of  items.<br> If there is only one dimension, enter the same number (e.g. 1) as many times as there is items and comma delimited.<br> E.g. type 1,1,...,1 with as many 1's as the number of items. Type e.g. 1,2,2,1,...,2 to split the items into two dimensions and so forth.<br>When the correct length of the dimensions is entered and 'Set dimensions' is pressed,
         an overview of the items and their specific<br>belonging dimension is visualized below.")
  })
  
  # score selection introduction text
  output$scoreSelector <- renderUI({
    HTML("Choose the score of interest. The score is an ability estimate e.g. sum score or mean score. <br>The sum score is
         choosen by default.")
  })
  
  # The assumptions per test
  observeEvent(input$sumScore, 
               {output$assumptions <- renderText({paste("<h4>You have choosen to compute the DETECT index for the 
                                                        following:</h4><br>
                                                        &emsp;&emsp;1. The following items:",
                                                        paste(colnames(dat()), collapse =", "), "<br>
                                                        &emsp;&emsp;2. With the following belonging dimensions: ",
                                                        paste(dimVals(), collapse =", "), "<br>
                                                        &emsp;&emsp;3. And lastly to use the sum score as ability estimate<br><br>
                                                        To calculate the DETECT index press the following button:")})
               scoreMethod <<- "sumScore"})
  
  observeEvent(input$meanScore, 
               {output$assumptions <- renderText({paste("<h4>You have choosen to compute the DETECT index for the following:</h4><br>
                                                        &emsp;&emsp;1. The following items:",
                                                        paste(colnames(dat()), collapse =", "), "<br>
                                                        &emsp;&emsp;2. With the following belonging dimensions: ",
                                                        paste(dimVals(), collapse =", "), "<br>
                 &emsp;&emsp;3. And lastly to use the mean score as ability estimate<br><br>
                 To calculate the DETECT index press the following button:")})
               scoreMethod <<- "meanScore"})
  
  # the understand and assumptions fulfilled button
  observe({
    shinyjs::hide("understand")
    
    if(input$sumScore | input$meanScore){
      shinyjs::show("understand")
    }
  })
  
  # calculate DETECT index for uploaded data
  observeEvent(input$understand,
               {
                 if(scoreMethod == "sumScore"){
                   if(dataChoice == "upload"){
                     covar <- ccov.np(data = dat(), score = rowSums(dat(), na.rm = T), progress = FALSE)
                     res <- detect.index(covar, itemcluster = dimVals())
                     output$result <- renderText({
                       isolate({paste("<br><br><h4>4. Results</h4><hr>",round(res$unweighted[1],4),".", sep="")})})
                   } else {
                     covar <- ccov.np(data = dat(), score = rowSums(dat(), na.rm = T), progress = FALSE)
                     res <- detect.index(covar, itemcluster = dimVals())
                     output$result <- renderText({
                       isolate({paste("<br><br><h4>4. Results</h4><hr>",round(res$unweighted[1],4),".", sep="")})})
                   }
                   session$sendCustomMessage(type = "scrollCallback", 1)
                 } else if(scoreMethod=="meanScore"){
                   if(dataChoice == "upload"){
                     covar <- ccov.np(data = dat(), score = rowSums(dat(), na.rm = T)/rowSums(!is.na((dat()))),
                                      progress = FALSE)
                     res <- detect.index(covar, itemcluster = dimVals())
                     output$result <- renderText({
                       isolate({paste("<br><br><h4>4. Results</h4><hr>",round(res$unweighted[1],4),".", sep="")})})
                   } else {
                     covar <- ccov.np(data = dat(), score = rowSums(dat(), na.rm = T)/rowSums(!is.na((dat()))),
                                      progress = FALSE)
                     res <- detect.index(covar, itemcluster = dimVals())
                     output$result <- renderText({
                       isolate({paste("<br><br><h4>4. Results</h4><hr>",round(res$unweighted[1],4),".", sep="")})})
                   }
                   session$sendCustomMessage(type = "scrollCallback", 1)
                 } 
               })
  
  
  ### Multitrade
  
  #intro text to Multitrade
  output$introMultitrade <- renderText({
    "Multitrade analysis. For the multitrade analysis you need:"
  })
  
  rv <- reactiveValues()
  observeEvent(input$insertScale,
               {
                 # handle the case when user does not provide scale name
                 divID <- if (input$scales == "") input$insertScale 
                 else input$scales
                 btnID <- paste0(divID, "rmv")
                 
                 if (is.null(rv[[divID]])) { 
                   insertUI(
                     selector = "#placeholder",
                     ui = tags$div(id = divID,
                                   actionButton(btnID, "Remove this scale", class = "pull-right btn btn-danger"),
                                   textInput(paste0("txt",divID), paste("Insert items belonging to scale:",
                                                                                    input$scales), "item1, item2")
                     )
                   )
               
                   rv[[divID]] <- TRUE
                   #rv$id[[divID]] <- paste0("txt",divID)
                   
                   # create a listener on the newly-created button that will
                   # remove it from the app when clicked
                   observeEvent(input[[btnID]], {
                     removeUI(selector = paste0("#", divID))
                     
                     rv[[divID]] <- NULL
                     #rv() <- rv$id[names(rv$id) %in% divID == FALSE]      
                     
                     
                   }, ignoreInit = TRUE, once = TRUE)
                 }
                
                output$rvTest <- renderText({
                  paste(names(rv), unlist(rv))
                })
                
               })
 
  
  observe({
    shinyjs::hide("setScale")
    
    if(input$insertScale){
      shinyjs::show("setScale")
    }
  })

  observeEvent(input$setScale,
               {
                 msg <<- c()
                 if (length(rv) > 0) {
                   for (i in names(rv)) {
                     msg <- c(msg, input[[paste0("txt",i)]])
                   }
                   
                   names(msg) <- names(rv)

                   output$listForNow <- renderText({
                     paste(names(msg),msg,names(rv),".", sep="")
                   })

                 } else {
                   output$listForNow <- renderText({"hej"})
                 }
                 
                 
               }, once = FALSE)

  
  
  ### Examples
  output$testExamples <- renderText({
    "We will present examples of the types of tests possible to compute using this app"
  })
  
  
               }

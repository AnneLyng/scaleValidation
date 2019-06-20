## Purpose of script: (ui.R) Shiny app for Item Response Theory (IRT)
##
## Author: Anne Lyngholm Soerensen
##
## Date Created: 2019-05-28
##
## Copyright (c) Anne Lyngholm Soerensen, 2019
## Email: lynganne@gmail.com



# libraries
library(shiny)
library(shinydashboard)
library(shinyjs)
library(data.table)
library(DT)
library(sirt)

textInput3<-function (inputId, label, value = "",...) 
{
  div(style="display:inline-block",
      tags$label(label, `for` = inputId), 
      tags$input(id = inputId, type = "text", value = value,...))
}

# the ui
dashboardPage(
  dashboardHeader(title="R Shiny: Scale validation"),
  dashboardSidebar(sidebarMenu(
    menuItem("indexes and graphs", tabName = "ind"),
    menuItem("examples", tabName = "examples"),
    menuItem("theory", tabName="theory")
  )
  ),
  dashboardBody(
    tabItems(
      tabItem("ind",
              # Some css to style the div to make it more easily visible
              fluidRow(
                column(width=12,
                       box(width = NULL, solidHeader = TRUE,
                           status = "warning",
                           title="Steps to do scale validation using this app",
                           htmlOutput("steps"),
                           actionButton("selectButton", "Select existing data set"),
                           actionButton("uploadButton", "Use own data set"))
                )),
              # The select data-box
              fluidRow(
                column(width=5,
                       box(id = "selectData", width = NULL, #height = 350, 
                           status = "info",
                           h4("Choose data"),
                           hr(),
                           htmlOutput("dataChooseIntro"),
                           br(),
                           selectInput(inputId = "dataset",
                                       label = "Choose a dataset:",
                                       choices = c("HADS", "scqol",
                                                   "cortisol"),
                                       width=300))
                ),
                column(width=7,
                       box(id="showSelectData", width=NULL, #height = 350,
                           status = "info",
                           h4("Visualization of a subset of data (existing)"),
                           hr(),
                           htmlOutput("dataChoose"),
                           br(),
                div(style = 'overflow-x: scroll', tableOutput('contentsData'))))),
              # The upload data box
              fluidRow(
                column(width=5,
                       box(id= "uploadData", width=NULL, #height = 425,
                           status = "info",
                           h4("Upload data"),
                           hr(),
                           htmlOutput("dataLoad"),
                           br(),
                           fileInput("file1", "Choose a .csv or .txt file:",
                                     multiple = FALSE,
                                     accept = c("text/csv",
                                                "text/comma-separated-values,text/plain",
                                                ".csv", ".txt")),
                           selectInput("sep", "Choose separator:", c(comma=",", semicolon=";", tab = "\t", whitespace =" "),
                                       selected = NULL, multiple = FALSE,
                                       selectize = TRUE, width = 200, size = NULL),
                           checkboxInput("header", "Does the data have named columns (header)?", TRUE),
                           selectInput("na", "Missing value symbol", c(comma=",", dot="."),
                                       selected = NULL, multiple = FALSE,
                                       selectize = TRUE, width = 200, size = NULL),
                           box(title = "R code examples: import data", width = NULL,
                               solidHeader = FALSE, 
                               collapsible = TRUE,
                               collapsed = TRUE,
                               verbatimTextOutput("RcodeImport")
                           ))),
                # The data visualization box
                column(width=7, 
                       box(id="showUploadData",
                           width=NULL, #height = 425,
                           status = "info",
                           h4("Visualization of a subset of data (upload)"),
                           tags$hr(),
                           htmlOutput("dataLoadText"),
                           br(),
                           div(style = 'overflow-x: scroll', tableOutput('contents'))))),
              tabsetPanel(type = "tabs",
                          tabPanel("Multitrade",
                                   # The Multitrade box
                                   fluidRow(
                                     column(width=12, 
                                            box(id = "multitrade", width = NULL,
                                                h3("Choose scales, scale items, correlation and restscore"),
                                                hr(),
                                                h4("1. Choose scales"),
                                                uiOutput("scalesSelector"), #intro text to scales
                                                textInput("scales", "Write the name of the scale:", ''),
                                                actionButton("insertScale", "Add scale"),
                                                tags$div(id = "placeholder"),
                                                textOutput("rvTest"),
                                                actionButton("setScale", "Set scale(s)"),
                                                htmlOutput("listForNow")
                                                ))
                                   ),
                                   textOutput("introMultitrade")),
                          tabPanel("DETECT Index",
                                   # The t-test box
                                   useShinyjs(),
                                   fluidRow(
                                     column(width=12,
                                            box(id="detect",width = NULL, 
                                                h3("Choose items, dimensions and score"),
                                                hr(),
                                                h4("1. Choose items"),
                                                htmlOutput("itemSelector"), # intro-text choose items
                                                br(),
                                                uiOutput("choose_columns"), # check boxes items
                                                br(),
                                                tableOutput("data_table"), # visualize first two rows
                                                br(),
                                                h4("2. Choose dimensions"),
                                                uiOutput("dimSelector"),
                                                textInput('cluster', 'Enter the dimensions to the items 
                                                              (comma delimited):',''), 
                                                actionButton("clusterSet", "Set dimensions"),
                                                tableOutput("itemcluster"),
                                                h4("3. Choose score"),
                                                htmlOutput("scoreSelector"), # intro-text decide on score
                                                br(),
                                                actionButton("sumScore", "sum score"),
                                                actionButton("meanScore", "mean score"),
                                                htmlOutput("assumptions"),
                                                actionButton("understand", "Calculate DETECT index"),
                                                br(),
                                                br(),
                                                htmlOutput("result")
                                            ))
                                   )),
                          tabPanel("Monotonicity", textOutput("introMonotonicity")),
                          tabPanel("DIF", textOutput("introDIF")),
                          tabPanel("TJUR", textOutput("introTJUR"))
              )),
      tabItem("examples",
              titlePanel("Examples of app's three t-tests"),
              textOutput("testExamples"))
    )
  )
)

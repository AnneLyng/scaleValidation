## Purpose of script: Module part of scaleValidation shiny App
##
## Author: Anne Lyngholm Sørensen
##
## Date Created: 2019-06-13
##
## Copyright (c) Anne Lyngholm Soerensen, 2019
## Email: lynganne@gmail.com

HADS <- read.table("data/HADS.txt", header = T)
scqol <- read.table("data/scqol.txt", header = T, na.strings = ".")

dt_output = function(title, id) {
  fluidRow(column(
    12, h1(paste0('Table ', sub('.*?([0-9]+)$', '\\1', id), ': ', title)),
    hr(), DTOutput(id)
  ))
}

render_dt = function(data, editable = 'cell', server = TRUE, ...) {
  renderDT(data, selection = 'none', server = server, editable = editable, ...)
}
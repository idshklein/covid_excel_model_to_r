library(shiny)
library(DT)
library(tidyverse)
library(plotly)
options(scipen = 10)
source("init.R",local = TRUE)
ui = shinyUI(navbarPage(
  title = "SEIR",
  source("ui.R", local = TRUE)$value
))
server = shinyServer(function(input, output,session) {
  source("calc.R", local = TRUE, encoding = "UTF-8")
})


shinyApp(ui,server)
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinyWidgets)
library(shinyjs)
library(tidyverse)
library(data.table)


source('error_class')
source('ui/ui_basic_info.R')

# Define UI for application that draws a histogram
shinyUI(fluidPage(
  shinyjs::useShinyjs(),
  shiny::includeCSS("www/animate.css"),

  tags$head(
    tags$link(href = "https://fonts.googleapis.com/css?family=Fjalla+One|Lato|Roboto+Slab&display=swap", rel = "stylesheet"),
    tags$link(href = "styles.css", rel = "stylesheet", type = "text/css")
  ),

  tags$div(
    class = "title",
    tags$br(),
    tags$h1("Balance Data Toolkit", class = "title"),
    tags$br(),

    tags$span(icon("bolt"), class = "main-icon")
  ),
  
  tags$style(HTML("
        .tabs-above > .nav > li[class=active] > a {
           background-color: #000;
           color: #FFF;
        }")),

  verticalTabsetPanel(
    ui_basic_info(id = 'Mystery Boxes'),
    ui_basic_info(id = 'TimedIAPs'),
    ui_basic_info(id = 'Leaderboards'),
    ui_basic_info(id = 'Localization'),
    ui_basic_info(id = 'Economy')
  )
))

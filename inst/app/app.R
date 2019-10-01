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
library(shinydashboard)
library(tidyverse)
library(data.table)


tabs_names <- c("Mystery Boxes", "TimedIAPs", "Leaderboards", "Localization", "Economy", "Stories") %>% sort()
gf_names <- hs.balancedata::find_game_folder_names()

source("error_class.R")

# source("ui/ui_basic_info.R")
source("ui/ui_stories.R")

source("server/check_spreadsheet_existence.R")
source("server/update_balance_data_button.R")

header <- dashboardHeader(title = "Balance Data Tool")
sidebar <- dashboardSidebar(
  selectInput("combobox.game_location", "Game Folder Name", choices = gf_names),
  # sidebarMenu(menuItem('Stories', tabName = 'stories'), menuItem('Economy', tabName = 'economy'))
  sidebarMenuOutput("sidebar_menu_UI")
)
body <- dashboardBody(
  waiter::use_waiter(),
  uiOutput('ui_basic_info')
)
ui <- dashboardPage(header, sidebar, body, skin = "black")
server <- function(input, output, session) {

  source("ui/sidebar_menu_UI.R", local = TRUE)
  source("ui/ui_basic_info.R", local = TRUE)
  source("ui/ui_pick_economy_document.R", local = TRUE)
  
  for (tab in tabs_names) update_balance_data_button(input, output, session, name = tab)

  # Close App when closing Browser Window
  session$onSessionEnded(function() {
    stopApp()
  })
}


shinyApp(ui, server)

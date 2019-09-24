
library(shiny)

source('server/check_spreadsheet_existence.R')
source('server/update_balance_data_button.R')

shinyServer(function(input, output, session) {
  update_balance_data_button(input, output, session, name = "mysteryboxes")
  update_balance_data_button(input, output, session, name = "timediaps")
  update_balance_data_button(input, output, session, name = "leaderboards")
  update_balance_data_button(input, output, session, name = "localization")
  update_balance_data_button(input, output, session, name = "economy")
  update_balance_data_button(input, output, session, name = "stories")

  source("ui/ui_pick_economy_document.R", local = TRUE)

  # Close App when closing Browsaixer Window
  session$onSessionEnded(function() {
    stopApp()
  })
})

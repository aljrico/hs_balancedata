
library(shiny)

update_balance_data_button <- function(input, output, session, name) {
  update_functions <- list(
    mysteryboxes = hs.balancedata::update_prize_tent,
    timediaps = hs.balancedata::update_timediaps,
    leaderboards = hs.balancedata::update_leaderboard_prizes,
    localization = hs.balancedata::update_localization_file,
    economy = hs.balancedata::update_economy
  )

  button_name <- paste0(name, ".button.update_data")
  spreadsheet_name <- paste0(name, ".text.spreadsheet_name")
  game_folder_name <- paste0(name, ".combobox.game_location")
  loading_name <- paste0(name, ".loading_page")


  observeEvent(input[[button_name]], {
    shinyjs::show(loading_name)
    
    if(name == 'economy'){
      update_functions[[name]](document == input[['economy.combobox.economy_file']], game_folder = input[[game_folder_name]])
    }else{
      update_functions[[name]](spreadsheet_name = input[[spreadsheet_name]], game_folder = input[[game_folder_name]]) 
    }
    shinyjs::hide(loading_name)

    sendSweetAlert(
      session = session,
      title = "Done!",
      text = "Balance Data Updated",
      type = "success"
    )
  })
}


shinyServer(function(input, output, session) {
  update_balance_data_button(input, output, session, name = "mysteryboxes")
  update_balance_data_button(input, output, session, name = "timediaps")
  update_balance_data_button(input, output, session, name = "leaderboards")
  update_balance_data_button(input, output, session, name = "localization")
  update_balance_data_button(input, output, session, name = "economy")

  source("ui/ui_pick_economy_document.R", local = TRUE)

  # Close App when closing Browsaixer Window
  session$onSessionEnded(function() {
    stopApp()
  })
})


library(shiny)

update_balance_data_button <- function(input, output, session, name) {
  update_functions <- list(
    mysteryboxes = hs.balancedata::update_prize_tent,
    timediaps = hs.balancedata::update_timediaps
  )

  button_name <- paste0(name, ".button.update_data")
  spreadsheet_name <- paste0(name, ".text.spreadsheet_name")
  game_folder_name <- paste0(name, ".combobox.game_location")
  loading_name <- paste0(name, ".loading_page")

  observeEvent(input[[button_name]], {
    shinyjs::show(loading_name)
    update_functions[[name]](spreadsheet_name = input[[spreadsheet_name]], game_folder = input[[game_folder_name]])
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

  # Close App when closing Browser Window
  session$onSessionEnded(function() {
    stopApp()
  })
})


library(shiny)

update_balance_data_button <- function(input, output, session) {
  observeEvent(input[["mysteryboxes.button.update_data"]], {
    shinyjs::show("loading_page")
    hs.balancedata::update_prize_tent(spreadsheet_name = input$mysteryboxes.text.spreadsheet_name, game_folder = input$mysteryboxes.combobox.game_location)
    shinyjs::hide("loading_page")

    sendSweetAlert(
      session = session,
      title = "Done!",
      text = "Balance Data Updated",
      type = "success"
    )
  })

  observeEvent(input[["timediaps.button.update_data"]], {
    shinyjs::show("timdeiaps.loading_page")
    hs.balancedata::update_timediaps(spreadsheet_name = input[["timediaps.text.spreadsheet_name"]], game_folder = input[["timediaps.combobox.game_location"]])
    shinyjs::hide("timediaps.loading_page")

    sendSweetAlert(
      session = session,
      title = "Done!",
      text = "Balance Data Updated",
      type = "success"
    )
  })
}

shinyServer(function(input, output, session) {
  update_balance_data_button(input, output, session)

  # Close App when closing Browser Window
  session$onSessionEnded(function() {
    stopApp()
  })
})

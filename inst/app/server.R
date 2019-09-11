
library(shiny)

shinyServer(function(input, output, session) {
  observeEvent(input['mysteryboxes.button.update_data'], {
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


  # Close App when closing Browser Window
  session$onSessionEnded(function() {
    stopApp()
  })
})

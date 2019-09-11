
library(shiny)

shinyServer(function(input, output, session) {
    
    
  observeEvent(input$prize_tent.button.update_prize_tent, {
    shinyjs::show("loading_page")
    update_prize_tent()
    shinyjs::hide("loading_page")

    sendSweetAlert(
      session = session,
      title = "Done!",
      text = "Prize Tent Updated",
      type = "success"
    )
  })


  # Close App when closing Browser Window
  session$onSessionEnded(function() {
    stopApp()
  })
})

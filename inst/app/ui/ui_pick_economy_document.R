output$ui_pick_economy_document <- renderUI({
  economy_documents_list <- hs.balancedata::find_economy_documents(game_folder = input$economy.combobox.game_location)
  choices_economy <- NA
  if (length(economy_documents_list) > 0) choices_economy <- economy_documents_list

  selectInput("economy.combobox.economy_file", "Economy Document", choices = choices_economy)
})

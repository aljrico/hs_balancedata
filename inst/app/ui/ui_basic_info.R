output$ui_basic_info <- renderUI({
  small_id <- tolower(stringr::str_remove_all(tabs_names, " "))
  myTabs <- c()
  for (i in 1:length(small_id)) {
    
    title <- h1(tabs_names[[i]])
    update_box <-  box(width = 4, 
      if (small_id[[i]] != "economy") textInput(paste0(small_id[[i]], ".text.spreadsheet_name"), "Spreadsheet Name", value = paste0("(HS) ", small_id[[i]])),
      if (small_id[[i]] == "economy") uiOutput("ui_pick_economy_document"),
      actionButton(paste0(small_id[[i]], ".button.update_data"), "Update Balance Data")
    )
    myTabs[[i]] <- tabItem(title, update_box, tabName = small_id[[i]])
  }
  do.call(tabItems, myTabs)
})

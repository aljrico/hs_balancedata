output$sidebar_menu_UI <- renderUI({
  small_id <- tolower(stringr::str_remove_all(tabs_names, " "))
  myTabs <- c()
  for (i in 1:length(small_id)) {
    myTabs[[i]] <- menuItem(tabName = small_id[[i]], text = tabs_names[[i]])
  }
  do.call(sidebarMenu, myTabs)
})

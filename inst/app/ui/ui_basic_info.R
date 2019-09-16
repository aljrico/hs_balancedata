ui_basic_info <- function(id = 'Timed IAPs'){
  
  small_id <- tolower(stringr::str_remove_all(id, ' '))
  gf_names <- hs.balancedata::find_game_folder_names()
  
  verticalTabPanel(id = small_id, title = id, fluid = TRUE, box_height = "70px", color = '#000000',
                   sidebarLayout(
                     sidebarPanel(
                       textInput(paste0(small_id , ".text.spreadsheet_name"), "Spreadsheet Name", value = paste0("(HS) ", small_id)),
                       selectInput(paste0(small_id, ".combobox.game_location"), "Game Folder Name", choices = gf_names),
                       actionButton(paste0(small_id, ".button.update_data"), 'Update Balance Data')
                     ),
                     mainPanel(
                       shinyjs::hidden(
                         div(
                           id = paste0(small_id, ".loading_page"),
                           class = "loading-content",
                           h2(class = "animated infinite pulse", "Loading data..."),
                           align = "center"
                         )
                       )
                     )
                   )
  )
}
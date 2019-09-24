ui_stories <- function(id){
  
  small_id <- tolower(stringr::str_remove_all(id, ' '))
  gf_names <- hs.balancedata::find_game_folder_names()
  
  sh_name <- paste0("(HS) ", small_id)
  verticalTabPanel(id = small_id, title = id, fluid = TRUE, box_height = "70px", color = '#000000',
                   sidebarLayout(
                     sidebarPanel(
                       uiOutput('ui_pick_economy_document'),
                       selectInput("stories.combobox.game_location", "Game Folder Name", choices = gf_names),
                       checkboxInput('stories.checkbox.timeids', label = 'Update Time IDs?', value = FALSE),
                       actionButton("stories.button.update_data", 'Update Balance Data')
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
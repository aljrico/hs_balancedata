ui_timediaps <- function(id = 'TimedIAPs'){
  verticalTabPanel("TimedIAPs", fluid = TRUE, box_height = "70px", color = '#000000',
                   sidebarLayout(
                     sidebarPanel(
                       textInput("timediaps.text.spreadsheet_name", "Spreadsheet Name", value = "(HS) timediaps"),
                       selectInput("timediaps.combobox.game_location", "Game Folder Name", choices = c("homestreet", "spark")),
                       actionButton("timediaps.button.update_prize_tent", "Update TimedIAPs")
                     ),
                     mainPanel(
                       shinyjs::hidden(
                         div(
                           id = "loading_page",
                           class = "loading-content",
                           h2(class = "animated infinite pulse", "Loading data..."),
                           align = "center"
                         )
                       )
                     )
                   )
  )
}
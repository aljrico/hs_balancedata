update_balance_data_button <- function(input, output, session, name) {
  
  there_has_been_an_error <- FALSE
  
  update_functions <- list(
    mysteryboxes = hs.balancedata::update_prize_tent,
    timediaps = hs.balancedata::update_timediaps,
    leaderboards = hs.balancedata::update_leaderboard_prizes,
    localization = hs.balancedata::update_localization_file,
    economy = hs.balancedata::update_economy
  )
  
  button_name <- paste0(name, ".button.update_data")
  spreadsheet_name <- paste0(name, ".text.spreadsheet_name")
  game_folder_name <- paste0(name, ".combobox.game_location")
  loading_name <- paste0(name, ".loading_page")
  
  
  observeEvent(input[[button_name]], {
    shinyjs::show(loading_name)
    
    if(name == 'economy'){
      update_functions[[name]](document = input[['economy.combobox.economy_file']], game_folder = input[[game_folder_name]])
    }else{
      sh_exists <- check_spreadsheet_existence(sh_name = input[[spreadsheet_name]])
      if(sh_exists){
        update_functions[[name]](spreadsheet_name = input[[spreadsheet_name]], game_folder = input[[game_folder_name]]) 
      }else{
        there_has_been_an_error <- TRUE
        
        sendSweetAlert(
          session = session,
          title = "Error!",
          text = "Spreadsheet does not exist with this name. Please check it.",
          type = "error"
        )
      }
    }
    shinyjs::hide(loading_name)
    
    if(!there_has_been_an_error){
      sendSweetAlert(
        session = session,
        title = "Done!",
        text = "Balance Data Updated",
        type = "success"
      )
    }
  })
}
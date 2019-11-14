update_balance_data_button <- function(input, output, session, name) {
  
  name <- tolower(stringr::str_remove_all(name, " "))
  
  there_has_been_an_error <- FALSE
  
  update_functions <- list(
    mysteryboxes = hs.balancedata::update_prize_tent,
    timediaps = hs.balancedata::update_timediaps,
    leaderboards = hs.balancedata::update_leaderboard_prizes,
    localization = hs.balancedata::update_localization_file,
    economy = hs.balancedata::update_economy,
    stories = hs.balancedata::update_stories
  )
  
  button_name <- paste0(name, ".button.update_data")
  spreadsheet_name <- paste0(name, ".text.spreadsheet_name")
  game_folder_name <- "combobox.game_location"
  loading_name <- paste0(name, ".loading_page")
  
  
  observeEvent(input[[button_name]], {
    waiter::show_waiter(waiter::spin_folding_cube())
    
    if(name == 'economy'){
      update_functions[[name]](document = input[['combobox.economy_file']], game_folder = input[[game_folder_name]])
    }else if(name == 'stories'){
      update_functions[[name]](economy_file = input[['combobox.stories.economy_file']], game_folder = input[[game_folder_name]], release_version = input[['combobox.stories.economy_file']] %>% str_extract_all('[[0-9]]') %>% unlist() %>% paste(collapse = '') %>% as.numeric())
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
    waiter::hide_waiter()
    
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
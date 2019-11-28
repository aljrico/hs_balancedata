#' @export
update_quest_localisation <- function(new_quest, seasonalquest_prod, quest_id, this_design_table, game_folder){
  
  loc <- localisation_sh %>% tail(1) %>% data.frame()
  loc[1,] <- NA
  intro_text <- this_design_table[['Intro Text']] %>% stringr::str_split(' - ') %>% unlist()
  completed_text <- this_design_table[['Complete Text']]
  expired_text <- this_design_table[['Miss Text']]
  task1_text <- this_design_table[['Task Text 1']]
  task2_text <- this_design_table[['Task Text 2']]
  task3_text <- this_design_table[['Task Text 3']]
  title_text <- paste0(this_design_table[['Quest Title']])
  description_text <- this_design_table[['Description Text']] %>% stringr::str_remove_all(' - ')
  
  if(any((intro_text %>% stringr::str_count()) > 120)){
    stop('Script too long!')
  }
  string_types <- c()
  for(intro_l in seq_along(intro_text)){
    string_types[[intro_l]] <- paste0('Intro.', intro_l)
  }
  string_types <- string_types %>% c('Completed', 'Expired', 'Task1', 'Task2', 'Task3', 'Title', 'Description')
  
  
  # Generate String ID ------------------------------------------------------

  string_identifier <- c()
  string_values <- c(
    intro_text,
    completed_text,
    expired_text,
    task1_text,
    task2_text,
    task3_text,
    title_text,
    description_text
  ) %>% 
    stringr::str_trim()
  
  for(strs in seq_along(string_types)){
    string_identifier[[strs]] <- paste0(
      'QuestText',
      string_types[[strs]],
      '_',
      this_design_table[['Event Name']], 
      '_', 
      this_design_table[['Quest ID']]
    )
  }
  
  loc_df <<- tibble::tibble(`String.Identifier` = string_identifier, 
                   `English` = string_values) %>% 
    dplyr::left_join(loc) %>% 
    dplyr::select(`String.Identifier`, `String.Description`, everything()) %>% 
    dplyr::filter(!(English %>% is.na()))
  
  stories_sheet %>% 
    googlesheets::gs_add_row(ws = "seasonal_scripts", 
               input = loc_df %>% data.table::data.table()
    )
}

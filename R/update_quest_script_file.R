#' @export
update_quest_script_file <- function(new_quest, seasonalquest_prod, quest_id, this_design_table, game_folder){
  script_ids <- hs.balancedata::get_scripts_id(game_folder = game_folder)
  last_script_id <- script_ids %>% tail(1) %>% .$id %>% as.numeric()
  
  script_categories <- loc_df$String.Identifier %>% 
    stringr::str_remove_all('QuestText') %>% 
    stringr::str_remove_all(this_design_table$`Event Name` %>% as.character()) %>% 
    stringr::str_remove_all(this_design_table$`Quest ID` %>% as.character()) %>% 
    stringr::str_remove_all('_')
  
  script_super_categories <- c('Intro')
  
  
  scripts_txt <- readr::read_file('~/homestreet/Assets/data/source/xml/scripts.xml') %>% stringr::str_remove('\r\n</group>\r\n')
  
  new_actor_id_script <- this_design_table$`Actor ID` %>% as.character()
  new_whole_quest_script <- c()
  
  for(scrs in seq_along(script_super_categories)){
    new_simple_script <- ''
    sub_categories <- script_categories %>% .[which(script_categories %>% stringr::str_detect(script_super_categories[[scrs]]))]
    for(sub_scrs in seq_along(sub_categories)){
      new_simple_script <- paste0(
        new_simple_script,
        '\t\t\t', 
        '<speech actorId=\"', 
        new_actor_id_script, 
        '\" portrait=\"neighbour\" text=\"', 
        loc_df$String.Identifier %>% .[which(loc_df$String.Identifier %>% stringr::str_detect(script_super_categories[[scrs]]))] %>% .[[sub_scrs]], 
        '\" actionText=\"TapToContinue\" animation=\"Happy2\" blocking=\"true\"/> \n'
      )
    }
    
    new_whole_quest_script[[scrs]] <- paste0('\t<scripts id=\"',
                                             last_script_id + scrs,
                                             '\" name=\"', 
                                             this_design_table$`Event Name`, 
                                             ' ',
                                             script_super_categories[[scrs]],
                                             '\">\r\n\t\t<scripts>\r\n\t\t\t<disableInputs/>\r\n\t\t\t<pauseGameActions paused=\"true\"/>\r\n\t\t\t<wait seconds=\"2\"/>\r\n\t\t\t<enableInputs executeInSkip=\"true\"/>\r\n', 
                                             new_simple_script, 
                                             '\t\t\t<showQuestMenu questId=\"',
                                             this_design_table$`Quest ID` %>% as.character(), 
                                             '\"/>\r\n\t\t\t<pauseGameActions paused=\"false\" executeInSkip=\"true\"/>\r\n\t\t</scripts>\r\n\t</scripts> '
    )
    scripts_txt <- scripts_txt %>% paste0(new_whole_quest_script[[scrs]])
  }
  
  
  scripts_txt %>% stringr::str_remove_all('</group>') %>% paste0('\r\n</group>\r\n') %>%  readr::write_file('~/homestreet/Assets/data/source/xml/scripts.xml')
  
}

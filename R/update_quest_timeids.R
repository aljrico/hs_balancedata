#' @export
update_quest_timeids <- function(start_date, end_date, code_name, last_time_id, update_times_xml = FALSE) {
  
  
  event_dates <- hs.balancedata::get_event_dates() %>% dplyr::mutate(type = 'event')
  season_dates <- hs.balancedata::get_season_dates() %>% dplyr::mutate(type = 'season')
  bingo_dates <- hs.balancedata::get_bingo_dates() %>% dplyr::mutate(type = 'bingo') 
  quest_dates <- hs.balancedata::get_quest_dates() %>% dplyr::mutate(type = 'quest')
  
  time_ids <- event_dates %>% rbind(season_dates) %>% rbind(bingo_dates) %>% rbind(quest_dates)
  
  # Correct one, for good practice
  this_time_id <- quest_dates %>% 
    dplyr::filter(start_date == this_quest_human$Start) %>% 
    dplyr::filter(end_date == this_quest_human$End) %>% 
    .$time_id
  
  if(length(this_time_id) < 1){
    
    if(is.na(this_quest_human$Start) | is.na(this_quest_human$End)) stop('Time ID not defined')
    last_time_id <- time_ids %>% dplyr::arrange(time_id) %>% .$time_id %>%  tail(1) %>% as.numeric()
    
    update_quest_timeids(
      start_date = this_quest_human$Start,
      end_date = this_quest_human$End,
      code_name = this_quest_human$`Event Name`,
      last_time_id = last_time_id
    )
    
    this_time_id <- last_time_id + 1
    
  }else{
    new_quest[, `times id` := this_time_id]
  }
  
  if(update_times_xml){
    
    generated_folder <- hs.balancedata::find_source_folder(game_folder = game_folder) %>% stringr::str_replace("data/source", "data/generated")
    x_file <- generated_folder %>% paste0("/xml/times_prod.xml")
    
    x <-
      readr::read_file(x_file) %>%
      stringr::str_remove("</group>")
    
    
    paste0(
      x,
      "
",
'\t<times id="', last_time_id + 1,
'" name="', code_name,
'" category="Quest">
    \t<specificTime startDate="', start_date,
'T11:00:00Z" endDate="', end_date,
'T11:00:00Z" />
    </times>',
"
</group>"
    ) %>%
      readr::write_file(x_file)
  }
  
}

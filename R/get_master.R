#' @export
get_master <- function(economy_file){
  
  df <- economy_file %>% readxl::read_excel(sheet = 'MASTER', skip = 4) %>% select(-`...1`, -`...2`)
  
  
  events <- df %>% 
    filter(`Item Name` %>% stringr::str_detect('Event') | `Workbench Name` %>% stringr::str_detect('Bingo')) %>% 
    .[['Workbench Name']]
  
  events_pos <- which(df$`Workbench Name` %in% events)
  df$event <- 'permanent'
  
  for(i in seq_along(events_pos)){
    df[events_pos[[i]]:(df$event %>% length()), 'event'] <- events[[i]] %>% stringr::str_remove_all(' ')
  }
  
  return(df)
}
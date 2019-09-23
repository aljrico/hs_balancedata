#' @export
update_timediaps <- function(spreadsheet_name = '(HS) timediaps', game_folder = 'homestreet'){
  hs.balancedata::gs_credentials()
  
  spreadsheet_name %>% 
    googlesheets::gs_title() %>% 
    googlesheets::gs_read(ws = 'timediaps_prod') %>% 
    data.table::data.table() %>% 
    data.table::fwrite(paste0('~/', game_folder, '/Assets/data/source/csv/timediaps_prod.csv'))
}




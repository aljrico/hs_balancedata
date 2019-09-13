#' @export
update_localization_file <- function(spreadsheet_name = '(HS) localization', game_folder = 'homestreet'){
  hs.balancedata::gs_credentials()
  spreadsheet_name %>% 
    googlesheets::gs_title() %>% 
    googlesheets::gs_read(ws = 'Main') %>% 
    data.table::data.table() %>% 
    data.table::fwrite(paste0('~/', game_folder, '/Assets/data/source/csv/localization.csv')) %>% 
    return()
}
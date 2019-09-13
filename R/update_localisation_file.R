#' @export
update_localisation_file <- function(spreadsheet_name = 'Home Street SPARK Localization', game_folder = 'homestreet'){
  hs.balancedata::gs_credentials()
  spreadsheet_name %>% 
    googlesheets::gs_title() %>% 
    googlesheets::gs_read(ws = 'Main') %>% 
    data.table::data.table() %>% 
    dplyr::select(-`X14`) %>% 
    data.table::fwrite(paste0('~/', game_folder, '/Assets/data/source/csv/localization.csv')) %>% 
    return()
}
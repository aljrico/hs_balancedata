#' @export
update_stories <- function(spreadsheet_name = "(HS) stories", game_folder = "homestreet", economy_file, release_version) {
  
  hs.balancedata::gs_credentials()
  
  # Dependencies: Balance Data
  event_dates <- hs.balancedata::get_event_dates(game_folder = game_folder)
  season_items <- hs.balancedata::get_season_items()
  
  # Set up initial files
  economy_path <- hs.balancedata::find_economy_document_folder(game_folder = game_folder)
  spark_economy_file <- paste0(economy_path, '/', document)
  file_version <- paste0('seasonalquests_prod.csv (0.', release_version, ')')
  
  
  # Load Spreadsheets
  stories_sheet        <- spreadsheet_name %>% googlesheets::gs_title() 
  design_table         <- stories_sheet %>% googlesheets::gs_read(ws = 'seasonal_design')
  seasonalquest_prod   <- stories_sheet %>% googlesheets::gs_read(ws = file_version)
  task_types           <- stories_sheet %>% googlesheets::gs_read(ws = 'tasks hub')
  
  which_ids_are_new <- function(design_table){
    design_table %>% 
      dplyr::filter(`Accepted` == 'ok') %>% 
      .$`Quest ID` %>% 
      hs.balancedata::outersect(seasonalquest_prod$`# id`) %>% 
      intersect(
        design_table %>% 
          filter(`Accepted` == 'ok') %>% .$`Quest ID`
      ) %>% 
      return()
  }
  
  if(length(which_ids_are_new) == 0) return(new('appError', error_msg = 'No new Stories ID to be found.'))
  
  return(NA)
}

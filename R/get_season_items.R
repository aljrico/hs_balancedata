#' @export
get_season_items <- function() {

  # Identify
  hs.balancedata::gs_credentials()

  # Load Spreadsheets
  items_prod_tracker <- googlesheets::gs_title("[Home Street] Items Art Production Tracker")
  production_tracker_file <- "items_prod_tracker.xlsx"

  get_tab_list <- function(production_tracker_file = "items_prod_tracker.xlsx") {
    tab_list <- readxl::excel_sheets("items_prod_tracker.xlsx")
    tab_list <- tab_list[1:min((which(tab_list %>% tolower() %>% stringr::str_detect("template"))) - 1)]
    tab_list <- tab_list[-which(tab_list == "Halloween 2017")]
    tab_list <- tab_list[-which(tab_list %>% stringr::str_detect("Index"))]
    # tab_list <- tab_list[-which(tab_list %>% str_detect('Bingo'))]
    return(tab_list)
  }
  get_items_list <- function(production_tracker_file, tab_list) {
    all_items_list <- list()
    for (i in seq_along(tab_list)) {
      items_ss <- readxl::read_excel(production_tracker_file, sheet = tab_list[[i]])
      
      colnames(items_ss)[which(items_ss %>% str_detect("Shop Items"))] <- "Shop Items"
      colnames(items_ss)[which(items_ss %>% str_detect("Production Name"))] <- "Shop Items"
      
      items_ids <- items_ss %>%
        dplyr::select(`Shop Items`, `ADDED VIA TOOL (ID)`, `TYPE`) %>%
        stats::na.omit() %>%
        purrr::set_names("Item", "Id", "Type") %>%
        dplyr::mutate(Id = as.character(Id)) %>%
        dplyr::mutate(reused = (Id == "-")) %>%
        dplyr::mutate(event = tab_list[[i]])
      
      all_items_list[[i]] <- items_ids
      Sys.sleep(1)
    }
    
    file.remove(production_tracker_file)
    
    items_df <- all_items_list %>% 
      purrr::reduce(rbind) %>% 
      dplyr::mutate(event = event %>% stringr::str_remove_all(' ')) %>% 
      dplyr::mutate(Id = as.character(as.numeric(Id))) %>% 
      dplyr::filter(!is.na(Id)) %>% 
      data.table::data.table()
    
    
    
    return(items_df)
  }
  
  googlesheets::gs_download(items_prod_tracker, to = production_tracker_file, overwrite = TRUE)
  tab_list <- get_tab_list(production_tracker_file = production_tracker_file)
  items_list <- get_items_list(production_tracker_file = production_tracker_file, tab_list = tab_list)
  
  return(items_list)
}

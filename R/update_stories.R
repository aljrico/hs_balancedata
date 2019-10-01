#' @export
update_stories <- function(spreadsheet_name = "(HS) stories", game_folder = "homestreet", economy_file, release_version) {
  check_new_ids <- function(design_table) {
    design_table %>%
      dplyr::filter(`Accepted` == "ok") %>%
      .$`Quest ID` %>%
      hs.balancedata::outersect(seasonalquest_prod$`# id`) %>%
      intersect(
        design_table %>%
          filter(`Accepted` == "ok") %>% .$`Quest ID`
      ) %>%
      return()
  }

  hs.balancedata::gs_credentials()

  # Dependencies: Balance Data
  event_dates <- hs.balancedata::get_event_dates(game_folder = game_folder)
  season_items <- hs.balancedata::get_season_items()

  # Set up initial files
  economy_path <- hs.balancedata::find_economy_document_folder(game_folder = game_folder)
  spark_economy_file <- paste0(economy_path, "/", economy_file)
  file_version <- paste0("seasonalquests_prod.csv (0.", release_version, ")")

  # Load Spreadsheets
  stories_sheet      <- spreadsheet_name %>% googlesheets::gs_title()
  design_table       <- stories_sheet %>% googlesheets::gs_read(ws = "seasonal_design")
  seasonalquest_prod <- stories_sheet %>% googlesheets::gs_read(ws = file_version)
  task_types         <- stories_sheet %>% googlesheets::gs_read(ws = "tasks hub")
  localisation_sh    <- stories_sheet %>% googlesheets::gs_read(ws = "seasonal_scripts")

  new_ids <- check_new_ids(design_table)

  if (length(new_ids) == 0) {
    return(new("appError", error_msg = "No new Stories ID to be found."))
  }

  for (q in seq_along(new_ids)) {
    quest_id <- new_ids[[q]]
    this_design_table <- design_table %>% dplyr::filter(`Quest ID` == quest_id)
    
    new_quest <- seasonalquest_prod %>%
      tail(1) %>%
      data.table::data.table()
    
    new_quest <- new_quest %>% 
      hs.balancedata::update_quest_assets_ids(seasonalquest_prod = seqsonalquest_prod, 
                                              quest_id = quest_id, 
                                              this_design_table = this_design_table)
  }

  return(NA)
}


#' @export
update_quest_assets_ids <- function(new_quest, seasonalquest_prod, quest_id, this_design_table) {

  new_quest[, `# id` := quest_id, ]
  new_quest[, title := paste0("QuestTextTitle", "_", this_design_table$`Event Name`, "_", quest_id)]
  new_quest[, description := paste0("QuestTextDescription", "_", this_design_table$`Event Name`, "_", quest_id)]
  new_quest[, `completed text` := paste0("QuestTextCompleted", "_", this_design_table$`Event Name`, "_", quest_id)]
  new_quest[, `expired text` := paste0("QuestTextExpired", "_", this_design_table$`Event Name`, "_", quest_id)]
  new_quest[, `icon prefab` := NA] # No Icon
  new_quest[, `level` := this_design_table$`Level Unlock`]
  new_quest[, `actor id` := this_design_table$`Actor ID`]
  new_quest[, `complete actor id` := this_design_table$`Actor ID`]
  new_quest[, `expire actor id` := this_design_table$`Actor ID`]
  new_quest[, `actor animation` := this_design_table$`Actor Animation`]

  return(new_quest)
}

#' @export
update_quest_script_ids <- function(new_quest, seasonalquest_prod, quest_id, this_design_table) {
  
  last_script <- c(seasonalquest_prod$`start script id`, 
                   seasonalquest_prod$`end script id`, 
                   seasonalquest_prod$`expire script id`) %>% 
    as.numeric() %>% 
    max(na.rm = TRUE)
  
  script_ids <- hs.balancedata::get_scripts_id()
  
  this_event <- this_design_table$`Event Name`
  this_chapter <- this_design_table$`Chapter`
  
  new_quest[, `start script id` := NA]
  new_quest[, `end script id` := NA]
  new_quest[, `expire script id` := NA]
  
  script_ids_array <- script_ids %>% 
    dplyr::filter(script_name %>% stringr::str_detect(this_event)) %>% 
    dplyr::filter(script_name %>% stringr::str_detect('Intro')) %>% 
    .$id
  
  if(length(script_ids_array) > 0){
    if(max(as.numeric(script_ids_array)) > last_script){
      if(this_chapter > 1 & !is.na(this_chapter)){
        new_script <- script_ids_array[[this_chapter]]
      }else{
        if(is.na(this_chapter)){
          new_script <- last_script + 1
        }else{
          new_script <- script_ids_array[[1]]
        }
      }
    }else{
      new_script <- last_script + 1
    }
  }else{
    new_script <- script_ids %>% arrange(id) %>% .$id %>% as.numeric() %>% max() %>% `+`(1)
  }
  
  if(!is.na(this_quest_human$`Intro Text`)){
    new_quest[, `start script id` := new_script]
  }
  
}

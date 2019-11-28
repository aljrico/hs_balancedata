#' @export
update_quest_scripts_ids <- function(new_quest, seasonalquest_prod, quest_id, this_design_table, game_folder) {
  last_script <- c(
    seasonalquest_prod$`start script id`,
    seasonalquest_prod$`end script id`,
    seasonalquest_prod$`expire script id`
  ) %>%
    as.numeric() %>%
    max(na.rm = TRUE)

  script_ids <- hs.balancedata::get_scripts_id(game_folder = game_folder)

  this_event <- this_design_table$`Event Name`
  this_chapter <- this_design_table$`Chapter`

  new_quest[, `start script id` := NA]
  new_quest[, `end script id` := NA]
  new_quest[, `expire script id` := NA]

  script_ids_array <- script_ids %>%
    dplyr::filter(script_name %>% stringr::str_detect(this_event)) %>%
    dplyr::filter(script_name %>% stringr::str_detect("Intro")) %>%
    .$id

  if (length(script_ids_array) > 0) {
    if (max(as.numeric(script_ids_array)) > last_script) {
      if (this_chapter > 1 & !is.na(this_chapter)) {
        new_script <- script_ids_array[[this_chapter]]
      } else {
        if (is.na(this_chapter)) {
          new_script <- last_script + 1
        } else {
          new_script <- script_ids_array[[1]]
        }
      }
    } else {
      new_script <- last_script + 1
    }
  } else {
    new_script <- script_ids %>%
      arrange(id) %>%
      .$id %>%
      as.numeric() %>%
      max() %>%
      `+`(1)
  }

  if (!is.na(this_design_table$`Intro Text`)) {
    new_quest[, `start script id` := new_script]
  }
  return(new_quest)
}

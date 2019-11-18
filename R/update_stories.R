#' @export
update_stories <- function(spreadsheet_name = "(HS) stories", game_folder = "homestreet", economy_file, release_version, prod_checkbox) {
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
  # spark_economy_file <- paste0(economy_path, economy_file)
  spark_economy_file <- economy_file
  file_version <- paste0("seasonalquests_prod.csv (0.", release_version, ")")
  source_folder <- hs.balancedata::find_source_folder(game_folder = game_folder)

  # Load Spreadsheets
  stories_sheet <- spreadsheet_name %>% googlesheets::gs_title()
  design_table <- stories_sheet %>% googlesheets::gs_read(ws = "seasonal_design")
  seasonalquest_prod <- stories_sheet %>% googlesheets::gs_read(ws = file_version)
  task_types <- stories_sheet %>% googlesheets::gs_read(ws = "tasks hub")
  localisation_sh <- stories_sheet %>% googlesheets::gs_read(ws = "seasonal_scripts")

  original_length <- nrow(seasonalquest_prod)
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

    if (prod_checkbox) {
      new_quest <- new_quest %>%
        hs.balancedata::update_quest_assets_ids(
          seasonalquest_prod = seqsonalquest_prod,
          quest_id = quest_id,
          this_design_table = this_design_table
        )

      new_quest <- new_quest %>%
        hs.balancedata::update_quest_scripts_ids(
          seasonalquest_prod = seasonalquest_prod,
          quest_id = quest_id,
          this_design_table = this_design_table,
          game_folder = game_folder
        )

      new_quest <- new_quest %>%
        hs.balancedata::update_quest_rewards(
          seasonalquest_prod = seasonalquest_prod,
          quest_id = quest_id,
          this_design_table = this_design_table
        )

      new_quest <- new_quest %>%
        hs.balancedata::update_quest_multichapter(
          quest_id = quest_id,
          this_design_table = this_design_table
        )

      new_quest <- new_quest %>%
        hs.balancedata::update_quest_tasks(
          seasonalquest_prod = seasonalquest_prod,
          quest_id = quest_id,
          this_design_table = this_design_table,
          task_types = task_types,
          economy_file = spark_economy_file
        )
    }


    seasonalquest_prod <- seasonalquest_prod %>% rbind(new_quest)
  }

  submit_stories(
    seasonalquest_prod = seasonalquest_prod,
    original_length = original_length,
    source_folder = source_folder,
    file_version = file_version
  )

  return(NA)
}

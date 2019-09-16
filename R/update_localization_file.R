#' @export
update_localization_file <- function(spreadsheet_name = "(HS) localization", game_folder = "homestreet") {
  hs.balancedata::gs_credentials()
  source_folder <- hs.balancedata::find_source_folder(game_folder = game_folder)
  spreadsheet_name %>%
    googlesheets::gs_title() %>%
    googlesheets::gs_read(ws = "Main") %>%
    data.table::data.table() %>%
    data.table::fwrite(paste0(source_folder, "/csv/localization.csv")) %>%
    return()
}

#' @export
update_quest_timeids <- function(start_date, end_date, code_name, last_time_id) {
  
  generated_folder <- hs.balancedata::find_source_folder(game_folder = game_folder) %>% stringr::str_replace('data/source', 'data/generated')
  x_file <- generated_folder %>% paste0('/xml/times_prod.xml')
  
  x <-
    readr::read_file(x_file) %>%
    stringr::str_remove("</group>")


  paste0(
    x,
    "
",
    '\t<times id="', last_time_id + 1,
    '" name="', code_name,
    '" category="Quest">
    \t<specificTime startDate="', start_date,
    'T11:00:00Z" endDate="', end_date,
    'T11:00:00Z" />
    </times>',
    "
</group>"
  ) %>%
    readr::write_file(x_file)
}

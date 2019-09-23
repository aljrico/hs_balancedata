update_quest_timeids <- function(start_date, end_date, code_name, last_time_id) {
  x <-
    readr::read_file("~/homestreet/Assets/data/generated/xml/times_prod.xml") %>%
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
    readr::write_file("~/homestreet/Assets/data/generated/xml/times_prod.xml")
}

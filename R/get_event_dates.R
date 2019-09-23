#' @export
get_event_dates <- function(game_folder) {

  generated_folder <- hs.balancedata::find_source_folder(game_folder = game_folder) %>% stringr::str_replace('data/source', 'data/generated')
  x_file <- generated_folder %>% paste0('/xml/times_prod.xml')
  x <- xml2::read_xml(x_file)

  # Dates
  dates_df <-
    x %>%
    xml2::xml_find_all("//specificTime") %>%
    purrr::map_df(~ list(
      attrs = list(xml2::xml_attrs(.x)),
      variable = list(map(xml2::xml_children(.x), xml2::xml_name)),
      value = list(map(xml2::xml_children(.x), xml2::xml_text))
    )) %>%
    dplyr::mutate(n = 1:n()) %>%
    dplyr::select(attrs, n) %>%
    tidyr::unnest(attrs) %>%
    dplyr::mutate(date = as.Date(attrs) %>% lubridate::ymd()) %>%
    dplyr::group_by(n) %>%
    dplyr::mutate(end_date = max(date)) %>%
    dplyr::mutate(start_date = min(date)) %>%
    dplyr::ungroup() %>%
    dplyr::select(n, end_date, start_date) %>%
    dplyr::distinct()

  # Attributes
  attr_df <- x %>%
    xml2::xml_find_all("//times") %>%
    purrr::map_df(~ list(
      attrs = list(xml2::xml_attrs(.x)),
      variable = list(map(xml2::xml_children(.x), xml2::xml_name)),
      value = list(map(xml2::xml_children(.x), xml2::xml_text))
    )) %>%
    dplyr::mutate(n = 1:n()) %>%
    dplyr::select(attrs, n) %>%
    tidyr::unnest(attrs) %>%
    dplyr::mutate(contains_numeric = attrs %>% stringr::str_detect("\\d")) %>%
    dplyr::mutate(pure_numeric = !(attrs %>% as.numeric() %>% is.na())) %>%
    dplyr::mutate(attribute = ifelse(pure_numeric, "id", ifelse(contains_numeric, "event", "category"))) %>%
    dplyr::select(attrs, attribute) %>%
    dplyr::mutate(attribute = ifelse(stringr::str_count(attrs, "\\S+") > 1, "event", attribute))
  # dplyr::distinct() %>%
  # reshape2::dcast(formula = attrs ~ attribute, value.var = 'attrs')

  ids <- attr_df %>%
    dplyr::filter(attribute == "id") %>%
    .$attrs
  events <- attr_df %>%
    dplyr::filter(attribute == "event") %>%
    .$attrs
  categories <- attr_df %>%
    dplyr::filter(attribute == "category") %>%
    .$attrs


  event_dates <- tibble(ids, events, categories) %>%
    dplyr::mutate(n = 1:n()) %>%
    dplyr::filter(categories %>% stringr::str_detect("Event")) %>%
    inner_join(dates_df) %>%
    dplyr::mutate(events = events %>% stringr::str_replace(" Event", "")) %>%
    dplyr::mutate(code_name = events %>% stringr::str_replace_all(" ", "")) %>%
    dplyr::filter(!(code_name %>% stringr::str_detect("Mia House"))) %>%
    dplyr::filter(!(code_name %>% stringr::str_detect("Mia house"))) %>%
    dplyr::filter(!(code_name %>% stringr::str_detect("MiaHouse"))) %>%
    dplyr::filter(!(code_name %>% stringr::str_detect("Miahouse"))) %>%
    # dplyr::mutate(code_name = paste0('Event', code_name)) %>%
    dplyr::select(time_id = ids, code_name, start_date, end_date) %>%
    data.table::data.table()


  return(event_dates)
}

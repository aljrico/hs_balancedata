#' @export
get_event_dates <- function(game_folder) {
  generated_folder <- hs.balancedata::find_source_folder(game_folder = game_folder) %>% stringr::str_replace("data/source", "data/generated")
  x_file <- generated_folder %>% paste0("/xml/times_prod.xml")
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
    dplyr::inner_join(dates_df) %>%
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

#' @export
get_season_dates <- function(game_folder){
  generated_folder <- hs.balancedata::find_source_folder(game_folder = game_folder) %>% stringr::str_replace("data/source", "data/generated")
  x_file <- generated_folder %>% paste0("/xml/times_prod.xml")
  x <- xml2::read_xml(x_file)
  
  # Dates
  dates_df <- x %>% 
    xml2::xml_find_all('//specificTime') %>%
    purrr::map_df(~list(attrs = list(xml2::xml_attrs(.x)), 
                 variable = list(map(xml2::xml_children(.x), xml2::xml_name)), 
                 value = list(map(xml2::xml_children(.x), xml2::xml_text)))) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::select(attrs,n) %>% 
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
    xml2::xml_find_all('//times') %>%
    purrr::map_df(~list(attrs = list(xml2::xml_attrs(.x)), 
                 variable = list(map(xml2::xml_children(.x), xml2::xml_name)), 
                 value = list(map(xml2::xml_children(.x), xml2::xml_text)))) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::select(attrs,n) %>% 
    tidyr::unnest(attrs) %>% 
    dplyr::mutate(contains_numeric = attrs %>% str_detect('\\d')) %>% 
    dplyr::mutate(pure_numeric = !(attrs %>% as.numeric() %>% is.na())) %>% 
    dplyr::mutate(attribute = ifelse(pure_numeric, 'id', ifelse(contains_numeric, 'event', 'category'))) %>% 
    dplyr::select(attrs, attribute) %>% 
    dplyr::mutate(attribute = ifelse(stringr::str_count(attrs, "\\S+") > 1, 'event', attribute))
  # dplyr::distinct() %>% 
  # reshape2::dcast(formula = attrs ~ attribute, value.var = 'attrs')
  
  ids <- attr_df %>% dplyr::filter(attribute == 'id') %>% .$attrs
  events <- attr_df %>% dplyr::filter(attribute == 'event') %>% .$attrs
  categories <- attr_df %>% dplyr::filter(attribute == 'category') %>% .$attrs
  
  
  season_dates <- tibble(ids, events, categories) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::filter(categories %>% str_detect('Season')) %>% 
    dplyr::inner_join(dates_df) %>% 
    dplyr::mutate(events = events %>% str_replace(' Season', '')) %>% 
    dplyr::mutate(code_name = events %>% str_replace_all(' ', '')) %>% 
    dplyr::filter(!(code_name %>% str_detect('Mia House'))) %>% 
    dplyr::filter(!(code_name %>% str_detect('Mia house'))) %>% 
    dplyr::filter(!(code_name %>% str_detect('MiaHouse'))) %>% 
    dplyr::filter(!(code_name %>% str_detect('Miahouse'))) %>% 
    # dplyr::mutate(code_name = paste0('Event', code_name)) %>% 
    dplyr::select(time_id = ids, code_name, start_date, end_date) %>% 
    data.table::data.table()
  
  
  return(season_dates)
}

#' @export
get_bingo_dates <- function(game_folder){
  generated_folder <- hs.balancedata::find_source_folder(game_folder = game_folder) %>% stringr::str_replace("data/source", "data/generated")
  x_file <- generated_folder %>% paste0("/xml/times_prod.xml")
  x <- xml2::read_xml(x_file)
  
  # Dates
  dates_df <- x %>% 
    xml2::xml_find_all('//specificTime') %>%
    purrr::map_df(~list(attrs = list(xml2::xml_attrs(.x)), 
                 variable = list(map(xml2::xml_children(.x), xml2::xml_name)), 
                 value = list(map(xml2::xml_children(.x), xml2::xml_text)))) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::select(attrs,n) %>% 
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
    xml2::xml_find_all('//times') %>%
    purrr::map_df(~list(attrs = list(xml2::xml_attrs(.x)), 
                 variable = list(map(xml2::xml_children(.x), xml2::xml_name)), 
                 value = list(map(xml2::xml_children(.x), xml2::xml_text)))) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::select(attrs,n) %>% 
    tidyr::unnest(attrs) %>% 
    dplyr::mutate(contains_numeric = attrs %>% stringr::str_detect('\\d')) %>% 
    dplyr::mutate(pure_numeric = !(attrs %>% as.numeric() %>% is.na())) %>% 
    dplyr::mutate(attribute = ifelse(pure_numeric, 'id', ifelse(contains_numeric, 'event', 'category'))) %>% 
    dplyr::select(attrs, attribute) %>% 
    dplyr::mutate(attribute = ifelse(stringr::str_count(attrs, "\\S+") > 1, 'event', attribute))
  # dplyr::distinct() %>% 
  # reshape2::dcast(formula = attrs ~ attribute, value.var = 'attrs')
  
  ids <- attr_df %>% dplyr::filter(attribute == 'id') %>% .$attrs
  events <- attr_df %>% dplyr::filter(attribute == 'event') %>% .$attrs
  categories <- attr_df %>% dplyr::filter(attribute == 'category') %>% .$attrs
  
  
  bingo_dates <- tibble(ids, events, categories) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::filter(categories %>% stringr::str_detect('Bingo')) %>% 
    dplyr::inner_join(dates_df) %>% 
    dplyr::mutate(events = events %>% stringr::str_replace(' Bingo', '')) %>% 
    dplyr::mutate(code_name = events %>% stringr::str_replace_all(' ', '')) %>% 
    dplyr::filter(!(code_name %>% stringr::str_detect('Mia House'))) %>% 
    dplyr::filter(!(code_name %>% stringr::str_detect('Mia house'))) %>% 
    dplyr::filter(!(code_name %>% stringr::str_detect('MiaHouse'))) %>% 
    dplyr::filter(!(code_name %>% stringr::str_detect('Miahouse'))) %>% 
    # dplyr::mutate(code_name = paste0('Event', code_name)) %>% 
    dplyr::select(time_id = ids, code_name, start_date, end_date) %>% 
    dplyr::mutate(code_name = code_name %>% stringr::str_remove('Event') %>% stringr::str_trim()) %>% 
    data.table::data.table()
  
  
  return(bingo_dates)
}

#' @export
get_quest_dates <- function(game_folder){
  generated_folder <- hs.balancedata::find_source_folder(game_folder = game_folder) %>% stringr::str_replace("data/source", "data/generated")
  x_file <- generated_folder %>% paste0("/xml/times_prod.xml")
  x <- xml2::read_xml(x_file)
  
  # Dates
  dates_df <- x %>% 
    xml2::xml_find_all('//specificTime') %>%
    purrr::map_df(~list(attrs = list(xml2::xml_attrs(.x)), 
                 variable = list(map(xml2::xml_children(.x), xml2::xml_name)), 
                 value = list(map(xml2::xml_children(.x), xml2::xml_text)))) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::select(attrs,n) %>% 
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
    xml2::xml_find_all('//times') %>%
    purrr::map_df(~list(attrs = list(xml2::xml_attrs(.x)), 
                 variable = list(map(xml2::xml_children(.x), xml2::xml_name)), 
                 value = list(map(xml2::xml_children(.x), xml2::xml_text)))) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::select(attrs,n) %>% 
    tidyr::unnest(attrs) %>% 
    dplyr::mutate(contains_numeric = attrs %>% stringr::str_detect('\\d')) %>% 
    dplyr::mutate(pure_numeric = !(attrs %>% as.numeric() %>% is.na())) %>% 
    dplyr::mutate(attribute = ifelse(pure_numeric, 'id', ifelse(contains_numeric, 'event', 'category'))) %>% 
    dplyr::select(attrs, attribute) %>% 
    dplyr::mutate(attribute = ifelse(stringr::str_count(attrs, "\\S+") > 1, 'event', attribute))
  
  ids <- attr_df %>% dplyr::filter(attribute == 'id') %>% .$attrs
  events <- attr_df %>% dplyr::filter(attribute == 'event') %>% .$attrs
  categories <- attr_df %>% dplyr::filter(attribute == 'category') %>% .$attrs
  
  
  quest_dates <- tibble(ids, events, categories) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::filter(categories %>% tolower() %>%  stringr::str_detect('quest')) %>% 
    dplyr::inner_join(dates_df) %>% 
    dplyr::mutate(events = events %>% stringr::str_replace(' Quest', '')) %>% 
    dplyr::mutate(code_name = events %>% stringr::str_replace_all(' ', '')) %>% 
    dplyr::filter(!(code_name %>% stringr::str_detect('Mia House'))) %>% 
    dplyr::filter(!(code_name %>% stringr::str_detect('Mia house'))) %>% 
    dplyr::filter(!(code_name %>% stringr::str_detect('MiaHouse'))) %>% 
    dplyr::filter(!(code_name %>% stringr::str_detect('Miahouse'))) %>% 
    # dplyr::mutate(code_name = paste0('Event', code_name)) %>% 
    dplyr::select(time_id = ids, code_name, start_date, end_date) %>% 
    dplyr::mutate(code_name = code_name %>% stringr::str_remove('Event') %>% stringr::str_trim()) %>% 
    data.table::data.table()
  
  
  return(quest_dates)
}

#' @export
get_bundle_dates <- function(game_folder){
  '(HS) timediaps' %>% 
    googlesheets::gs_title() %>% 
    googlesheets::gs_read(ws = 'timediaps_prod') %>% 
    data.table::data.table() %>% 
    .[, c('Header (string identifier)', 'Reference Name', 'StartTime', 'EndTime')] %>% 
    .[!(`Reference Name` %>% stringr::str_detect('Tokens'))] %>% 
    .[!(`Reference Name` %>% stringr::str_detect('Currency'))] %>% 
    dplyr::rename(
      code_name = `Header (string identifier)`,
      bundle_name = `Reference Name`,
      start_time = StartTime,
      end_time = EndTime
    ) %>% 
    data.table::data.table() %>% 
    dplyr::inner_join(
      '(HS) timediaps' %>% 
        googlesheets::gs_title() %>% 
        googlesheets::gs_read(ws = 'HOME STREET IN-GAME IAPs') %>% 
        .[-(1:3),] %>% 
        data.table::data.table() %>% 
        .[, c('X4', 'X5')] %>% 
        dplyr::rename(
          item_name = X4,
          code_name = X5
        ) %>% 
        data.table::data.table()
    ) %>% 
    dplyr::mutate(item_name = item_name %>% stringr::str_remove(' Offer')) %>% 
    dplyr::mutate(bundle_name = paste0(bundle_name, ' (', item_name, ')')) %>% 
    dplyr::mutate(start_date = as.Date(start_time)) %>% 
    dplyr::mutate(end_date = as.Date(end_time)) %>% 
    data.table::data.table() %>% 
    return()
}
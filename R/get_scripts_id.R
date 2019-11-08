#' @export
get_scripts_id <- function(game_folder){
  source_folder <- hs.balancedata::find_source_folder(game_folder = game_folder)
  script_file <- paste0(source_folder, '/xml/scripts.xml')
  x <- xml2::read_xml(script_file) 
  
  ids_df <- x %>% 
    xml2::xml_find_all('//scripts ') %>%
    purrr::map_df(~list(attrs = list(xml2::xml_attrs(.x)), 
                 variable = list(map(xml2::xml_children(.x), xml2::xml_name)), 
                 value = list(map(xml2::xml_children(.x), xml2::xml_text)))) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::select(attrs,n) %>% 
    tidyr::unnest(attrs) %>% 
    dplyr::filter(!is.na(as.numeric(attrs)))
  
  script_ids <- x %>% 
    xml2::xml_find_all('//scripts ') %>%
    map_df(~list(attrs = list(xml2::xml_attrs(.x)), 
                 variable = list(map(xml2::xml_children(.x), xml2::xml_name)), 
                 value = list(map(xml2::xml_children(.x), xml2::xml_text)))) %>% 
    dplyr::mutate(n = 1:n()) %>% 
    dplyr::select(attrs,n) %>% 
    tidyr::unnest(attrs) %>% 
    dplyr::filter(n %in% ids_df$n) %>% 
    dplyr::filter(is.na(as.numeric(attrs))) %>% 
    dplyr::filter(attrs != 'true') %>% 
    dplyr::rename(script_name = attrs) %>% 
    cbind(ids_df %>% dplyr::select(-n)) %>% 
    dplyr::rename(id = attrs) %>% 
    dplyr::select(-n) %>% 
    dplyr::group_by(script_name) %>% 
    dplyr::mutate(gen_name = script_name %>% stringr::str_split(' - ') %>% .[[1]] %>% .[[1]] %>% .[[1]]) %>% 
    data.table::data.table()
  return(script_ids)
}

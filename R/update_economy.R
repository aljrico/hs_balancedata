#' @export
update_economy <- function(document, game_folder){
  
  economy_path <- hs.balancedata::find_economy_document_folder(game_folder = game_folder) %>% .[[1]]
  source_folder <- hs.balancedata::find_source_folder(game_folder = game_folder) %>% .[[1]]
  
  spark_economy_file <- paste0(economy_path, '/', document)
  
  event_csv_list <- c(
    'ingredients_prod.csv',
    'ingredientsbingoevent.csv',
    'products_prod.csv',
    'thoughts_prod.csv',
    'eventworkstations.csv',
    'workstationproducts.csv',
    'guildeventorders.csv',
    'guildeventpredefinedorders.csv',
    'guildevents_prod.csv',
    'bingoevents_prod.csv',
    'bingoeventmysteryboxes.csv',
    'guildeventmysteryboxes.csv'
  )
  convert_na <- function(x) ifelse(x == 'NA', NA, x) %>% return()
  
  # Pulling Data ------------------------------------------------------------
  
  for(c in seq_along(event_csv_list)){
    sheet_name <- event_csv_list[[c]]
    
    if(sheet_name == 'ingredients_prod.csv'){
      
      this_sheet <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 4) %>% 
        dplyr::mutate_if(is.numeric, as.integer)
      
      max_col <- which(colnames(this_sheet) == 'skipCash Ratio') - 1
      
      to_write <- this_sheet[, 1:max_col]
      colnames(to_write) <- to_write %>% colnames() %>% stringr::str_remove_all("\\.[^.]*$") %>% stringr::str_remove_all('\\.') %>% stringr::str_trim()
      to_write <- to_write[ , -which(names(to_write) %in% c(""))]
    }
    if(sheet_name == 'ingredientsbingoevent.csv'){
      this_sheet <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 2) %>% 
        dplyr::filter(!is.na(`# ingredient id`))
      
      max_col <- which(colnames(this_sheet) == 'PM') - 3
      
      to_write <- this_sheet[, 1:max_col]
      colnames(to_write) <- to_write %>% colnames() %>% stringr::str_remove_all("\\.[^.]*$") %>% stringr::str_remove_all('\\.') %>% stringr::str_trim()
    }
    if(sheet_name == 'products_prod.csv'){
      to_write <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 1) %>% 
        dplyr::mutate_if(is.numeric, format, scientific = FALSE) %>% 
        dplyr::mutate_if(is.character, stringr::str_trim) %>% 
        dplyr::mutate_if(is.character, convert_na)
      
      colnames(to_write) <- to_write %>% colnames() %>% stringr::str_remove_all("\\.[^.]*$") %>% stringr::str_remove_all('\\.')
      
    }
    if(sheet_name == 'thoughts_prod.csv'){
      to_write <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 2) %>% 
        dplyr::mutate_if(is.numeric, format, scientific = FALSE) %>% 
        dplyr::mutate_if(is.character, stringr::str_trim) %>% 
        dplyr::mutate_if(is.character, convert_na)
      
      colnames(to_write) <- to_write %>% colnames() %>% stringr::str_remove_all("\\.[^.]*$") %>% stringr::str_remove_all('\\.')
    }
    if(sheet_name == 'eventworkstations.csv'){
      to_write <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 2)
      colnames(to_write) <- to_write %>% colnames() %>% stringr::str_remove_all("\\.[^.]*$") %>% stringr::str_remove_all('\\.')
    }
    if(sheet_name == 'workstationproducts.csv'){
      this_sheet <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 2) %>% 
        dplyr::mutate_if(is.numeric, format, scientific = FALSE) %>% 
        dplyr::filter(`#buildingitemid` != 'x') 
      
      max_col <- 11
      
      to_write <- this_sheet[, 1:max_col]
    }
    if(sheet_name == 'guildeventorders.csv'){
      this_sheet <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 2) %>% 
        dplyr::mutate_if(is.numeric, format, scientific = FALSE)
      
      max_col <- which(colnames(this_sheet) == 'Unique Item Count Weight 5-4') 
      
      to_write <- this_sheet[, 1:max_col]
      colnames(to_write) <- to_write %>% colnames() %>% stringr::str_remove_all("\\.[^.]*$") %>% stringr::str_remove_all('\\.')
    }
    if(sheet_name == 'guildeventpredefinedorders.csv'){
      this_sheet <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 2) %>% 
        dplyr::mutate_if(is.numeric, as.integer)
      
      max_col <- which(colnames(this_sheet) == 'ingredient count 4')
      
      to_write <- this_sheet[, 1:max_col]
      colnames(to_write) <- to_write %>% colnames() %>% stringr::str_remove_all("\\.[^.]*$") %>% stringr::str_remove_all('\\.')
    }
    if(sheet_name == 'guildevents_prod.csv'){
      to_write <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 2) %>% 
        dplyr::mutate_if(is.numeric, as.integer)
      # colnames(to_write) <- to_write %>% colnames() %>% stringr::str_remove_all("\\.[^.]*$") %>% stringr::str_remove_all('\\.')
    }
    if(sheet_name == 'bingoevents_prod.csv'){
      to_write <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 3)
      colnames(to_write) <- to_write %>% colnames() %>% stringr::str_remove_all("\\.[^.]*$") %>% stringr::str_remove_all('\\.')
    }
    if(sheet_name == 'bingoeventmysteryboxes.csv'){
      to_write <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 2) %>% 
        dplyr::mutate_if(is.numeric, format, scientific = FALSE) %>% 
        dplyr::mutate_if(is.character, stringr::str_trim) %>% 
        dplyr::mutate_if(is.character, convert_na)
      
      
      
      
      colnames(to_write) <- to_write %>% colnames() %>% stringr::str_remove_all("\\.[^.]*$") %>% stringr::str_remove_all('\\.')
    }
    if(sheet_name == 'guildeventmysteryboxes.csv'){
      to_write <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 2) %>% 
        dplyr::mutate_if(is.numeric, format, scientific = FALSE) %>% 
        dplyr::mutate_if(is.character, stringr::str_trim) %>% 
        dplyr::mutate_if(is.character, convert_na)
    }
    if(sheet_name == 'prizetent_prod.csv'){
      to_write <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 2) %>% 
        .[, 1:13] %>% 
        .[rowSums(is.na(.)) != ncol(.), ] %>% 
        dplyr::mutate_if(is.numeric, format, scientific = FALSE) %>% 
        dplyr::mutate_if(is.character, stringr::str_trim) %>% 
        dplyr::mutate_if(is.character, convert_na)
    }
    if(sheet_name == 'prizetentmysteryboxes.csv'){
      to_write <- spark_economy_file %>% 
        readxl::read_excel(sheet = sheet_name, skip = 2) %>% 
        .[rowSums(is.na(.)) != ncol(.), ] %>% 
        dplyr::mutate_if(is.numeric, format, scientific = FALSE) %>% 
        dplyr::mutate_if(is.character, stringr::str_trim) %>% 
        dplyr::mutate_if(is.character, convert_na)
    }
    
    cat(paste0('... ', sheet_name, ' ... \n'))
    
    to_write %>% 
      data.table::fwrite(
        paste0(source_folder, '/csv/', sheet_name),
        na = ''
      )
  }
  
}



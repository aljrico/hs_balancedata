#' @export
csv_source_path <- function(windows = FALSE, game_folder, user_name = NA){
  if(windows){
    path <- paste0('~/', game_folder, '/Assets/data/source/csv/')
  }else{
    path <- paste0("C:\\Users\\", user_name, "\\", game_folder, "Assets\\data\\source\\csv\\")
  }
}


# FIND USERNAME
find_windows_path <- function(game_folder){
  user_folders <- list.files('C:/Users/', full.names = TRUE)
  for(fs in user_folders){
    full_path <- paste0(fs, '/', game_folder, '/Assets/data/source')
    if(file.exists(full_path)){
      user <- fs %>% str_remove('C:/Users/')
      return(user)
    }
  }
  print('No folder found')
  return(NA)
}

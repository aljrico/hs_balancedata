#' @export
economy_document_path <- function(windows = FALSE, game_folder, user_name = NA){
  if(!windows){
    path <- paste0('~/', game_folder, '/pm/')
  }else{
    path <- paste0("C:/Users/", user_name, "/", game_folder, "pm/")
  }
  return(path)
}
#' @export
get_os <- function() {
  sysinf <- Sys.info()
  if (!is.null(sysinf)) {
    os <- sysinf["sysname"]
    if (os == "Darwin") {
      os <- "osx"
    }
  } else { ## mystery machine
    os <- .Platform$OS.type
    if (grepl("^darwin", R.version$os)) {
      os <- "osx"
    } else if (grepl("linux-gnu", R.version$os)) {
      os <- "linux"
    } else {
      os <- "windows"
    }
  }
  tolower(os)
}

#' @export
find_source_folder <- function(game_folder = NA) {
  if (is.na(game_folder)) {
    check_condition <- function(x) {
      x %>%
        stringr::str_detect("Assets/data/source") %>%
        return()
    }
  } else {
    check_condition <- function(x) {
      x %>%
        stringr::str_detect(paste0(game_folder, "/Assets/data/source")) %>%
        return()
    }
  }

  if (get_os() == "windows") {
    origin_folders <- list.files('C:/Users', full.names = TRUE)
  } else {
    origin_folders <- list.files("~", full.names = TRUE)
  }

  counter_finding_game_folder <- 0
  while (sum(check_condition(origin_folders)) == 0) {
    origin_folders <- list.files(origin_folders, full.names = TRUE)
    origin_folders <- origin_folders[!(origin_folders %>% stringr::str_detect('.meta'))]
    
    counter_finding_game_folder <- counter_finding_game_folder + 1
    if(counter_finding_game_folder > 1e4) stop('SHINY APP ERROR: No game was found in the system. Please make sure you have the Git repositories prorperly cloned in your machine.')
  }

  source_folder <- origin_folders[origin_folders %>% check_condition()]
  return(source_folder)
}

#' @export
find_game_folder_names <- function() {
  a <- find_source_folder() %>%
    stringr::str_split("\\/")

  game_folder <- c()

  for (i in 1:length(a)) {
    gf_position <- which(a[[i]] == "Assets") - 1
    game_folder[[i]] <- a[[i]][[gf_position]]
  }
  return(game_folder)
}

#' @export
find_economy_document_folder <- function(game_folder = NA){
  source_folder <- hs.balancedata::find_source_folder(game_folder = game_folder)
  economy_folder <- source_folder %>% stringr::str_replace('/Assets/data/source', '/pm')
}

#' @export
find_economy_documents <- function(game_folder = NA){
  economy_folder <- hs.balancedata::find_economy_document_folder(game_folder = game_folder)
  all_xlsx <- economy_folder %>% list.files(pattern = '*.xlsx') 
  saved_xlsx <- all_xlsx[!all_xlsx %>% str_detect('\\$')] # Removes files containing '$' in their name. As they're usually temporary unsaved files.
  return(saved_xlsx)
}

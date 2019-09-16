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
        str_detect("Assets/data/source") %>%
        return()
    }
  } else {
    check_condition <- function(x) {
      x %>%
        str_detect(paste0(game_folder, "/Assets/data/source")) %>%
        return()
    }
  }

  if (get_os() == "windows") {
    origin_folders <- list.files("C:/Users", full.names = TRUE)
  } else {
    origin_folders <- list.files("~", full.names = TRUE)
  }

  while (sum(check_condition(origin_folders)) == 0) {
    origin_folders <- list.files(origin_folders, full.names = TRUE)
    origin_folders <- origin_folders[!(origin_folders %>% str_detect("\.meta"))]
  }

  source_folder <- origin_folders[origin_folders %>% check_condition()]
  return(source_folder)
}

#' @export
find_game_folder_names <- function() {
  a <- find_source_folder() %>%
    str_split("\/")

  game_folder <- c()

  for (i in 1:length(a)) {
    gf_position <- which(a[[i]] == "Assets") - 1
    game_folder[[i]] <- a[[i]][[gf_position]]
  }
  return(game_folder)
}

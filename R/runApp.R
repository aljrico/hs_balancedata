#' @export
runApp <- function(display.mode = "normal", launch.browser = TRUE, ...) {
  appDir <- system.file('app', package = "hs.balancedata")
  if (appDir == "") {
    stop("Could not find example directory. Try re-installing `mypackage`.", call. = FALSE)
  }
  
  shiny::runApp(appDir, display.mode = display.mode, launch.browser = launch.browser)
}
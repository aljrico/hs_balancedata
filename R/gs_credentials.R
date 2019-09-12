#' @export
gs_credentials <- function(){
  googlesheets::gs_auth(token = paste0(system.file('app', package = "hs.balancedata"), '/misc/aljrico_google_credentials.rds'))
}
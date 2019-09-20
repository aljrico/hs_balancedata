check_spreadsheet_existence <- function(sh_name){
  hs.balancedata::gs_credentials()
  exists <- TRUE
  trial <- try(
    sh_name %>% 
      googlesheets::gs_title() 
    , silent = TRUE
  )
  
  if(class(trial) == 'try-error') exists <- FALSE
  
  return(exists)
  
}
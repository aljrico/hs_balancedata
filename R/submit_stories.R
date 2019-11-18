#' @export
submit_stories <- function(spreadsheet_title, seasonalquest_prod, original_length, source_folder, file_version){
  if(nrow(seasonalquest_prod[-(1:original_length),]) < 1){
    stop('No new Quests to add')
  }else{
    spreadsheet_title %>% 
      gs_title() %>% 
      gs_add_row(ws = file_version, 
                 input = seasonalquest_prod[-(1:original_length),]
      )
    
    spreadsheet_title %>%
      gs_title() %>%
      gs_read(ws = file_version) %>%
      fwrite(paste0(source_folder, '/csv/seasonalquests_prod.csv'))
  }
  
}
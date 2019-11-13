#' @export
update_quest_multichapter <- function(new_quest, quest_id, this_design_table){
  if(this_design_table$Chapter > 1 & !is.na(this_design_table$Chapter)){
    new_quest[, `completed quest id` := as.numeric(quest_id) - 1]
  }else{
    new_quest[, `completed quest id` := NA]
  }
  return(new_quest)
}
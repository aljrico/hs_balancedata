#' @export
update_quest_assets_ids <- function(new_quest, seasonalquest_prod, quest_id, this_design_table) {
  
  new_quest[, `# id` := quest_id, ]
  new_quest[, title := paste0("QuestTextTitle", "_", this_design_table$`Event Name`, "_", quest_id)]
  new_quest[, description := paste0("QuestTextDescription", "_", this_design_table$`Event Name`, "_", quest_id)]
  new_quest[, `completed text` := paste0("QuestTextCompleted", "_", this_design_table$`Event Name`, "_", quest_id)]
  new_quest[, `expired text` := paste0("QuestTextExpired", "_", this_design_table$`Event Name`, "_", quest_id)]
  new_quest[, `icon prefab` := NA] # No Icon
  new_quest[, `level` := this_design_table$`Level Unlock`]
  new_quest[, `actor id` := this_design_table$`Actor ID`]
  new_quest[, `complete actor id` := this_design_table$`Actor ID`]
  new_quest[, `expire actor id` := this_design_table$`Actor ID`]
  new_quest[, `actor animation` := this_design_table$`Actor Animation`]
  
  return(new_quest)
}
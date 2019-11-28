#' @export
update_quest_rewards <- function(new_quest, seasonalquest_prod, quest_id, this_design_table) {
  extract_rewards <- function(this_design_table) {
    quantities <- c()
    quantities_raw <- this_design_table$Reward %>%
      stringr::str_split("&") %>%
      unlist() %>%
      stringr::str_extract_all("[0-9]")
    rewards <- this_design_table$Reward %>%
      stringr::str_split("&") %>%
      unlist() %>%
      stringr::str_remove_all("[0-9]") %>%
      stringr::str_trim()
    for (qt in seq_along(quantities_raw)) {
      quantities[[qt]] <- quantities_raw[qt][[1]] %>%
        paste(collapse = "") %>%
        as.numeric()
    }
    return(list(quantities = quantities, rewards = rewards))
  }
  
  items_prices <- hs.balancedata::get_season_items()

  # Resetting all values
  new_quest[, `cash reward` := NA]
  new_quest[, `coin reward` := NA]
  new_quest[, `xp reward` := NA]
  new_quest[, `item reward id` := NA]
  new_quest[, `petColorId` := NA]
  new_quest[, `item reward count` := NA]
  new_quest[, `item reward id 2` := NA]
  new_quest[, `petColorId 2` := NA]
  new_quest[, `item reward count 2` := NA]


  quantities <- this_design_table %>%
    extract_rewards() %>%
    .[["quantities"]]
  rewards <- this_design_table %>%
    extract_rewards() %>%
    .[["rewards"]]

  number_rewards <- length(rewards)
  voucher_list <- c("Bronze", "Silver", "Gold", "Rainbow")
  voucher_ids <- c(145004, 145003, 145002, 145001)
  for (i in 1:number_rewards) {
    if (rewards[[i]] %>% str_detect("Gem")) {
      new_quest[, `cash reward` := quantities[[i]]]
    } else if (rewards[[i]] %>% str_detect("Coin")) {
      new_quest[, `coin reward` := quantities[[i]]]
    } else if (any(rewards[[i]] %>% str_detect(voucher_list))) {
      voucher_reward <- voucher_ids[which(rewards[[i]] %>% str_detect(voucher_list))]
      if (is.na(new_quest$`item reward id`)) {
        new_quest[, `item reward id` := voucher_reward]
        new_quest[, `item reward count` := quantities[[i]]]
      } else {
        new_quest[, `item reward id 2` := voucher_reward]
        new_quest[, `item reward count 2` := quantities[[i]]]
      }
    } else if (any(items_prices$Item %>% str_detect(rewards[[i]]))) {
      item_reward <- items_prices %>%
        filter(Item %>% str_detect(rewards[[i]])) %>%
        .$Id %>%
        as.numeric() %>%
        max()
      if (is.na(new_quest$`item reward id`)) {
        new_quest[, `item reward id` := item_reward]
        new_quest[, `item reward count` := 1]
      } else {
        new_quest[, `item reward id 2` := item_reward]
        new_quest[, `item reward count 2` := 1]
      }
    }
  }
  return(new_quest)
}

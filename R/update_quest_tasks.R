#' @export
update_quest_tasks <- function(new_quest, seasonalquest_prod, quest_id, this_design_table, task_types, spark_economy_file, season_items) {
  specific_columns <- c(
    "product id",
    "workstation id",
    "thought id",
    "ConsumeIngredient ingredient id",
    "QuestProgress createRewardIcon",
    "OwnItem item ids (separator '|')",
    "LevelUpRelationship status level",
    "PurchaseShopItem category",
    "PurchaseShopItem subcategory",
    "PurchaseShopItem useCoins",
    "AvatarInteraction interactionId",
    "AvatarInteraction cost itemId",
    "AvatarInteraction cost count",
    "PlayerInteraction uniqueInteraction",
    "NpcInteraction npcId",
    "ViewBillboardVideo videoCampaignId",
    "ViewBillboardVideo videoId"
  )

  master_clean <- function(spark_economy_file) {
    df <- spark_economy_file %>%
      readxl::read_excel(sheet = "MASTER", skip = 4) %>%
      dplyr::select(-`...1`, -`...2`)

    events <- df %>%
      dplyr::filter(`Item Name` %>% stringr::str_detect("Event") | `Workbench Name` %>% stringr::str_detect("Bingo")) %>%
      .[["Workbench Name"]]

    events_pos <- which(df$`Workbench Name` %in% events)
    df$event <- "permanent"
    for (i in seq_along(events_pos)) {
      df[events_pos[[i]]:(df$event %>% length()), "event"] <- events[[i]] %>% stringr::str_remove_all(" ")
    }
    return(df)
  }
  specific_tasks <- function(new_quest, this_design_table, spark_economy_file, this_task, season_items) {
    master_file <- master_clean(spark_economy_file = spark_economy_file)

    if (this_task == "Own an item") {
      new_quest[, paste0("task", t, " skip cash") := this_design_table[[paste0("Task Skip Cost ", t)]]]
      this_specific <- this_design_table[[paste0("Task Specifics ", t)]] %>%
        stringr::str_split("\\|") %>%
        unlist() %>%
        stringr::str_trim()

      if (is.na(this_specific)) {
        this_specific <- this_design_table[[paste0("Task Prod ", t)]] %>%
          stringr::str_split("\\|") %>%
          unlist() %>%
          stringr::str_trim()
      }

      item_id <-
        season_items %>%
        dplyr::filter(Item %in% this_specific) %>%
        .$Id %>%
        as.numeric() %>%
        as.character() %>%
        unique()

      if (length(item_id) > 1) {
        to_fill <- paste(item_id, collapse = " | ")
      } else {
        to_fill <- item_id
      }

      new_quest[, paste0("task", t, " OwnItem item ids (separator '|')") := to_fill]

      new_quest[, paste0("task", t, " icon prefab") := paste0(
        "UI/icons/Quest/quest_task_icon_",
        this_design_table[[paste0("Task Specifics ", t)]] %>%
          tolower() %>%
          stringr::str_remove_all(" ") %>%
          stringr::str_remove_all("\\'") %>%
          stringr::str_trim()
      )]
    } else if (this_task == "Collect a specific product") {
      product <- this_design_table[[paste0("Task Prod ", t)]]
      event_name <- this_design_table[["Event Name"]]

      if (is.na(product)) product <- this_design_table[[paste0("Task Specifics ", t)]]

      prod_id <-
        master_file %>%
        dplyr::filter(event == (event_name %>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("\\_.*"))) %>%
        dplyr::mutate(clean_name = `Item Name` %>% gsub(".*[0-9]", "", .) %>% stringr::str_trim()) %>%
        dplyr::filter(product %>% stringr::str_detect(clean_name)) %>%
        .[["Product"]] %>%
        .[[length(.)]] %>%
        as.character()

      this_event_name <-
        this_design_table[["Event Name"]] %>%
        tolower() %>%
        stringr::str_remove_all("\\_.*")

      new_quest[, paste0("task", t, " product id") := prod_id]
      new_quest[, paste0("task", t, " icon prefab") := paste0(
        "UI/icons/Quest/quest_task_icon_",
        this_design_table[[paste0("Task Specifics ", t)]] %>%
          tolower() %>%
          stringr::str_remove_all(" ") %>%
          stringr::str_remove_all("\\'s") %>%
          stringr::str_remove_all("\\'") %>%
          stringr::str_remove_all(this_event_name) %>%
          stringr::str_trim()
      )]
      new_quest[, `icon prefab` := paste0("UI/icons/Quest/quest_icon_", this_design_table$`Event Name` %>% tolower())]

      # Exception for Safari 2019 Explorer's Hat
      if (new_quest[[paste0("task", t, " icon prefab")]] == "UI/icons/Quest/quest_task_icon_explorershat") new_quest[, paste0("task", t, " icon prefab") := "UI/icons/Quest/quest_task_icon_explorerhat"]

      # Exception for Safari 2019 Explorer's Camera
      if (new_quest[[paste0("task", t, " icon prefab")]] == "UI/icons/Quest/quest_task_icon_explorerscamera") new_quest[, paste0("task", t, " icon prefab") := "UI/icons/Quest/quest_task_icon_explorercamera"]

      # Exception for English 2019 Afternoon Tea
      if (new_quest[[paste0("task", t, " icon prefab")]] == "UI/icons/Quest/quest_task_icon_afternoontea") new_quest[, paste0("task", t, " icon prefab") := "UI/icons/Quest/quest_task_icon_tea"]
    } else if (this_task == "Buy X decoration items with a limited time") {
      new_quest[, paste0("task", t, " PurchaseShopItem isTimeLimited") := TRUE]
      this_specific <- this_design_table[[paste0("Task Specifics ", t)]]
      if (this_design_table[[paste0("Task Prod ", t)]] %>% tolower() %>% stringr::str_detect("coin")) {
        new_quest[, paste0("task", t, " PurchaseShopItem useCoins") := TRUE]
      }
    } else if (this_task == "Complete X amount of event jobs") {
      new_quest[, paste0("task", t, " icon prefab") := paste0(
        "UI/icons/Quest/quest_task_icon_eventorder_",
        this_design_table[["Event Name"]] %>%
          stringr::str_trim() %>%
          stringr::str_remove_all("2019") %>%
          stringr::str_remove_all("2018") %>%
          stringr::str_remove_all("2020") %>%
          stringr::str_remove_all("2021") %>%
          stringr::str_remove_all("Bingo") %>%
          stringr::str_remove_all("\\_1$") %>%
          stringr::str_remove_all("\\_2$") %>%
          # stringr::str_remove_all('_[^_]+$') %>%
          stringr::str_trim() %>%
          tolower()
      )]
    } else if (this_task == "Collect a specific thought") {
      product <- this_design_table[[paste0("Task Prod ", t)]]
      event_name <- this_design_table[["Event Name"]]

      if (is.na(product)) product <- this_design_table[[paste0("Task Specifics ", t)]]

      ing_id <- master_file %>%
        dplyr::filter(event == (event_name %>% stringr::str_remove_all(" ") %>% stringr::str_remove_all("\\_.*"))) %>%
        dplyr::mutate(clean_name = `Item Name` %>% gsub(".*[0-9]", "", .) %>% stringr::str_trim()) %>% 
        dplyr::filter(!is.na(clean_name)) %>%
        dplyr::filter(product != '') %>% 
        dplyr::filter(product %>% stringr::str_detect(clean_name)) %>%
        .[["Thought"]] %>%
        .[[length(.)]] %>%
        as.character()

      this_event_name <-
        this_design_table[["Event Name"]] %>%
        tolower() %>%
        stringr::str_remove_all("\\_.*")

      new_quest[, paste0("task", t, " thought id") := ing_id]
      new_quest[, paste0("task", t, " icon prefab") := paste0(
        "UI/icons/Quest/quest_task_icon_",
        this_design_table[[paste0("Task Specifics ", t)]] %>%
          tolower() %>%
          stringr::str_remove_all("\\'s") %>%
          stringr::str_remove_all(" ") %>%
          stringr::str_remove_all(this_event_name) %>%
          stringr::str_trim()
      )]
      new_quest[, `icon prefab` := paste0("UI/icons/Quest/quest_icon_", this_design_table$`Event Name` %>% tolower())]
    } else if (this_task == "Upgrade any Event Product using Tokens") {
      new_quest[, paste0("task", t, " icon prefab") := paste0(
        "UI/icons/Quest/quest_task_icon_eventorder_",
        this_design_table[["Event Name"]] %>%
          stringr::str_trim() %>%
          stringr::str_remove_all("2019") %>%
          stringr::str_remove_all("2018") %>%
          stringr::str_remove_all("2020") %>%
          stringr::str_remove_all("2021") %>%
          stringr::str_remove_all("Bingo") %>%
          stringr::str_remove_all("\\_1$") %>%
          stringr::str_remove_all("\\_2$") %>%
          # stringr::str_remove_all('_[^_]+$') %>%
          stringr::str_trim() %>%
          tolower()
      )]
    }

    return(new_quest)
  }

  maximum_number_tasks <- 3

  for (t in (1:maximum_number_tasks)) {
    this_task <- this_design_table[[paste0("Task Action ", t)]]

    this_task_data <-
      task_types %>%
      dplyr::filter(Category == this_design_table[[paste0("Task Action ", t)]]) %>%
      dplyr::select(-Category)

    colnames(this_task_data) <-
      this_task_data %>%
      colnames() %>%
      stringr::str_remove("task ")

    colnames(this_task_data) <-
      paste0("task", t, " ", colnames(this_task_data))

    cols <- colnames(this_task_data)

    if (is.na(this_design_table[[paste0("Task Action ", t)]])) {
      for (c in seq_along(cols)) {
        new_quest[, cols[[c]] := NA]
      }
    } else if (nrow(this_task_data) < 1) {
      stop("Task Type not defined")
    } else if (nrow(this_task_data) > 1) {
      stop("Duplicated Tasks")
    } else {
      for (c in seq_along(cols)) {
        task_input_value <- this_task_data[cols[[c]]][[1]]
        new_quest[, cols[[c]] := task_input_value]
      }

      # Task Text
      new_quest[, paste0("task", t, " text") := paste0("QuestTextTask", t, "_", this_design_table[["Event Name"]], "_", quest_id)]

      # Task Count
      new_quest[, paste0("task", t, " count") := this_design_table[[paste0("Task Amount ", t)]]]

      # Task Skip Cost
      new_quest[, paste0("task", t, " skip cash") := this_design_table[[paste0("Task Skip Cost ", t)]]]

      cols_with_na <- this_task_data %>%
        as.data.frame() %>%
        .[specific_columns %>% paste0("task", t, " ", .)] %>%
        is.na() %>%
        which()

      cols_to_fill <- specific_columns[-cols_with_na]

      if (length(cols_to_fill) > 1) {
        stop("Undefined Task. Needs to update code.")
      } else {
        new_quest <- specific_tasks(new_quest = new_quest, this_design_table = this_design_table, spark_economy_file = spark_economy_file, this_task = this_task, season_items = season_items)
      }
      if (is.na(new_quest %>% .[[paste0("task", t, " count")]])) new_quest[, paste0("task", t, " count") := 0]
    }
  }
  return(new_quest)
}

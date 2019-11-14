#' @export
update_quest_tasks <- function(new_quest, seasonalquest_prod, quest_id, this_design_table, task_types, economy_file) {
  master_clean <- function(economy_file) {
    df <- economy_file %>%
      read_excel(sheet = "MASTER", skip = 4) %>%
      select(-`...1`, -`...2`)

    events <- df %>%
      filter(`Item Name` %>% str_detect("Event") | `Workbench Name` %>% str_detect("Bingo")) %>%
      .[["Workbench Name"]]

    events_pos <- which(df$`Workbench Name` %in% events)
    df$event <- "permanent"
    for (i in seq_along(events_pos)) {
      df[events_pos[[i]]:(df$event %>% length()), "event"] <- events[[i]] %>% str_remove_all(" ")
    }
    return(df)
  }
  specific_tasks <- function(new_quest, this_design_table, economy_file) {
    master_file <- master_clean(economy_file = economy_file)
    items_season <- hs.balancedata::get_season_items()

    if (this_task == "Own an item") {
      new_quest[, paste0("task", t, " skip cash") := this_design_table[[paste0("Task Skip Cost ", t)]]]
      this_specific <- this_design_table[[paste0("Task Specifics ", t)]] %>%
        str_split("\\|") %>%
        unlist() %>%
        str_trim()

      if (is.na(this_specific)) {
        this_specific <- this_design_table[[paste0("Task Prod ", t)]] %>%
          str_split("\\|") %>%
          unlist() %>%
          str_trim()
      }

      item_id <-
        items_season

      filter(Item %in% this_specific) %>%
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
          str_remove_all(" ") %>%
          str_remove_all("\\'") %>%
          str_trim()
      )]
    } else if (this_task == "Collect a specific product") {
      product <- this_design_table[[paste0("Task Prod ", t)]]
      event_name <- this_design_table[["Event Name"]]

      if (is.na(product)) product <- this_design_table[[paste0("Task Specifics ", t)]]

      prod_id <-
        master_file %>%
        filter(event == (event_name %>% str_remove_all(" ") %>% str_remove_all("\\_.*"))) %>%
        mutate(clean_name = `Item Name` %>% gsub(".*[0-9]", "", .) %>% str_trim()) %>%
        filter(product %>% str_detect(clean_name)) %>%
        .[["Product"]] %>%
        .[[length(.)]] %>%
        as.character()

      this_event_name <-
        this_design_table[["Event Name"]] %>%
        tolower() %>%
        str_remove_all("\\_.*")

      new_quest[, paste0("task", t, " product id") := prod_id]
      new_quest[, paste0("task", t, " icon prefab") := paste0(
        "UI/icons/Quest/quest_task_icon_",
        this_design_table[[paste0("Task Specifics ", t)]] %>%
          tolower() %>%
          str_remove_all(" ") %>%
          str_remove_all("\\'s") %>%
          str_remove_all("\\'") %>%
          str_remove_all(this_event_name) %>%
          str_trim()
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
      if (this_design_table[[paste0("Task Prod ", t)]] %>% tolower() %>% str_detect("coin")) {
        new_quest[, paste0("task", t, " PurchaseShopItem useCoins") := TRUE]
      }
    } else if (this_task == "Complete X amount of event jobs") {
      new_quest[, paste0("task", t, " icon prefab") := paste0(
        "UI/icons/Quest/quest_task_icon_eventorder_",
        this_design_table[["Event Name"]] %>%
          str_trim() %>%
          str_remove_all("2019") %>%
          str_remove_all("2018") %>%
          str_remove_all("2020") %>%
          str_remove_all("2021") %>%
          str_remove_all("Bingo") %>%
          str_remove_all("\\_1$") %>%
          str_remove_all("\\_2$") %>%
          # str_remove_all('_[^_]+$') %>%
          str_trim() %>%
          tolower()
      )]
    } else if (this_task == "Collect a specific thought") {
      product <- this_design_table[[paste0("Task Prod ", t)]]
      event_name <- this_design_table[["Event Name"]]

      if (is.na(product)) product <- this_design_table[[paste0("Task Specifics ", t)]]

      ing_id <- master %>%
        filter(event == (event_name %>% str_remove_all(" ") %>% str_remove_all("\\_.*"))) %>%
        mutate(clean_name = `Item Name` %>% gsub(".*[0-9]", "", .) %>% str_trim()) %>%
        filter(product %>% str_detect(clean_name)) %>%
        .[["Thought"]] %>%
        .[[length(.)]] %>%
        as.character()

      this_event_name <-
        this_design_table[["Event Name"]] %>%
        tolower() %>%
        str_remove_all("\\_.*")

      new_quest[, paste0("task", t, " thought id") := ing_id]
      new_quest[, paste0("task", t, " icon prefab") := paste0(
        "UI/icons/Quest/quest_task_icon_",
        this_design_table[[paste0("Task Specifics ", t)]] %>%
          tolower() %>%
          str_remove_all("\\'s") %>%
          str_remove_all(" ") %>%
          str_remove_all(this_event_name) %>%
          str_trim()
      )]
      new_quest[, `icon prefab` := paste0("UI/icons/Quest/quest_icon_", this_design_table$`Event Name` %>% tolower())]
    } else if (this_task == "Upgrade any Event Product using Tokens") {
      new_quest[, paste0("task", t, " icon prefab") := paste0(
        "UI/icons/Quest/quest_task_icon_eventorder_",
        this_design_table[["Event Name"]] %>%
          str_trim() %>%
          str_remove_all("2019") %>%
          str_remove_all("2018") %>%
          str_remove_all("2020") %>%
          str_remove_all("2021") %>%
          str_remove_all("Bingo") %>%
          str_remove_all("\\_1$") %>%
          str_remove_all("\\_2$") %>%
          # str_remove_all('_[^_]+$') %>%
          str_trim() %>%
          tolower()
      )]
    }

    return(new_quest)
  }

  maximum_number_tasks <- 3

  for (t in maximum_number_tasks) {
    this_task_data <-
      task_types %>%
      filter(Category == this_design_table[[paste0("Task Action ", t)]]) %>%
      select(-Category)

    colnames(this_task_data) <-
      this_task_data %>%
      colnames() %>%
      str_remove("task ")

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
        new_quest <- specific_tasks(new_quest = new_quest, this_design_table = this_design_table, economy_file = economy_file)
      }
      if (is.na(new_quest %>% .[[paste0("task", t, " count")]])) new_quest[, paste0("task", t, " count") := 0]
    }
  }
}

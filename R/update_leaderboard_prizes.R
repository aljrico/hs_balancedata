#' @export
update_leaderboard_prizes <- function(spreadsheet_name = '(HS) leaderboards', game_folder = 'homestreet'){
  
  clean_rewards <- function(df){
    # REMEMBER: THE TENNARY_ID ONLY WORKS IF THE MAXIMUM AMOUNT OF ANY PRIZE IS 100, IF IT HAPPENS TO HAVE MORE, THIS CODE MIGHT BREAK WITHOUT TELLING ANYONE
    df %>% 
      dplyr::filter(amount > 0) %>%
      dplyr::group_by(rank) %>% 
      dplyr::mutate(centenary_id = sum((100^as.numeric(as.character(reward_id) %>% str_remove_all('14500'))) * as.numeric(amount))) %>% 
      # dplyr::mutate(n_prizes = n_distinct(reward_id)) %>% 
      dplyr::group_by(reward_id, amount, reward, centenary_id) %>% 
      dplyr::summarise(min_rank = min(rank)) %>% 
      dplyr::select(-centenary_id) %>% 
      dplyr::arrange(min_rank) %>% 
      return()
  }
  
  
  hs.balancedata::gs_credentials()

# Load Quest Spreadsheets
spreadsheet_title <- spreadsheet_name


# Main Events -------------------------------------------------------------

clean_df <- spreadsheet_title %>% googlesheets::gs_title() %>% googlesheets::gs_read(ws = 'guild_events') %>% .[-(1:3), -1]
csv_location <- paste0('~/', game_folder, '/Assets/data/source/csv/leagues.csv')

rewards_names <- c('Rainbow', 'Gold', 'Silver', 'Bronze')
rewards_ids <- c(145001, 145002, 145003, 145004)

rewards_names <- c('Bronze', 'Silver', 'Gold', 'Rainbow')
rewards_ids   <- c(145004, 145003, 145002, 145001)

leagues_names <- c('leagueStarter', 'leagueSapphire', 'leagueRuby', 'leagueDiamond')
leagues_ids <- c('270011', '270012', '270013', '270014')

leagues_df <- tibble::tibble(league = leagues_names, league_id = leagues_ids)
rewards_df <- tibble::tibble(reward = rewards_names, reward_id = rewards_ids)

starter_league <- clean_df[,1:6]
sapphire_league <- clean_df[,8:15]
ruby_league <- clean_df[,15:22]
diamond_league <- clean_df[,22:27]

colnames(starter_league) <- rewards_names %>% c('league_points')
colnames(sapphire_league) <- rewards_names %>% c('league_points')
colnames(ruby_league) <- rewards_names %>% c('league_points')
colnames(diamond_league) <- rewards_names %>% c('league_points')

starter_league$league <- 'leagueStarter'
sapphire_league$league <- 'leagueSapphire'
ruby_league$league <- 'leagueRuby'
diamond_league$league <- 'leagueDiamond'

complete_df <- starter_league %>% 
  dplyr::bind_rows(sapphire_league) %>% 
  dplyr::bind_rows(ruby_league) %>% 
  dplyr::bind_rows(diamond_league) %>% 
  data.table::melt(id.vars = c('league_points', 'league')) %>% 
  dplyr::rename(reward = variable,
         amount = value) %>% 
  dplyr::mutate(amount = ifelse(is.na(amount), 0, amount)) %>% 
  dplyr::left_join(rewards_df) %>% 
  dplyr::left_join(leagues_df) %>% 
  dplyr::group_by(league_id, reward) %>% 
  dplyr::mutate(rank = 1:n())


leagues_csv <- data.table::fread(csv_location)

new_csv <- 
  leagues_csv %>% 
  dplyr::mutate_if(is.logical, as.numeric) %>% 
  data.table::data.table()
for(i in seq_along(leagues_names)){
  this_league <- leagues_names[[i]]
  rewards <- 
    complete_df %>% 
    dplyr::filter(league == this_league) %>% 
    clean_rewards()
  
  ranks <- rewards$min_rank %>% unique()
  
  for(k in seq_along(ranks)){
    rank_rewards <- rewards %>% 
      dplyr::filter(min_rank == ranks[[k]])
    
    n_rewards <- nrow(rank_rewards)
    
    new_csv[i, paste0('prize loots 1 loot ', k, ' rank') := ranks[[k]]]
    
    for(j in 1:n_rewards){
      
      if(!is.na(rank_rewards[j, ]$reward_id)){
        new_csv[i, paste0('prize loots 1 loot ', k, ' item ', j, ' id') := rank_rewards[j, ]$reward_id]
        new_csv[i, paste0('prize loots 1 loot ', k, ' item ', j, ' count') := rank_rewards[j,]$amount %>% as.numeric()]
      }else{
        if(tolower(rank_rewards[j, ]$reward) == 'cash'){
          new_csv[i, paste0('prize loots 1 loot ', k, ' cash amount') := rank_rewards[j, ]$amount %>% as.numeric()]
        }else if(tolower(rank_rewards[j, ]$reward) == 'coins'){
          new_csv[i, paste0('prize loots 1 loot ', k, ' coins amount') := rank_rewards[j, ]$amount %>% as.numeric()]
        }
      }
    }
  }
}

data.table::fwrite(new_csv, csv_location)


# Token Events ------------------------------------------------------------

df <- spreadsheet_title %>% googlesheets::gs_title() %>% googlesheets::gs_read(ws = 'token_events')
csv_location <- paste0('~/', game_folder, '/Assets/data/source/csv/leagues.csv')

clean_df <- df %>% 
  .[-(1:3), -1]

rewards_names <- c('Ruby', 'Sapphire', 'Citrine', 'Jade', 'Diamond', 'Coins', 'Cash')
rewards_ids <- c(46002, 46003, 46005, 46004, 46001, NA, NA)

leagues_names <- c('leagueStarter', 'leagueSapphire', 'leagueRuby', 'leagueDiamond')
leagues_ids <- c('270011', '270012', '270013', '270014')

leagues_df <- tibble::tibble(league = leagues_names, league_id = leagues_ids)
rewards_df <- tibble::tibble(reward = rewards_names, reward_id = rewards_ids)

starter_league <- clean_df[,1:8]
sapphire_league <- clean_df[,10:17]
ruby_league <- clean_df[,19:26]
diamond_league <- clean_df[,28:35]

colnames(starter_league) <- rewards_names %>% c('league_points')
colnames(sapphire_league) <- rewards_names %>% c('league_points')
colnames(ruby_league) <- rewards_names %>% c('league_points')
colnames(diamond_league) <- rewards_names %>% c('league_points')

starter_league$league <- 'leagueStarter'
sapphire_league$league <- 'leagueSapphire'
ruby_league$league <- 'leagueRuby'
diamond_league$league <- 'leagueDiamond'

complete_df <- starter_league %>% 
  dplyr::bind_rows(sapphire_league) %>% 
  dplyr::bind_rows(ruby_league) %>% 
  dplyr::bind_rows(diamond_league) %>% 
  data.table::melt(id.vars = c('league_points', 'league')) %>% 
  dplyr::rename(reward = variable,
         amount = value) %>% 
  dplyr::mutate(amount = ifelse(is.na(amount), 0, amount)) %>% 
  dplyr::left_join(rewards_df) %>% 
  dplyr::left_join(leagues_df) %>% 
  dplyr::group_by(league_id, reward) %>% 
  dplyr::mutate(rank = 1:n())



### Input values


leagues_csv <- data.table::fread(csv_location)

# starting_point <- which(colnames(leagues_csv) == 'prize loots 2 loot 1 rank')
# next_point <- which(colnames(leagues_csv) == 'prize loots 2 loot 2 rank')
# ending_point <- ncol(leagues_csv)
# 
# cols_to_keep <- starting_point : ending_point
# this_event <- leagues_csv[, ..cols_to_keep]

new_csv <- leagues_csv %>% 
  dplyr::mutate_if(is.logical, as.numeric) %>% 
  data.table()
for(i in seq_along(leagues_names)){
  this_league <- leagues_names[[i]]
  rewards <- 
    complete_df %>% 
    dplyr::filter(league == this_league) %>% 
    clean_rewards()
  
  ranks <- rewards$min_rank %>% unique()
  
  for(k in seq_along(ranks)){
    rank_rewards <- rewards %>% 
      dplyr::filter(min_rank == ranks[[k]])
    
    n_rewards <- nrow(rank_rewards)
    
    new_csv[i, paste0('prize loots 2 loot ', k, ' rank') := ranks[[k]]]
    
    for(j in 1:n_rewards){
      
      if(!is.na(rank_rewards[j, ]$reward_id)){
        new_csv[i, paste0('prize loots 2 loot ', k, ' item ', j, ' id') := rank_rewards[j, ]$reward_id]
        new_csv[i, paste0('prize loots 2 loot ', k, ' item ', j, ' count') := rank_rewards[j,]$amount %>% as.numeric()]
      }else{
        if(tolower(rank_rewards[j, ]$reward) == 'cash'){
          new_csv[i, paste0('prize loots 2 loot ', k, ' cash amount') := rank_rewards[j, ]$amount %>% as.numeric()]
        }else if(tolower(rank_rewards[j, ]$reward) == 'coins'){
          new_csv[i, paste0('prize loots 2 loot ', k, ' coins amount') := rank_rewards[j, ]$amount %>% as.numeric()]
        }
      }
    }
  }
}

data.table::fwrite(new_csv, csv_location)
}


#### Predictive Model Load and Clean


#load libraries
library(tidyverse)
library(dplyr)
library(lubridate)
library(ggplot2)
library(mvoutlier)


#Loading in Data
game_data <- read.csv("./data/game.csv")
Viewgame_officials <- read.csv('./data/game_officials.csv')
game_penalties <- read.csv('./data/game_penalties.csv')
game_plays <- read.csv('./data/game_plays.csv')
team_info <- read.csv('./data/team_info.csv')
game_plays_players <- read.csv('./data/game_plays_players.csv')
player_info <- read.csv('./data/player_info.csv')
game_goals <- read_csv('./data/game_goals.csv')
game_scratches <-  read_csv('./data/game_scratches.csv')
load('./data/penalty_data.Rdata')



#Removing duplicates
game_df <- distinct(game_data)

#Making 2 records per game
game_df <- game_data %>% 
  filter(type != 'A') %>% 
  mutate(home_team = home_team_id, away_team = away_team_id) %>% 
  pivot_longer(c('home_team_id', 'away_team_id'), names_to = 'home_away', values_to = 'team')


#Separating into home and away

home_df <- game_df %>%
  filter(home_away == 'home_team_id')
home_df <- home_df %>% 
  rename(opponent = away_team) %>% 
  mutate(outcome_type = str_trim(str_sub(home_df$outcome,-3))) %>% 
  mutate(outcome = str_sub(home_df$outcome, 1,4))
home_df <- home_df %>% 
  rename(goals_for = home_goals, goals_against = away_goals, h_a = outcome) %>% 
  mutate(outcome = 'win')
home_df$outcome[home_df$h_a == 'away'] <- 'loss'
home_df <- home_df %>%
  select(-venue, -venue_link, -venue_time_zone_id, -venue_time_zone_offset, -h_a, -home_away)


away_df <- game_df %>%
  filter(home_away == 'away_team_id')
away_df <- away_df %>% 
  mutate(opponent = home_team) %>% 
  select(-away_team) %>% 
  mutate(outcome_type = str_trim(str_sub(away_df$outcome,-3))) %>% 
  mutate(outcome = str_sub(away_df$outcome, 1,4))
away_df <- away_df %>% 
  rename(goals_for = away_goals, goals_against = home_goals, h_a = outcome) %>% 
  mutate(outcome = 'win')
away_df$outcome[away_df$h_a == 'home'] <- 'loss'
away_df <- away_df %>%
  select(-venue, -venue_link, -venue_time_zone_id, -venue_time_zone_offset, -h_a, -home_away)


#Recombining into one dataframe
game_df <- rbind(home_df, away_df)
game_df <- distinct(game_df)

#Getting Efficiency Metrics
shots_df <- game_plays[game_plays$event == 'Shot' | game_plays$event == 'Goal', ]
shots_for <- shots_df %>% 
  group_by(game_id, team_id_for) %>% 
  summarise(shots = n()) %>% 
  rename(team = team_id_for)

shots_against <- shots_df %>% 
  group_by(game_id, team_id_against) %>% 
  summarise(shots = n()) %>% 
  rename(team = team_id_against)

game_df <- game_df %>% 
  left_join(shots_for, by = c('game_id', 'team')) %>% 
  rename(shots_for = shots)
game_df <- game_df %>% 
  left_join(shots_against, by = c('game_id', 'team')) %>% 
  rename(shots_against = shots)

game_df <- game_df %>% 
  drop_na(shots_for)
game_df <- game_df %>% 
  drop_na(shots_against)

#Getting Penalty Data
penalty_data <- penalty_data %>% 
  select(game_id, play_id, team_id_for, team_id_against, penaltyMinutes) %>% 
  distinct()

#Getting time spent on penalty kill per team
penalty_kills <- penalty_data %>% 
  group_by(game_id, team_id_for) %>% 
  summarise(penalty_kills = n(),
            p_kill_minutes = sum(penaltyMinutes)) %>% 
  rename(team = team_id_for)


#Getting time spent on powerplay per team

powerplays <- penalty_data %>% 
  group_by(game_id, team_id_against) %>% 
  summarise(power_plays = n(),
            p_play_minutes = sum(penaltyMinutes)) %>% 
  rename(team = team_id_against)

game_df <- game_df %>% 
  left_join(powerplays, by = c('game_id', 'team'))

game_df <- game_df %>% 
  left_join(penalty_kills, by = c('game_id', 'team'))

game_df$penalty_kills[is.na(game_df$penalty_kills)] <- 0
game_df$p_kill_minutes[is.na(game_df$p_kill_minutes)] <- 0
game_df$power_plays[is.na(game_df$power_plays)] <- 0
game_df$p_play_minutes[is.na(game_df$p_play_minutes)] <- 0

#Joining with game data
rm(game_df_test)




#Getting Seasonal Stats

seasons = unique(game_df$season)
teams = unique(game_df$team)

game_df2 <- game_df %>% 
  filter(team %in% c(2,3,4))
game_df3 <- distinct(game_df)

season_list <- vector(mode = 'list', length = n_distinct(game_df$season))
i <- 1
for (s in seasons) {
  df <- game_df %>%
    filter(season == s)
  teams = unique(df$team)
  team_list <- vector(mode = 'list', length = n_distinct(df$team))
  j <- 1
    for (t in teams) {
      df_t <- df %>% 
        filter(team == t)
      df_t <- df_t %>% 
        arrange(date_time_GMT) %>% 
        distinct() %>% 
        mutate(game_num = 1:nrow(df_t)) %>%
        mutate(seas_goals_for = cumsum(goals_for)) %>%
        mutate(seas_goals_against = cumsum(goals_against)) %>%
        mutate(seas_goals_for_per_game = round(seas_goals_for/game_num,3)) %>%
        mutate(seas_goals_against_per_game = round(seas_goals_against/game_num,3)) %>%
        mutate(pt_diff = seas_goals_for_per_game - seas_goals_against_per_game) %>% 
        mutate(seas_shots_for = cumsum(shots_for)) %>%
        mutate(seas_shots_against = cumsum(shots_against)) %>%
        mutate(seas_shots_for_per_game = round(seas_shots_for/game_num,3)) %>%
        mutate(seas_shots_against_per_game = round(seas_shots_against/game_num,3)) %>%
        mutate(shot_diff = seas_shots_for_per_game - seas_shots_against_per_game) %>%
        mutate(seas_power_plays = cumsum(power_plays)) %>% 
        mutate(seas_penalty_kills = cumsum(penalty_kills)) %>% 
        mutate(seas_p_play_minutes = cumsum(p_play_minutes)) %>% 
        mutate(seas_p_kill_minutes= cumsum(p_kill_minutes)) %>% 
        mutate(seas_power_plays_per_game = round(seas_power_plays / game_num, 3)) %>% 
        mutate(seas_penalty_kills_per_game = round(seas_penalty_kills / game_num, 3)) %>% 
        mutate(seas_p_play_minutes_per_game = round(seas_p_play_minutes / game_num, 3)) %>% 
        mutate(seas_p_kill_minutes_per_game = round(seas_p_kill_minutes / game_num, 3)) %>% 
        #mutate(pp_per_game = round(power_plays / game)
        mutate(win = 0)
      df_t$win[df_t$outcome == 'win'] <- 1
      streak = calc_streak(df_t$win)
      df_t <- df_t %>%
        mutate(streak = streak) %>% 
        mutate(wins = cumsum(win)) %>%
        mutate(win_pct = wins/game_num) %>%
        mutate(game_prior = lag(date_time_GMT,1))
      df_t <- df_t %>%
        mutate(days_since_last_game = day(days(as.Date(df_t$date_time_GMT)-as.Date(df_t$game_prior))))
      df_t <- df_t %>% 
        mutate(seas_goals_for = lag(seas_goals_for)) %>% 
        mutate(seas_goals_against = lag(seas_goals_against)) %>% 
        mutate(seas_goals_for_per_game = lag(seas_goals_for_per_game)) %>% 
        mutate(seas_goals_against_per_game = lag(seas_goals_against_per_game)) %>% 
        mutate(pt_diff = lag(pt_diff)) %>% 
        mutate(seas_shots_for = lag(seas_shots_for)) %>% 
        mutate(seas_shots_against = lag(seas_shots_against)) %>% 
        mutate(seas_shots_for_per_game = lag(seas_shots_for_per_game)) %>% 
        mutate(seas_shots_against_per_game = lag(seas_shots_against_per_game)) %>% 
        mutate(shot_diff = lag(shot_diff)) %>%
        mutate(seas_power_plays = lag(seas_power_plays)) %>% 
        mutate(seas_penalty_kills = lag(seas_penalty_kills)) %>% 
        mutate(seas_p_play_minutes = lag(seas_p_play_minutes)) %>% 
        mutate(seas_p_kill_minutes= lag(seas_p_kill_minutes)) %>% 
        mutate(seas_power_plays_per_game = lag(seas_power_plays_per_game)) %>% 
        mutate(seas_penalty_kills_per_game =lag(seas_penalty_kills_per_game)) %>% 
        mutate(seas_p_play_minutes_per_game = lag(seas_p_play_minutes_per_game)) %>% 
        mutate(seas_p_kill_minutes_per_game = lag(seas_p_kill_minutes_per_game)) %>% 
        mutate(streak = lag(streak)) %>% 
        mutate(wins = lag(wins)) %>% 
        mutate(win_pct = lag(win_pct))
      team_list[[j]] <- df_t
      j <- j + 1
  }
  team_list <- bind_rows(team_list)
  season_list[[i]] <- team_list
  i <- i + 1
}

game_data_final <- bind_rows(season_list)
View(game_data_final)


#Creating opponent features
opponent_df <- game_data_final
opponent_df <- opponent_df[, -(5:8)]
opponent_df <- opponent_df %>% 
  rename(opp_game_num = game_num, opp_seas_goals_for = seas_goals_for, 
       opp_seas_goals_against = seas_goals_against, opp_pt_diff = pt_diff, opp_seas_shots_for = seas_shots_for, 
       opp_seas_shots_against = seas_shots_against, opp_seas_shots_for_per_game = seas_shots_for_per_game,
       opp_seas_shots_against_per_game = seas_shots_against_per_game, opp_shot_diff = shot_diff, 
       opp_seas_power_plays = seas_power_plays, opp_seas_penalty_kills = seas_penalty_kills, 
       opp_seas_p_play_minutes = seas_p_play_minutes, opp_seas_p_kill_minutes = seas_p_kill_minutes,
       opp_seas_power_plays_per_game = seas_power_plays_per_game,
       opp_seas_penalty_kills_per_game = seas_penalty_kills_per_game, 
       opp_seas_p_play_minutes_per_game = seas_p_play_minutes_per_game, 
       opp_seas_p_kill_minutes_per_game = seas_p_kill_minutes_per_game,
       opp_streak = streak, opp_wins = wins,
       opp_win_pct = win_pct, opp_seas_goals_for_per_game = seas_goals_for_per_game, 
       opp_seas_goals_against_per_game = seas_goals_against_per_game, opp_days_since_last_game = days_since_last_game)

opponent_df <- opponent_df %>% 
  select(-opponent,-type, -home_team, -outcome_type, -outcome, -win, -game_prior, -shots_for, -shots_against,
         -power_plays,-p_play_minutes, -penalty_kills, -p_kill_minutes) %>% 
  rename(opponent  = team)



#Joining with main dataset

game_data_final2 <- game_data_final %>% 
  left_join(opponent_df, by = c('game_id', 'season', 'date_time_GMT', 'opponent'))

#Creating Interaction Variables

game_data_final2$int_pt_diff <- game_data_final2$pt_diff - game_data_final2$opp_pt_diff
game_data_final2$int_win_pct <- game_data_final2$win_pct - game_data_final2$opp_win_pct
game_data_final2$int_shot_diff <- game_data_final2$shot_diff - game_data_final2$opp_shot_diff
game_data_final2$int_pp_diff <- game_data_final2$seas_power_plays_per_game - game_data_final2$opp_seas_power_plays_per_game
game_data_final2$int_pk_df <- game_data_final2$seas_penalty_kills_per_game - game_data_final2$opp_seas_penalty_kills_per_game


#Converting categorical variables
game_data_final2$home_rink_right_start <- 0
game_data_final2$home_rink_right_start[game_data_final2$home_rink_side_start == 'right'] <- 1 

#Saving data
save(game_data_final2, file = './data/modeling_data.Rdata')



get_summary_stats <- function (data, var_list, yvar, export = FALSE) {
  # Function to collect summary statistics for all variables in the training dataset
  # Params data: a data frame
  # Params var_list: a vector of the variable names of the data frame
  # Param yvar: a character label of the y variable
  # Param export: a boolean for csv output
  
  #Getting number of variables
  vec_length <- length(var_list)
  
  #Initializing vectors to store statistics
  variable <- vector(mode = 'character', length = vec_length)
  data_type <- vector(mode = 'character', length = vec_length)
  N <- vector(mode = 'integer', length = vec_length)
  missing <- vector(mode = 'integer', length = vec_length)
  missingPct <- vector(mode = 'double', length = vec_length)
  uniqueVals <- vector(mode = 'integer', length = vec_length)
  corr <- vector(mode = 'double', length = vec_length)
  corr_rank <- vector(mode = 'double', length = vec_length)
  min <- vector(mode = 'double', length = vec_length)
  pct01 <- vector(mode = 'double', length = vec_length)
  pct02 <- vector(mode = 'double', length = vec_length)
  pct05 <- vector(mode = 'double', length = vec_length)
  pct25 <- vector(mode = 'double', length = vec_length)
  pct50 <- vector(mode = 'double', length = vec_length)
  pct75 <- vector(mode = 'double', length = vec_length)
  pct95 <- vector(mode = 'double', length = vec_length)
  pct98 <- vector(mode = 'double', length = vec_length)
  pct99 <- vector(mode = 'double', length = vec_length)
  max <- vector(mode = 'double', length = vec_length)
  mean <- vector(mode = 'double', length = vec_length)
  std.dev <- vector(mode = 'double', length = vec_length)
  
  #Looping through variables to gather statistics
  for (i in seq(vec_length)) {
    variable[[i]] <- var_list[[i]][[1]]
    data_type[[i]] <- typeof(data[, i][[1]])
    N[[i]] <- length(na.omit(data[, i][[1]]))
    missing[[i]] <- sum(is.na(data[, i][[1]]))
    missingPct[[i]] <- missing[[i]] / N[[i]]
    uniqueVals[[i]] <- n_distinct(data[, i][[1]])
    if (is.numeric(data[, i][[1]])) {
      corr[[i]] <- cor(data[, i][[1]], data[, yvar][[1]],  method = "pearson")
      corr_rank[[i]] <- cor(data[, i][[1]], data[, yvar][[1]],  method = "spearman")
      pct01[[i]] <- quantile(data[, i][[1]], .01, na.rm = TRUE)[[1]]
      pct02[[i]] <- quantile(data[, i][[1]], .02, na.rm = TRUE)[[1]]
      pct05[[i]] <- quantile(data[, i][[1]], .05, na.rm = TRUE)[[1]]
      pct25[[i]] <- quantile(data[, i][[1]], .25, na.rm = TRUE)[[1]]
      pct50[[i]] <- quantile(data[, i][[1]], .5, na.rm = TRUE)[[1]]
      pct75[[i]] <- quantile(data[, i][[1]], .75, na.rm = TRUE)[[1]]
      pct95[[i]] <- quantile(data[, i][[1]], .95, na.rm = TRUE)[[1]]
      pct98[[i]] <- quantile(data[, i][[1]], .98, na.rm = TRUE)[[1]]
      pct99[[i]] <- quantile(data[, i][[1]], .99, na.rm = TRUE)[[1]]
      min[[i]] <- min(data[, i][[1]], na.rm = TRUE)
      max[[i]] <- max(data[, i][[1]], na.rm = TRUE)
      mean[[i]] <- mean(data[, i][[1]], na.rm = TRUE)
      std.dev[[i]] <- sd(data[, i][[1]], na.rm = TRUE)
    } else {
      corr[[i]] <- NA
      corr_rank[[i]] <- NA
      pct01[[i]] <- NA
      pct02[[i]] <- NA
      pct05[[i]] <- NA
      pct25[[i]] <- NA
      pct50[[i]] <- NA
      pct75[[i]] <- NA
      pct95[[i]] <- NA
      pct98[[i]] <- NA
      pct99[[i]] <- NA
      min[[i]] <- NA
      max[[i]] <- NA
      mean[[i]] <- NA
      std.dev[[i]] <- NA
    }
  }
  output <- data.frame(variable, data_type, N, missing, missingPct, uniqueVals, corr, corr_rank, min, pct01,
                       pct02, pct05, pct25, pct50, pct75, pct95, pct98, pct99, max, mean, std.dev)
  if (export == TRUE) {
    write_csv(output, path = './results/summary_stats2.csv')
  } else {
    return(output)
  }
}


yvar <- 'goals_for'
names <- names(game_data_final2)
names <- names[names != yvar]

length(names)
summ_stats <- get_summary_stats(game_data_final2, names, yvar)
typeof(game_data_final2[, 1][[1]])
is.numeric(game_data_final2[, 1][[1]])




test <- game_df %>% filter(season == seasons[2])
test <- test %>% filter(team == teams[2])
test <- test %>% arrange(date_time_GMT)
test <- test %>% mutate(game_num = 1:nrow(test))
test <- test %>% mutate(game_num = 1:nrow(test))
test <- test %>% mutate(seas_goals_for = cumsum(goals_for))
test <- test %>% mutate(seas_goals_against = cumsum(goals_against))
test <- test %>% mutate(seas_goals_for_per_game = round(seas_goals_for/game_num,3))
test <- test %>% mutate(seas_goals_against_per_game = round(seas_goals_against/game_num,3))
test <- test %>% mutate(seas_goals_against_per_game = round(seas_goals_against/game_num,3))
test <- test %>% mutate(win = 0)
test$win[test$outcome == 'win'] <- 1
test <- test %>% mutate(wins = cumsum(win))
test <- test %>% mutate(win_pct = wins/game_num)
test <- test %>% mutate(game_prior = lag(date_time_GMT,1))
test <- test %>% mutate(days_since_last_game = day(days(as.Date(test$date_time_GMT)-as.Date(test$game_prior))))
test <- test %>% mutate(win_lag = lag(win))
test <- test %>% mutate(on_streak = (win == 1 & win == win_lag))
streak <- calc_streak(test$win)
test <- test %>% mutate(streak = streak)

View(test)



calc_streak <- function(x){
  output <- c(rep(0,length(x)))
  output[[1]] <- x[[1]]
  for (i in 2:length(output)){
    if (x[[i]] == 1) {
      output[[i]] <- output[[i-1]] + 1
    }
  }
  output
}

View(calc_streak(test$win))


gm = calc_streak(test$win)
gmm = data.frame(test$win, gm)
View(data.frame(test$win, calc_streak(test$win)))

day(days(as.Date(test$date_time_GMT[2])-as.Date(test$date_time_GMT[1])))

View(test)

View(get_summary_stats(opponent_df, names(opponent_df), 'opponent'))
View(get_summary_stats(game_data_final, names(game_data_final), 'goals_for'))

game_data %>%
  mutate(season = as.factor(season)) %>% 
  group_by(season) %>% 
  summarise(games = n_distinct(game_id)) %>% 
  ggplot(mapping = aes(x = season, y = games)) +
  geom_col() +
  theme(axis.text.x = element_text(angle = -45, vjust = -1))

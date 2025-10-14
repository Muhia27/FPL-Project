library(jsonlite) #library Handles Json Data
library(dplyr) #library handles data manipulation

#URL for the FPL API with static Data
fpl_url <- "https://fantasy.premierleague.com/api/bootstrap-static/"

#Fetching JSON data from the API
fpl_data_raw <- fromJSON(fpl_url) # the function fetches the url and parses JSON into R list

names(fpl_data_raw) #explore structure of fetched data 

#$elements contains all player data
players_df <- fpl_data_raw$elements

#$teams contains all teams data
teams_df <- fpl_data_raw$teams

#$events contains gw data
gameweeks_df <- fpl_data_raw$events

#$element_types contains position data
Positions_df <- fpl_data_raw$element_types

#$chips contains chips data
chips_df <- fpl_data_raw$chips

#viewing the data frames
head(players_df)
head(teams_df)
head(gameweeks_df)
head(Positions_df)
head(chips_df)


#Processing the data 
str(players_df) #checks overall structure and data types of columns in players_df

#check missing values
colSums(is.na(players_df))

#cleaninng chance_of_playing columns,NA means a player is fully fit and has 100% chance of playing
players_df <-players_df %>%
  mutate(
    chance_of_playing_next_round=ifelse(is.na(chance_of_playing_next_round),100,
chance_of_playing_next_round),

    chance_of_playing_this_round=ifelse(is.na(chance_of_playing_this_round),100,
chance_of_playing_this_round)
  )

#Re-check missing values for these columns
colSums(is.na(players_df))


#Convert character columns that should be numeric
players_df <- players_df %>%
  mutate(
    ep_next = as.numeric(ep_next),
    ep_this = as.numeric(ep_this),
    form = as.numeric(form),
    points_per_game = as.numeric(points_per_game),
    selected_by_percent = as.numeric(selected_by_percent),
    value_form = as.numeric(value_form),
    value_season = as.numeric(value_season),
    influence = as.numeric(influence),
    creativity = as.numeric(creativity),
    threat = as.numeric(threat),
    ict_index = as.numeric(ict_index),
    expected_goals = as.numeric(expected_goals),
    expected_assists = as.numeric(expected_assists),
    expected_goal_involvements = as.numeric(expected_goal_involvements),
    expected_goals_conceded = as.numeric(expected_goals_conceded)
    
  )

str(players_df)

#Renaming columns
#Rename columns in players_df
players_df <- players_df %>%
  rename(
    player_id = id,
    position_id = element_type,
    team_id = team
    
  )

#rename columns in teams_df
teams_df <- teams_df %>%
  rename(
    team_id = id,
    team_name = name
  )

#Rename columns in Positions_df
Positions_df <- Positions_df %>%
  rename(
    position_id =id,
    position_name=singular_name_short
  )

head(players_df)
head(teams_df)
head(Positions_df)

#Intergrating Data Frames
#Join players_df with Positions_df
players_df <- players_df %>%
  left_join(Positions_df %>% select(position_id, position_name), by = "position_id")

#join updated player_df with teams_df
players_df <- players_df %>%
  left_join(teams_df %>% select(team_id, team_name), by = "team_id")

players_df %>%
  select(first_name, second_name, team_name, position_name, now_cost, total_points, form) %>%
  head()

#names(players_df)
            
            
            
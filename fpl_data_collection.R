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

#Feature engineering 
# setting player values by dividing cost by 10 
players_df <- players_df %>%
  mutate(
    actual_cost = now_cost /10,
    ppm = ifelse(total_points > 0, total_points / actual_cost, 0) #ppm(points per million)
  )

# sort top players now
players_df %>% 
  select(web_name, team_name, total_points, actual_cost, ppm) %>%
  arrange(desc(ppm)) %>% # sort per ppm in descending order
  head(20) # first 20 players

# Positional Grouping players
players_df <- players_df %>% 
  mutate(
    position_group = case_when(
      position_name %in% c("GKP", "DEF") ~ "Defender/Gk",
      position_name %in% c("MID", "FWD") ~ "Attacker",
      TRUE ~ "Unknown"
    )
  )
players_df %>%
  count(position_name,position_group)

#Play availability status
players_df <- players_df %>%
  mutate(
    availability_status =case_when(
      
      status=="a" ~ "Available",
      status == "i" ~ "Injured",
      status == "d" ~ "Doubtful",
      status == "u" ~ "Unavailable",
      status == "s" ~ "Suspended",
      
      TRUE ~ "Unknown"
    )
  )

players_df %>%
  count(availability_status, status, sort = TRUE) #show most common first 


#Data Aggregation and Team Level Prep
#calculating stats for each team 
active_squad_summary_df <- players_df %>%
  filter(availability_status=="Available") %>%
group_by(team_id, team_name) %>%
  summarise(
    #calculating the total cost of all players in the squad 
    active_squad_cost = sum(actual_cost, na.rm = TRUE),
    #Average ppm for each team 
    avg_active_player_ppm = mean(ppm, na.rm=TRUE)
  ) %>%
  
  ungroup()

#border summary with both attacking and defensive metrics
#calculating the average total points per player for each team
teams_summary_df <- players_df %>%
  group_by(team_id, team_name) %>%
  summarise(
    # Attacking metrics
    total_team_xg = sum(expected_goals, na.rm = TRUE),
    total_team_xa = sum(expected_assists, na.rm = TRUE),
    total_goals_scored = sum(goals_scored, na.rm = TRUE),
    
    # Defensive Metrics
    total_clean_sheets = sum(clean_sheets, na.rm = TRUE),
    total_team_xgc = sum(expected_goals_conceded, na.rm = TRUE),
    
    # General Metrics
    avg_player_points = mean(total_points, na.rm = TRUE),
    avg_team_ppm = mean(ppm, na.rm = TRUE),
    total_squad_cost = sum(actual_cost, na.rm = TRUE),
    
    # Discipline Metrics
    total_yellow_cards = sum(yellow_cards, na.rm = TRUE),
    total_red_cards = sum(red_cards, na.rm = TRUE),
    
    .groups = 'drop'
  ) %>%
  
  # Join the active squad summary
  left_join(active_squad_summary_df, by = c("team_id", "team_name")) %>%
  
  # Join the strength ratings
  left_join(
    teams_df %>% select(team_id, strength_attack_home, strength_attack_away, strength_defence_home, strength_defence_away),
    by = "team_id"
  )

teams_summary_df %>%
  select(team_name, total_goals_scored, total_team_xg, total_clean_sheets,
         total_team_xgc, total_squad_cost, active_squad_cost, avg_team_ppm
         ) %>%
  arrange(desc(avg_team_ppm)) %>% # arrange team by team with cost effective players
  head(20)

library(lubridate) # package makes it easy to work with data and time 
#converting deadline_time into data and time
gameweeks_df <- gameweeks_df %>%
  mutate(
    deadline_time = as_datetime(deadline_time)
    
  )

print("---Structure of the date and time conversion---")
str(gameweeks_df)

print("---Head of gameweeks_df after date conversion---")
head(gameweeks_df)

library(ggplot2) # Visualization Library

ggplot(players_df, aes(x= actual_cost, y= total_points)) + 
  geom_point(aes(color = position_name), alpha = 0.6) + 
  geom_smooth(method = "lm", se = FALSE, color = "black", linetype = "dashed") + #adds a regression line
  labs (
    title = "Player Cost vs Total FPL Points",
    subtitle = "Colored by Position",
    x = "Player Cost (£M)",
    y = "Total Points",
    color = "Position"
  ) + 
  theme_minimal()


# Distributing of Points Per million by Position 
ggplot(players_df, aes(x= position_name, y=ppm, fill = position_name)) +
  geom_boxplot(alpha = 0.7) +
  labs (
    title = "Distribution of Player Value (ppm) by Position",
    subtitle = "Higher median values indicate better value on average",
    x = "Position",
    y = "Points Per Million (PPM)",
    fill = "position"
  ) +
  theme_minimal() +
  theme(legend.position = "none")

# make ppm rankings in consideration of highest valued players in every position 
top_value_players_by_position <- players_df %>%
  filter(minutes > 90) %>% 
  group_by(position_name) %>%
  arrange(desc(ppm)) %>%
  slice_head(n=5)  %>% #takes top 5 rows from each group 
  ungroup() %>%
  select(web_name, team_name, position_name, actual_cost, total_points, ppm)

print("--- Top 5 value Players (PPM) by Position ---")
print(top_value_players_by_position, n=20 )

# tier list for players based on their position 
players_df <- players_df %>%
  mutate(
    price_tier = case_when(
      #Tiers for GKs
      position_name == "GKP" & actual_cost  <= 4.5 ~ "Budget GKP",
      position_name == "GKP" & actual_cost > 4.5 ~ "Premium GKP",
      
      #Tiers for DEF
      position_name == "DEF" & actual_cost<=4.5 ~ "Budget DEF",
      position_name == "DEF" & actual_cost > 4.5 & actual_cost <= 5.5 ~ "Mid-Range DEF",
      position_name == "DEF" & actual_cost >5.5 ~ "Premium DEF",
      
      #Tiers in Mid
      
      position_name == "MID" & actual_cost <= 5.5 ~ "Budget MID",
      position_name =="MID" & actual_cost > 5.5 & actual_cost <= 7.5 ~ "Mid-Range MID",
      position_name == "MID" & actual_cost > 7.5 ~ "Premium MID",
      
      #Tiers in FWDs
      position_name == "FWD" & actual_cost <= 6.0 ~ "Budget FWD",
      position_name =="FWD" & actual_cost > 6.0 & actual_cost <= 8.0 ~ "Mid-Range FWD",
      position_name == "FWD" & actual_cost > 8.0 ~ "Premium FWD",
      
      TRUE ~ "other"
    )
  )

#find best 3 in each tier 
top_value_in_tiers <- players_df %>%
  filter(minutes > 180) %>%
  group_by(price_tier) %>%
  arrange(desc(ppm)) %>%
  slice_head(n=3) %>% #take top 3 from each tier 
  ungroup() %>%
  
  mutate(price_tier = factor(price_tier, levels = c(
    
    "Premium FWD", "Mid-Range FWD", "Budget FWD",
    "Premium MID", "Mid-Range MID", "Budget MID",
    "Premium DEF", "Mid-Range DEF", "Budget DEF",
    "Premium GKP",  "Budget GKP", "Other"
    
  ))) %>%
  arrange(price_tier) %>%
  select(web_name,team_name,price_tier,actual_cost,total_points,ppm)
print("--- Top 3 value players PPM by price tier ---")
print(top_value_in_tiers, n=42)

#visualizing the tier list 
top_player_per_tier <- top_value_in_tiers %>%
  group_by(price_tier) %>%
  slice_head(n=1) %>%
  ungroup()

ggplot(top_player_per_tier, aes(x = reorder(price_tier, ppm), y=ppm)) +
  geom_col(aes(fill= substr(
    as.character(price_tier),
  nchar(as.character(price_tier)) -2,
  nchar(as.character(price_tier))
           
)), show.legend = TRUE ) +
  coord_flip() + 
  labs(
    title = "Best Player value (PPM) Found in each Price Tier",
    subtitle = "Comparing single perfoming player from each Category",
    x="Player Price Tier",
    y="PPM of Top Player",
    fill="Position"
  ) +
  theme_minimal()

#Identifying Best Attacking and Defending teams 
#Attacking by (xG)
ggplot(teams_summary_df, aes(x = reorder(team_name, total_team_xg),y = 
total_team_xg)) +
  geom_col(aes(fill = total_team_xg), show.legend = FALSE) + 
  coord_flip() +
  labs(
    title = "Team Attacking Threat (Season to Date)",
    subtitle = "Based on sum of all players' Expected Goals (xG)",
    x="Team",
    y="Total Team xG"
    
  ) +
  theme_minimal()


#Best Defensive team by (xGC)
ggplot(teams_summary_df, aes(x=reorder(team_name, -total_team_xgc),
 y=total_team_xgc )) +
  geom_col(aes(fill = total_team_xgc), show.legend = FALSE) +
  coord_flip() +
  scale_fill_gradient(low = "darkgreen",high = "red") +
  labs(
    title = "Teams Defensive Strength (Season to Date)",
    subtitle = "Based on sum of all players' Expected Goals Conceded (xGC) -Lower is better",
    x= "Team",
    y= "Total Team xGC"
  ) +
  theme_minimal()

# Team Discipline Analysis (Bookings)
ggplot(teams_summary_df, aes(x=reorder(team_name, total_yellow_cards), y= total_yellow_cards)) +
  geom_col(aes(fill = total_yellow_cards), show.legend = FALSE) + 
  geom_text(aes(label = total_red_cards), hjust = -0.3, color = "red", size = 4) +
#Add a red card amount
  coord_flip() +
  scale_fill_gradient(low = "yellow", high = "#FFD700") +
  labs (
    title = "Team Discipline: Total Yellow & Red Cards",
    subtitle = "Red numbers indicate total red cards for the team",
    x="Team",
    y="Total Yellow Cards"
  ) +
  theme_minimal()

#Team Strength vs PLayer Perfomance 
teams_summary_df <- teams_summary_df %>%
  left_join(teams_df %>% select(team_id, strength_overall_home,
strength_overall_away), by ="team_id")

ggplot(teams_summary_df, aes(x= strength_overall_home, y=avg_player_points)) +
  geom_point(aes(color = team_name), size = 4, show.legend = FALSE) +
  ggrepel::geom_text_repel(aes(label = team_name)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  labs(
    title = "Teams Strength vs Average FPL points per Player",
    subtitle = "Does a higher team strength rating correlate with better FPL output?",
    x="Overral Home Strength Rating",
    y="Average FPL Points Per Player"
  ) +
  theme_minimal()
#  Top 20 Most Selected Players with Position

players_df %>%
  arrange(desc(selected_by_percent)) %>%
  head(20) %>%
  mutate(name_and_pos = paste0(web_name, " (", position_name, ")")) %>%
  ggplot(aes(x = reorder(name_and_pos, selected_by_percent), y = selected_by_percent)) +
  geom_col(aes(fill = team_name), show.legend = FALSE) +
  coord_flip() +
  labs(
    title = "Top 20 Most Selected Players",
    # The x-axis "Player"
    x = "Player",
    y = "Selected By Percent (%)"
  ) +
  theme_minimal()

ggsave("plot_new2_top_owned.png", width = 8, height = 7)


# Top 20 Bonus Point Magnets
players_df %>%
  arrange(desc(bonus)) %>%
  head(20) %>%
  ggplot(aes(x = reorder(web_name, bonus), y = bonus)) +
  geom_col(aes(fill = position_name)) +
  coord_flip() +
  labs(
    title = "Top 20 Bonus Point Magnets",
    x = "Player",
    y = "Total Bonus Points",
    fill = "Position"
  ) +
  theme_minimal()

ggsave("plot_new3_bonus_points.png", width = 8, height = 7)


# Team Attack Strength vs. Total Goals Scored by the Whole Team

# We can use our 'teams_summary_df' which already has total_goals_scored
ggplot(teams_summary_df, aes(x = strength_attack_home, y = total_goals_scored)) +
  geom_point(size = 3) +
  ggrepel::geom_text_repel(aes(label = team_name)) +
  geom_smooth(method = "lm", se = FALSE, color = "darkgrey") +
  labs(
    title = "Team Attack Strength vs. Total Goals Scored (All Players)",
    subtitle = "Does FPL's Attack Rating reflect a team's overall goal output?",
    x = "Team Attack Strength Rating",
    y = "Total Goals Scored by Team"
  ) +
  theme_minimal()

ggsave("plot_new6_attack_strength_vs_total_goals.png", width = 8, height = 6)

#  Team Defence Strength vs. Defender/GK Points
players_df %>%
  filter(position_group == "Defender/Gk") %>%
  group_by(team_name) %>%
  summarise(avg_defender_points = mean(total_points)) %>%
  left_join(teams_df, by = "team_name") %>%
  ggplot(aes(x = strength_defence_home, y = avg_defender_points)) +
    geom_point(size = 3) +
    ggrepel::geom_text_repel(aes(label = team_name)) +
    labs(title = "Team Defence Strength vs. Average Defender/GK FPL Points", x="Team Defence Strength", y="Avg. Points for DEFs/GKPs") +
    theme_minimal()

ggsave("plot_new5_defence_strength.png", width = 8, height = 6)

# --- Advanced Insight: Analyzing the Teams of the Top 100 Managers ---

print("--- Starting Analysis of Top Manager Teams ---")

# 1. Get the ID for the main "Overall" classic league
overall_league_id <- 314
print(paste("The Overall League ID is:", overall_league_id))

# 2. Fetch the standings for the Overall league to get the top manager IDs
# We will fetch Page 1 (Top 50) and Page 2 (51-100)
top_managers_p1_url <- paste0("https://fantasy.premierleague.com/api/leagues-classic/", overall_league_id, "/standings/?page_standings=1")
top_managers_p2_url <- paste0("https://fantasy.premierleague.com/api/leagues-classic/", overall_league_id, "/standings/?page_standings=2")

top_managers_p1_raw <- fromJSON(top_managers_p1_url)
top_managers_p2_raw <- fromJSON(top_managers_p2_url)

# Combine the results from both pages into a single data frame
top_managers_df <- rbind(top_managers_p1_raw$standings$results, top_managers_p2_raw$standings$results)
print(paste("Successfully fetched the Top", nrow(top_managers_df), "managers."))

# 3. Get the most recently completed gameweek ID
last_gw_id <- gameweeks_df %>%
  filter(finished == TRUE) %>%
  pull(id) %>%
  max()
print(paste("Fetching team picks for the last completed Gameweek:", last_gw_id))

# 4. Loop through each top manager and get their team picks
all_top_picks <- list()

# Use a for loop to go through all the top managers
for (i in 1:nrow(top_managers_df)) {
  manager_id <- top_managers_df$entry[i]
  
  # Let the user know the progress
  print(paste("Fetching data for manager", i, "of", nrow(top_managers_df), "..."))
  
  picks_url <- paste0("https://fantasy.premierleague.com/api/entry/", manager_id, "/event/", last_gw_id, "/picks/")
  
  picks_data <- try(fromJSON(picks_url), silent = TRUE)
  
  if (!inherits(picks_data, "try-error")) {
    # We store a data frame with the player ID and whether they were captain/vice-captain
    all_top_picks[[i]] <- data.frame(
      player_id = picks_data$picks$element,
      is_captain = picks_data$picks$is_captain,
      is_vice_captain = picks_data$picks$is_vice_captain
    )
  }
  
  # Add a small delay to be polite to the API (0.2 seconds is usually fine)
  Sys.sleep(0.2)
}

# 5. Analyze the results
# Combine the list of data frames into one big data frame
top_picks_df <- do.call(rbind, all_top_picks)

# Count the ownership of each player
top_owned_df <- top_picks_df %>%
  group_by(player_id) %>%
  summarise(
    elite_count = n(),
    captain_count = sum(is_captain),
    vice_captain_count = sum(is_vice_captain),
    .groups = 'drop'
  ) %>%
  # Join with our main players_df to get player names and other info
  left_join(players_df, by = "player_id") %>%
  # Calculate ownership percentage among this elite group
  mutate(elite_ownership_percent = (elite_count / nrow(top_managers_df)) * 100) %>%
  arrange(desc(elite_count))

# 6. View the final result!
print("--- Most Owned Players by the Top 100 FPL Managers ---")
top_owned_df %>%
  select(web_name, team_name, position_name, elite_ownership_percent, selected_by_percent, captain_count) %>%
  head(20)
#Captaincy selection 
top_owned_df %>%
  select(web_name, captain_count) %>%
  filter(captain_count > 0) %>%
  arrange(desc(captain_count)) %>%
  head(5)
# Visualization for Top Manager Ownership
top_owned_df %>%
  head(15) %>% # Take the top 15
  ggplot(aes(x = reorder(web_name, elite_ownership_percent), y = elite_ownership_percent)) +
  geom_col(aes(fill = position_name)) +
  coord_flip() +
  labs(
    title = "Player Ownership Among Top 100 FPL Managers",
    subtitle = "The 'Elite Player Template'",
    x = "Player",
    y = "Ownership Percentage (%)",
    fill = "Position"
  ) +
  theme_minimal()

ggsave("plot_advanced_elite_ownership.png", width = 8, height = 7)




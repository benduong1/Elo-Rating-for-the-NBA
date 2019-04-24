getwd()
setwd("C:/Users/bendu/Desktop/NBA Data/ELO Rating")

# install.packages("elo")
library(elo)
# install.packages("tidyverse")
library(tidyverse)


#### FIRST we must make a dataframe that contains....
# 1) game_date
# 2) game_id
# 3) home_team
# 4) home_score
# 5) away_team
# 6) away_score

#### This is the dataframe we will use to run the Elo rating system

box_scores <- read.csv("box_scores.csv")
# This is a dataframe that contains all the box scores for the NBA 2018-19 regular season
# This dataframe is not how we need it 
# Currently each row represents a player's stats for each game
# We will now prepare this data to something we can work with

colnames(box_scores) <- tolower(colnames(box_scores))
# Converting the column names to lower case so they are easier to work with

box_scores$x <- NULL
# Removing column "x" because it's just a count column and we can just us the row number for that

###### Sorting games based on who was the home team and who was the away team

# Since the data does not clearly give home and away teams we need to create our own

box_scores %>% 
  select(team_abbreviation, matchup)
# The above code shows us which team the player/row played for and the "matchup"
# You can see 2 examples of what we see here
#### "440                 OKC   OKC @ MIL"
#### "443                 LAL LAL vs. UTA"
# For each of these the team_abbreciation is the same as the first team that appears on "matchup"
# We can also see in "matchup" that in the middle it says either "@" or "vs."
# We can combine the knowledge of the 2 rules above to say that if we have OKC in "team_abbreviation" and an "@"
# in "matchup" that means OKC is the away team
# The reverse can be made on when it is "vs." that would mean OKC is the home team

working_df <- box_scores %>% 
  select(game_date, game_id, team_abbreviation, matchup, pts) %>% 
  group_by(team_abbreviation, game_id) %>% 
  mutate(score = sum(pts))
# Creating a new dataframe for us to work with so that if we make a mistake we still have the original on hand
# This is a good practice to follow
# We only select the columns we need for this analysis

working_df$matchup <- sub('...', ' ', working_df$matchup)
# Removing the first team in "matchup"
# '.' represents a wildcard character
# In this case we are removing the first 3 characters in "matchup"
# we already have that information in the "team_abbreviation" column

working_df$matchup <- substr(working_df$matchup, 1, nchar(working_df$matchup)-3)
# We now remove the last 3 characters in the "matchup" column
# This is so we are only left with "  @ " and "  vs. "
# We will remove those spaces so this is easier to work with

working_df$matchup <- sub("  @ ", "@", working_df$matchup)
working_df$matchup <- sub("  vs. ", "vs", working_df$matchup)
# Now we have removed the spaces in matchup this is easier to work with

# Now we will seperate the home team and the away team into different dataframes
# This will allow us to label which team is home and away
# Then we will match the teams together based on the "game_id"

working_df$home_away <- "home"
# We are adding a "home_away" column and this will be used to label if the team was home or away
# Right now we are putting "home" for all values
# We will add in the "away" for the correct away teams in the next line

working_df$home_away[which(working_df$matchup == "@")] <- "away"
# Now we have a colunm that labels is the team was home or away

# We will be seperating working_df into 2 dataframes
# 1 will contain the home teams and the other will contain the away team
home_teams <- working_df %>% 
  filter(home_away == "home")

away_teams <- working_df %>%
  filter(home_away == "away")
# Now we have our 2 dataframes with our home and away teams

game_match <- match(home_teams$game_id, away_teams$game_id)
# Create a vector that contains the row in "away_teams" that matches to "home_teams"

home_teams$away_team <- away_teams$team_abbreviation[game_match]
# Now using that matching vector to add the columns from "away_team" that matches the "home_teams"

home_teams$away_score <- away_teams$score[game_match]
# Using the same matching vector we add the scores of the away team

colnames(home_teams)[3] <- "home_team"
# Changing the column name from "team_abbreviation" to "home_team"

home_teams$matchup <- NULL
home_teams$home_away <- NULL

elo <- home_teams
# Now we have a dataframe that contains the "home_team", "away_team", their scores, and game_id

elo$pts <- NULL
# We are removing the "pts" column
# This is a column that contains how many points a player has scored in each game
# We needed this to calculate the total points each team scored
# Now that we made our calculation we do not need it anymore

colnames(elo)[4] <- "home_score"
# Changing the column name from "score" to "home_score"

elo <- elo[which(duplicated(elo$game_id) == FALSE),]
# There are duplicates because each row was information from all players
# Now we have eliminated the duplicates
# There are a total of 1230 NBA games in the regular season
# So our dataframe should contain exactly 1230 observations

elo <- elo %>% 
  arrange(game_date)
# Now we have arranged the games by the date

rm(working_df)
rm(game_match)
rm(away_teams)  
rm(home_teams)  
# We are removing the dataframes we used to create our elo dataframe
# These dataframes are not needed anymore

elo$home_win <- ifelse(elo$home_score > elo$away_score, 1, 0)
# Creating a column that states whether or not the home team won or lost
# We need this so that the elo package will know who won the game

elo_scores <- elo.run(home_win ~ home_team + away_team, data = elo, k = 100)
# Runs the elo system on all the games and creates a list to house them

x <- as.matrix(elo_scores)
# Converts the list of elo_scores to a matrix so that it was be read

x
# Now we can see how each team's elo rating moves through out the season

final.elos(elo_scores)
# Gives the final output of each teams elo score
# Portland has the highest ELO score so according to this system Portland is the best team
#Libraries needed
library(tidyverse)
library(lubridate)
library(readr)
library(dplyr)
#all years are in a four digit format, all column names are consistent across seasons

#Beginning of my function
EPL_Standings <- function(user_date, user_season) {
  
  #convert input to date type, from string/character type
  user_date <- as.Date(user_date,format = '%m/%d/%Y')
  
  #if statement to select the correct season
  #only 2020/21-2018/19 needed for project
  if (user_season == "2020/21") {
    football_data <- read_csv("http://www.football-data.co.uk/mmz4281/2021/E0.csv")
  } else if (user_season == "2019/20"){
    football_data <- read_csv("http://www.football-data.co.uk/mmz4281/1920/E0.csv")
  } else if (user_season == "2018/19"){
    football_data <- read_csv("http://www.football-data.co.uk/mmz4281/1819/E0.csv")
  } else {
    print("data unavailable at this time")
  }
  
  
  # create a smaller dataframe focused on the needed variables
  football_stats <- select(football_data, Date, HomeTeam, AwayTeam, FTHG, FTAG, FTR)
  
  #format dates in dataframe to date types
  football_stats <- football_stats %>% 
                    mutate(Date1 = dmy(Date)) %>%
                    filter(Date1 <= user_date) %>% #filter to include only desired dates
                    arrange(desc(Date1)) #sort by date
    
    # create new columns counting points and wins/losses/draws for given date
    football_stats <- mutate(football_stats, home_points = ifelse(FTR == 'D', 1, ifelse(FTR == 'H', 3, ifelse(FTR == 'A', 0, NA))), #home team points for the match
                             home_win = ifelse(FTR == 'H',1,0), # if win at home, count 1
                             home_draw = ifelse(FTR == 'D',1,0), # if draw at home, count 1
                             home_loss = ifelse(FTR == 'A',1,0)) # if lost at home, count 1
  
  football_stats <- mutate(football_stats, away_points = ifelse(FTR == 'D', 1, ifelse(FTR == 'H', 0, ifelse(FTR == 'A', 3,NA))), #away team points for the match
                           away_win = ifelse(FTR == 'A', 1,0), # if win away, count 1
                           away_draw = ifelse(FTR == 'D',1,0), # if dwaw away, count 1
                           away_loss = ifelse(FTR == 'H',1,0)) # if lost away, count 1
  
  #create smaller data frames to use to form a join
  #one for home stats, one for away stats
  #then join on teamName
  home_stats <- select(football_stats, HomeTeam, FTHG, FTAG, FTR,
                       home_points, home_win, home_draw, home_loss) %>%
    group_by(TeamName = HomeTeam) %>%
    summarize(count_home = n(), # total games for the team 
              home_points = sum(home_points), # total points
              home_win = sum(home_win), # home wins
              home_draw = sum(home_draw), # home draws
              home_loss = sum(home_loss), # home losses
              goals_for_home = sum(FTHG), # total goals scored at home
              goals_against_home = sum(FTAG))
  
  
  away_stats <- select(football_stats, AwayTeam, FTHG, FTAG, FTR,
                       away_points, away_win, away_draw, away_loss) %>%
    group_by(TeamName = AwayTeam) %>%
    summarize(count_away = n(), # total game played 
              away_points = sum(away_points), # total points earned
              away_win = sum(away_win), # total wins
              away_draw = sum(away_draw), # total draws
              away_loss = sum(away_loss), # total losses
              goals_for_away = sum(FTHG), # total goals scored
              goals_against_away = sum(FTAG))
  
  # join home subset and away subset by 'TeamName'
  joint_stats <- home_stats %>%
    full_join(away_stats, by = c('TeamName'))
  
  joint_stats[is.na(joint_stats)] <- 0
  
  # Make the variables for the assignment
  #start with whats need to calculate the other variables- building blocks to other columns
  joint_stats <- joint_stats %>%
    mutate(Wins = home_win + away_win,
           Losses = home_loss + away_loss,
           Draws = home_draw + away_draw,
           Points = home_points + away_points,
           GS = goals_for_home + goals_for_away,
           GA = goals_against_home + goals_against_away,
           MatchesPlayed = count_home + count_away
    )
  #move onto easier ones to calculate from others
  
  joint_stats <- joint_stats %>%
    mutate(PPM = (Points/MatchesPlayed),
           PtPct = (Points/(3*MatchesPlayed)),
           GSM = (GS/MatchesPlayed),
           GAM = (GA/MatchesPlayed)
    )
  #rounding the variables created so far to the thousandth, popular choice for sports data
  joint_stats <- joint_stats %>%
    mutate(PPM = round(PPM,3),
           PtPct = round(PtPct,3),
           GSM = round(GSM,3),
           GAM = round(GAM,3)
    )
  #last variables before streak and last10 games record
  joint_stats <- joint_stats %>%
    mutate(Record = paste0(Wins,'-',Losses,'-',Draws), 
           HomeRec = paste0(home_win,'-',home_loss,'-',home_draw), 
           AwayRec = paste0(away_win,'-',away_loss,'-', away_draw),
    )
  #last10 game record, how did each team perform in the last ten games?
  last10_home <- football_stats %>%
            mutate(Date1,
                   TeamName = HomeTeam,
                   Wins = home_win,
                   Draw = home_draw,
                   Loss = home_loss)
  last10_away <- football_stats %>%
                    mutate(Date1,
                    TeamName = AwayTeam,
                    Wins = away_win,
                    Draw = away_draw,
                    Loss = away_loss)
  last10_both = bind_rows(last10_home,last10_away)
  last10_final <- last10_both %>%
            group_by(TeamName) %>%
            arrange(desc(Date1)) %>%
            top_n(10, wt = Date1) %>%
            summarize(Wins = sum(Wins),
                      Draw = sum(Draw),
                      Loss = sum(Loss)) %>%
            mutate(Last10 = paste0(Wins, '-', Loss, '-', Draw)) %>%
            select(TeamName, Last10)
  

  missing_streak <- joint_stats %>%
    inner_join(last10_final, by = c('TeamName')) %>% 
    arrange(TeamName) %>%
    select(TeamName, Record, HomeRec, 
           AwayRec, MatchesPlayed, 
           Points, PPM, PtPct, GS, 
           GSM, GA, GAM, Last10)
  
  home_streak_data <- football_stats %>%
                  select(Date1, TeamName = HomeTeam, WLD = home_points) %>% #WLD is my win, lose, draw variable
                  arrange(TeamName)
  away_streak_data <- football_stats %>%
                  select(Date1,TeamName = AwayTeam, WLD = away_points) %>% #WLD is my win, lose, draw variable
                  arrange(TeamName)
  streak_data <- bind_rows(away_streak_data,home_streak_data) %>%
                arrange(TeamName, desc(Date1)) 
  streak_data_counted <- streak_data %>%
                        group_by(TeamName) %>% 
                        mutate(outcome_lag = ifelse(WLD == lag(WLD), 1,0)) #recode by win, lose, draw
  streak_complete <- streak_data_counted %>%
                        group_by(TeamName) %>%
                        summarize(Streak = min(which(outcome_lag == 0)) - 1)
  
  EPL_final <- missing_streak %>%
              inner_join(streak_complete, by = c('TeamName'))%>%
              select(TeamName, Record, HomeRec, 
                     AwayRec, MatchesPlayed, 
                     Points, PPM, PtPct, GS, 
                     GSM, GA, GAM, Last10, Streak)
#couldn't quite get the streak variable right in time to submit
  return(EPL_final)
  view(EPL_final)
}

#tests for each season
EPL_Standings("12/15/2020", "2020/21")
EPL_Standings("12/31/2019", "2019/20")
EPL_Standings('12/31/2018', '2018/19')

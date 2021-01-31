rm=(list = ls())
library(data.table)
library(dplyr)
library(stringr)
library(tidyverse)
library(lubridate)



tennis <- read.csv("/Users/awburns2/Documents/CIZR\ CSV\ 2018-2020.csv")

#at least 17 metrics 
#want cummulative stats report for each player
#3.Errors forced 4.Unforced errors5.Break points6.Aces7.Double Faults8.1st serve in9.1st serves won10.2nd serve in11.2nd serves won12.1st serve returns13.2nd serve returns14.Short rallies won <515.Medium rallies won 5-816.Long rallies won >817.Service games won

#mutating date variable to numeric :) 
tennis$date = as.Date(tennis$date, format = '%m/%d/%y' )
s3=tennis%>%filter(between(tennis$date, as.Date("2019-10-01"), as.Date("2020-03-10")))
s2=tennis%>%filter(between(tennis$date, as.Date("2019-01-21"), as.Date("2019-05-09")))
s1=tennis%>%filter(between(tennis$date, as.Date("2018-02-12"), as.Date("2018-04-25")))
s1$season="2017-2018 season"
s2$season="2018-2019 season"
s3$season="2019-2020 season"
ts_season=rbind(s1,s2,s3)
table(ts_season$season)
class(tennis$date)
view(tennis$date)




#creating seasons based on date value
S1
S2
S3

#Dropping old character date column
tennis = select(tennis, -3)
colnames(tennis)

#making sure player names are correct
summary(tennis$player)
unique(tennis$player)


#fixing player names 
tennis$player[tennis$player=='Reami'] <- 'Adriana Reami'
tennis$player[tennis$player=='Moldovan'] <- 'Bianca Moldovan'
tennis$player[tennis$player=='Bianca Moldovan )'] <- 'Bianca Moldovan'
tennis$player[tennis$player=='Rebol'] <- 'Amanda Rebol'
tennis$player[tennis$player=='claudia Wiktorin'] <- 'Claudia Wiktorin'
tennis$player[tennis$player=='Rebol'] <- 'Amanda Rebol'
tennis$player[tennis$player=='Tayla Bridges'] <- 'Taylor Bridges'
unique(tennis$player)

#fixing missing match name 
tennis$matchName[tennis$player== 'Lexi Keberle' & tennis$opp == 'Anna Campana'] <- 'NC State vs Wake Forest'
tennis$matchName[tennis$player== 'Anna Rogers' & tennis$opp == 'Sofia Munera'] <- 'NC State vs UVA'
tennis$matchName[tennis$player== 'Jaeda Daniel' & tennis$opp == 'Tenika McGiffin'] <- 'NC State vs UTS'
tennis$matchName[tennis$player== 'Adriana Reami' & tennis$opp == 'detkovskaya'] <- 'NC State vs OSU'
tennis$matchName[tennis$player== 'Anna Rogers' & tennis$opp == 'Detkovskaya'] <- 'NC State vs OSU'
tennis$matchName[tennis$player== 'Alana Smith' & tennis$opp == 'mattel'] <- 'NC State vs UCF'
tennis$matchName[tennis$player== 'Jaeda Daniel' & tennis$opp == 'Silva'] <- 'NC State vs UTS'
tennis$matchName[tennis$player== 'Adriana Reami' & tennis$opp == 'merteena'] <- 'NC State vs UTS'
tennis$matchName[tennis$player== 'Alana Smith' & tennis$opp == 'wolfberg'] <- 'NC State vs OSU'
tennis$matchName[tennis$player== 'Jaeda Daniel' & tennis$opp == 'Thamchaiwait'] <- 'NC State vs OSU'
tennis$matchName[tennis$player== 'Alana Smith' & tennis$opp == 'emma'] <- 'NC State vs WFU'
tennis$matchName[tennis$player== 'Jaeda Daniel' & tennis$opp == 'Thamchaiwait'] <- 'NC State vs OSU'
tennis$matchName[tennis$player== 'Adriana Reami' & tennis$opp == 'kessler'] <- 'NC State vs UF'
tennis$matchName[tennis$player== 'Jaeda Daniel' & tennis$opp == 'Sharma'] <- 'NC State vs UCF'
tennis$matchName[tennis$player== 'Anna Rogers' & tennis$opp == 'Sleeth'] <- 'NC State vs UF'
tennis$matchName[tennis$player== 'Alana Smith' & tennis$opp == 'boulais'] <- 'NC State vs OSU'
tennis$matchName[tennis$player== 'Adriana Reami' & tennis$opp == 'Stolamr'] <- 'NC State vs UCF'
tennis$matchName[tennis$player== 'Alana Smith' & tennis$opp == 'Morales'] <- 'NC State vs ETSU'
tennis$matchName[tennis$player== 'Anna Rogers' & tennis$opp == 'Hourigan'] <- 'NC State vs GT'
tennis$matchName[tennis$player== 'Claudia Wiktorin' & tennis$opp == 'Mcarthy'] <- 'NC State vs Duke'
tennis$matchName[tennis$player== 'Adriana Reami' & tennis$opp == 'Riley'] <- 'NC State vs UT'
tennis$matchName[tennis$player== 'Claudia Wiktorin' & tennis$opp == 'Schuck'] <- 'NC State vs UT'
tennis$matchName[tennis$player== 'Helene Grimm' & tennis$opp == 'Hamlin'] <- 'NC State vs Duke'
view(tennis$matchName)

# #ask about match name tomorrow?
# tennis$matchName[tennis$player=='Alana Smith' & tennis$opp == 'adriana reami'] <- 'NC State vs NC State (Practice?)'
# tennis %>% 
#   #total_points 
#   #group_by(player, matchId) %>%
#   #summarise(player, set, game, gamePoint) %>%
#   #arrange(Model_Year) %>% 
#   filter(player == 'Alana Smith' & opp == 'adriana reami') %>%
#   select(player, matchName, opp, date) 



#finding total average points won per match for all matches played by player
totalpointswon <- tennis %>% 
  group_by(player) %>%
  filter(pointWonBy == 0) %>%
  mutate(total_points=sum(pointsWon,  na.rm=TRUE)) %>%
  select(player, total_points) %>%
  ungroup(player)%>%
  distinct()

#filter by player then sum total points in those rows / amount of rows name appears 



#count rows where pointwonby = 0 then 

# totalpointswon$avgpointspergame <- [totalpointswon$total_points]

#winners
tennis$gamesWon
winners <- tennis %>% 
  group_by(player) %>%
  summarise(total_wins=sum(gamesWon,  na.rm=TRUE)) %>%
  select(player, total_wins) 


# 
# #total errors forced
# tennis$outcome
# errors_forced <- tennis %>% 
#   group_by(player) %>%
#   filter(outcome == 'ForcedError') %>%
# 
# errors_forced %>%
#   group_by(player) %>%
#   
# 
# #errors unforced


#BEGINNING OF MY CODE
#exploratory code
unique(tennis$rallyLength)
class(tennis$pointWonBy)


player_data = ts_season %>%
  group_by(player, season) %>%
  mutate(second_serve_won = if_else(server == 0 & pointWonBy == 0 & firstServeIn == FALSE, 1, 0),
            first_serve_return = if_else(server == 1 & rallyLength > 1 & firstServeIn == TRUE, 1, 0),
            second_serve_return = if_else(server == 1 & rallyLength > 1 & firstServeIn == FALSE, 1, 0),
            short_rally_won = if_else(pointWonBy == 0 & rallyLength < 5, 1, 0),
            medium_rally_won = if_else(pointWonBy == 0 & rallyLength >= 5 & rallyLength < 8, 1, 0),
            long_rally_won = if_else(pointWonBy == 0 & rallyLength >= 8, 1, 0)
            ) %>%
  summarise(second_serve_won_t = sum(second_serve_won),
            first_serve_return_t = sum(first_serve_return),
            second_serve_return_t = sum(second_serve_return),
            short_rally_won_t = sum(short_rally_won),
            medium_rally_won_t = sum(medium_rally_won),
            long_rally_won_t = sum(long_rally_won),
            service_games_won = sum(server == 0 & gameWonBy == 0)
            ) %>%
  select(player, season, second_serve_won_t, first_serve_return_t, second_serve_return_t, 
         short_rally_won_t, medium_rally_won_t, long_rally_won_t, service_games_won)
            
traceback()            


#?if_else()


second_serve_won = (count(firstServeIn == FALSE & server == 0) / (count(firstServeIn == FALSE & server == 0) + count(outcome == 'Fault' & server == 0)))
#short_rally_won = if_else(pointWonBy == 0 & rallyLength < 5 & rallylength > 1 & server == 0, 1, if_else(server == 1 & pointWonBy == 0 & rallyLength < 5, 1, 0))

#all stats on a per match basis
#total points played
player_point_totals = ts_season %>%
  group_by(player, matchId) %>%
  count(player, matchId, season) %>%
  rename(total_points_played = n)

#total serves
player_serve_total = ts_season %>%
  group_by(player, matchId) %>%
  filter(server == 0) %>%
  count(player, matchId, season) %>%
  rename(total_serves = n)

#total points won
player_total_points_won = ts_season %>%
  group_by(player, matchId) %>%
  filter(pointWonBy == 0) %>%
  count(player, matchId, season) %>%
  rename(total_points_won = n)

#total points lost
player_total_points_lost =ts_season %>%
  group_by(player, matchId) %>%
  filter(pointWonBy == 1) %>%
  count(player, matchId, season) %>%
  rename(total_points_lost = n)

#total short rallies encountered
player_short_rallies = ts_season %>%
  group_by(player, matchId) %>%
  filter(rallyLength < 5) %>%
  count(player, matchId, season) %>%
  rename(short_rallies_played = n)

#total medium rallies encountered
player_medium_rallies = ts_season %>%
  group_by(player, matchId) %>%
  filter(rallyLength >= 5 & rallyLength < 8) %>%
  count(player, matchId, season) %>%
  rename(medium_rallies_played = n)

#total medium rallies encountered
player_long_rallies = ts_season %>%
  group_by(player, matchId) %>%
  filter(rallyLength > 8) %>%
  count(player, matchId, season) %>%
  rename(long_rallies_played = n)

df=merge(player_point_totals, player_serve_total, by = c("matchId", "player", 'season'), all.x = TRUE)
df1=merge(df,player_total_points_won, by = c("matchId", "player", "season"), all.x = TRUE)
df2=merge(df1, player_total_points_lost, by = c("matchId", "player", "season"), all.x = TRUE)
df3=merge(df2,player_short_rallies, by = c("matchId", "player", "season"), all.x = TRUE)
df4=merge(df3,player_medium_rallies, by = c("matchId", "player", "season"), all.x = TRUE)
final = merge(df4,player_long_rallies, by = c("matchId", "player", "season"), all.x = TRUE)

write_csv(final, '/Users/awburns2/Documents/GeneralStats.csv')

ts_test = tennis %>%
  group_by(player) %>%
  glm(relevel(factor(pointWonBy), ref = '1') ~ firstServeIn + shotType + rallyLength, family = binomial(link = logit))

logit_ten = glm(relevel(factor(pointWonBy), ref = '1') ~ firstServeIn + shotType + rallyLength, data = ts_test, family = binomial(link = logit))
summary(logit_ten)

exp(.04316)

exp(.06)
unique(tennis$shotType)

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
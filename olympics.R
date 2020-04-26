## library packages
library(tidyverse)
library(nbastatR)
library(data.table)

## define teams then combine
## 2004 team: Bronze
team_2004 <- c(
  "Carmelo Anthony", "Carlos Boozer", "Tim Duncan", "Allen Iverson"
  ,"LeBron James", "Richard Jefferson", "Stephon Marbury", "Shawn Marion"
  ,"Lamar Odom", "Emeka Okafor", "Amar'e Stoudemire", "Dwyane Wade") %>% data.frame() %>% 
  select(player = 1) %>% 
  mutate(player = as.character(player), 
         year = 2004)

## 2008 team: Gold
team_2008 <- c(
  "Deron Williams", "Dwyane Wade", "Michael Redd", "Tayshaun Prince"
  ,"Chris Paul", "Jason Kidd", "LeBron James", "Dwight Howard"
  ,"Kobe Bryant", "Chris Bosh", "Carlos Boozer", "Carmelo Anthony") %>% data.frame() %>% 
  select(player = 1) %>% 
  mutate(player = as.character(player), 
         year = 2008)

## 2012 team: Gold
team_2012 <- c(
  "Carmelo Anthony", "Kobe Bryant", "Tyson Chandler", "Anthony Davis"
  ,"Kevin Durant", "James Harden", "Andre Iguodala", "LeBron James"
  ,"Kevin Love", "Chris Paul", "Russell Westbrook", "Deron Williams") %>% data.frame() %>% 
  select(player = 1) %>% 
  mutate(player = as.character(player), 
         year = 2012)

## 2016 team: Gold
team_2016 <- c(
  "Carmelo Anthony", "Harrison Barnes", "Jimmy Butler", "DeMarcus Cousins"
  ,"DeMar DeRozan", "Kevin Durant", "Paul George", "Draymond Green"
  ,"Kyrie Irving", "DeAndre Jordan", "Kyle Lowry", "Klay Thompson") %>% data.frame() %>% 
  select(player = 1) %>% 
  mutate(player = as.character(player), 
         year = 2016)

## combine teams into olympic_teams variable 
olympic_teams <- bind_rows(team_2004, team_2008, team_2012, team_2016) %>% data.frame() %>% 
  mutate(olympic_year = 1)
## remove for space
rm(list= ls()[(ls() != 'olympic_teams')])

## create empty df for loop output of player stats
all_players_stats <- data.frame()

## begin loop
## loop 1: teams
for (team in olympic_teams$year %>% unique()){
  print(team)
  team_roster <- olympic_teams %>% filter(year == team)
  
  ## loop 2: players   
  for (p_id in team_roster$player %>% unique()){
    # print player name getting stats for
    print(paste('Getting stats for', p_id,  sep = ' '))
    ## get player stats
    all_stats <- suppressWarnings(
      suppressMessages(
        players_careers(players = p_id
                        ,return_message = FALSE
                        ,assign_to_environment = T))) %>% data.frame()

    ## extract regular season data
    reg_season <- dataPlayerSeasonTotalsRegularSeason %>% data.frame()
    ## extract postseason data
    post_season <- dataPlayerSeasonTotalsPostSeason %>% data.frame()
    
    ## add if statement in case player did not have postseason stats
    if(sum(ls() %in% 'dataPlayerSeasonTotalsPostSeason') > 1){
      post_season <- dataPlayerSeasonTotalsPostSeason %>% data.frame()
    }
    
    ## combine player reg season & playoff stats
    player_season_stats <- bind_rows(reg_season, post_season)
    
    ## add individual player to final output 
    all_players_stats <- bind_rows(all_players_stats, player_season_stats) %>% distinct()
    
    ## clear memory for space
    rm(list= ls()[!(ls() %in% c('all_players_stats','olympic_teams', 'team_roster'))])
    
  } ## end loop 1 
  
} ## end loop 2

## clean data 
rs <- all_players_stats %>% 
  filter(slugSeasonType == 'RS') %>% # remove postseason stats
  mutate(season_start = substr(slugSeason, 1, 4)) %>% 
  group_by(namePlayer, slugSeason, slugSeasonType) %>% 
  mutate(teams_played_for = 1:n()) %>% ungroup() %>% 
  group_by(namePlayer, slugSeasonType, slugSeason) %>% 
  mutate(max_teams = max(teams_played_for)) %>% ungroup() %>% 
  filter(teams_played_for == max_teams) %>% 
  select(-teams_played_for, -max_teams, -urlNBAAPI) %>% 
## pts, rbs, asts, 2pg fgp, 3p fgp, fgp, ft rate, minutes per game
  group_by(namePlayer) %>% 
  mutate(career_pts = sum(ptsTotals, na.rm = TRUE), 
         career_rbs = sum(trebTotals, na.rm = TRUE),
         career_asts = sum(astTotals, na.rm = TRUE), 
         career_2pfga = sum(fg2aTotals, na.rm = TRUE), 
         career_2pfgm = sum(fg2mTotals, na.rm = TRUE), 
         career_3pfga = sum(fg3aTotals, na.rm = TRUE), 
         career_3pfgm = sum(fg3mTotals, na.rm = TRUE), 
         career_fga = sum(fgaTotals, na.rm = TRUE), 
         career_fgm = sum(fgmTotals, na.rm = TRUE),
         career_fta = sum(ftaTotals, na.rm = TRUE), 
         career_ftm = sum(ftmTotals, na.rm = TRUE), 
         career_mins = sum(minutesTotals, na.rm = TRUE), 
         cg = sum(gp, na.rm = TRUE)) %>% ungroup() %>% 
  data.frame() %>% 
  merge(olympic_teams, 
        by.x = c('namePlayer', 'season_start'), 
        by.y = c('player', 'year'), 
        all.x = TRUE) %>% 
  mutate(olympic_year = as.numeric(!is.na(olympic_year))) %>% 
  ## season stats 
  mutate(season_pts = ptsTotals / gp, 
         season_rbs = trebTotals / gp, 
         season_asts = astTotals / gp, 
         season_2p_pct = fg2mTotals / fg2aTotals, 
         season_3p_pct = fg3mTotals / fg3aTotals, 
         season_fg_pct = fgmTotals / fgaTotals,
         season_ftr = ftaTotals / fgaTotals, 
         season_mpg = minutesTotals / gp) %>% 
  ## career stats 
  mutate(career_pts = career_pts / cg, 
         career_rbs = career_rbs / cg, 
         career_asts = career_asts / cg, 
         career_2p_pct = career_2pfgm / career_2pfga, 
         career_3p_pct = career_3pfgm / career_3pfga, 
         career_fg_pct = career_fgm / career_fga, 
         career_ftr = career_fta / career_fga, 
         career_mpg = career_mins / cg) %>% 
  ## above avg stats 
  mutate(pts = as.numeric(career_pts <= season_pts), 
         rbs = as.numeric(career_rbs <= season_rbs), 
         asts = as.numeric(career_asts <= season_asts), 
         fg_2 = as.numeric(career_2p_pct <= season_2p_pct), 
         fg_3 = as.numeric(career_3p_pct <= season_3p_pct), 
         fg = as.numeric(career_fg_pct <= season_fg_pct), 
         ftr = as.numeric(career_ftr <= season_ftr), 
         mpg = as.numeric(career_mpg <= season_mpg)) %>% 
  mutate(season_3p_pct = ifelse(season_3p_pct == 'Inf' | 
                                  is.na(season_3p_pct), 0, 
                                season_3p_pct), 
         olympic_team = ifelse(olympic_year == 1, 
                               substr(slugSeason, 1, 4), NA))

## all seasons dataset 
all_seasons_data <- rs 
all_seasons_data$above_avg_stats <- rowSums(all_seasons_data[, 62:69], na.rm = TRUE)
all_seasons_data$above_avg_season <- as.numeric(all_seasons_data$above_avg_stats >= 5)
# all_seasons_data$olympic_team <- ifelse(all_seasons_data$olympic_year == 1, 
#                                         substr(all_seasons_data$slugSeason, 1, 4), NA)

## olympic years only dataset
olympic_seasons <- rs %>% filter(olympic_year == 1)
olympic_seasons$above_avg_stats <- rowSums(olympic_seasons[, 62:69], na.rm = TRUE)
olympic_seasons$above_avg_season <- as.numeric(olympic_seasons$above_avg_stats >= 5)
# olympic_seasons$olympic_team <- ifelse(olympic_seasons$olympic_year == 1,
#                                         substr(olympic_seasons$slugSeason, 1, 4), NA)

## loop for overall understanding of Olympic year performance vs career averages 
## columns to loop through 
stat_values <- names(olympic_seasons)[62:69]
for (sv in 1:length(stat_values)){
  stat <- stat_values[sv]
  df <- table(olympic_seasons[,stat]) %>% data.frame()
  numerator <- df %>% filter(Var1 == 1) %>% select(Freq)
  denominator <- df %>% summarise(sum(Freq))
  pct <- round(numerator / denominator * 100, 1) %>% select(Freq) %>% unique()
  print(paste(pct
        ,'% of the time players have above average '
        ,stringr::str_to_upper(stat)
        ,' after the Olympic Games.'
        ,sep = ''))
  rm(pct)
}

df <- table(olympic_seasons$above_avg_season) %>% data.frame()
numerator <- df %>% filter(Var1 == 1) %>% select(Freq)
denominator <- df %>% summarise(sum(Freq))
pct <- round(numerator / denominator * 100, 1) %>% select(Freq) %>% unique()
print(paste(pct
            ,'% of the time players have an above average season overall after the Olympic Games.', 
            sep = ''))

## season stats
season <- rs %>% 
  select(namePlayer, slugSeason, olympic_year, olympic_team, slugTeam,
         season_pts, season_rbs, season_asts, season_2p_pct, season_3p_pct, season_fg_pct, season_ftr, season_mpg) %>% 
  gather(key = 'stat_name', 'stat_value', -namePlayer, -slugSeason, -olympic_year, -olympic_team, -slugTeam) %>% 
  arrange(namePlayer)
## career stats
career <- rs %>% 
  select(namePlayer, slugSeason, olympic_year, olympic_team, slugTeam,
         career_pts, career_rbs, career_asts, career_2p_pct, career_3p_pct, career_fg_pct, career_ftr, career_mpg) %>% 
  gather(key = 'stat_name_c', 'stat_value_c', -namePlayer, -slugSeason, -olympic_year, -olympic_team, -slugTeam) %>% 
  arrange(namePlayer)
## stat comparison
stat_comparison <- suppressWarnings(season %>% 
  cbind(stat_value_c = career$stat_value_c) %>% 
  mutate(above_avg_stat = as.numeric(stat_value >= stat_value_c), 
         stat2 = stat_name) %>% 
  # separate(col = stat2, into = c('x', NA), sep = '_')
  separate(stat2, c("stat1", "stat2"), sep = "[_]") %>% 
  select(-stat1) %>% 
  rename(statistic = stat2))

## create stat comparisons for olympic years (totals) vs career averages
olympic_year_stat_comparisons <- all_seasons_data %>% 
  filter(olympic_year == 1) %>% 
  group_by(namePlayer) %>% 
  summarise(ptsTotals = sum(ptsTotals, na.rm = TRUE), 
            trebTotals = sum(trebTotals, na.rm = TRUE),
            astTotals = sum(astTotals, na.rm = TRUE), 
            fg2aTotals = sum(fg2aTotals, na.rm = TRUE), 
            fg2mTotals = sum(fg2mTotals, na.rm = TRUE), 
            fg3aTotals = sum(fg3aTotals, na.rm = TRUE), 
            fg3mTotals = sum(fg3mTotals, na.rm = TRUE), 
            fgaTotals = sum(fgaTotals, na.rm = TRUE), 
            fgmTotals = sum(fgmTotals, na.rm = TRUE),
            ftaTotals = sum(ftaTotals, na.rm = TRUE), 
            ftmTotals = sum(ftmTotals, na.rm = TRUE), 
            minutesTotals = sum(minutesTotals, na.rm = TRUE), 
            gp = sum(gp, na.rm = TRUE)) %>% 
  mutate(season_pts = ptsTotals / gp, 
         season_rbs = trebTotals / gp, 
         season_asts = astTotals / gp, 
         season_2p_pct = fg2mTotals / fg2aTotals, 
         season_3p_pct = fg3mTotals / fg3aTotals, 
         season_fg_pct = fgmTotals / fgaTotals,
         season_ftr = ftaTotals / fgaTotals, 
         season_mpg = minutesTotals / gp) %>% 
  arrange(namePlayer) %>% 
  select(namePlayer, season_pts:season_mpg) %>% 
  data.frame()

## player career averages
player_careers <- all_seasons_data %>% 
  select(namePlayer, career_pts, career_rbs, career_asts, 
         career_2p_pct, career_3p_pct, career_fg_pct, 
         career_ftr, career_mpg) %>% distinct() 

## add player career averages to olympic_year_stat_comparisons
olympic_year_stat_comparisons <- suppressWarnings(olympic_year_stat_comparisons %>% 
                                                    merge(player_careers, by = 'namePlayer') %>% 
                                                    gather(key = 'stat_name', value = 'stat_value', -namePlayer) %>% 
                                                    separate(stat_name, c('time', 'stat'), sep = '_') %>% 
                                                    spread(time, stat_value) %>% 
                                                    mutate(better_after_olympics = as.numeric(season > career)) %>% 
                                                  merge(olympic_teams %>% group_by(player) %>% 
                                                          summarise(olympic_teams = toString(year)), 
                                                        by.x = 'namePlayer', by.y = 'player'))

## write output files for tableau
fwrite(stat_comparison, "Desktop/stat_comparison.csv")
fwrite(all_seasons_data, "Desktop/all_seasons_data.csv")
fwrite(olympic_seasons, "Desktop/olympic_seasons_data.csv")
fwrite(olympic_year_stat_comparisons, "Desktop/olympic_year_stat_comparisons.csv")

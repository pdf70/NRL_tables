# Filename: "NRL_tables.R"

# Reads in data from wikipedia of history of all NSWRL, ARL and NRL tables (Men's).
# Includes NSWRFL/NSWRL from 1908 to 1994, ARL 1995 to 1997, Super League in 1997, and NRL from 1998.
# Note that the format of the input data may change as people change wikipedia entries

# Team colours sourced from https://sportsfancovers.com/national-rugby-league-color-codes/
# or from https://imagecolorpicker.com/en.

# Retrieve previous work from:
setwd(output_path) 
load(file = "nrl_tables_raw.Rdata")   # list - "tables"
load(file="nrl_tables.Rdata")         # data frame 


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Libraries & directories

# Read in data files
path = "C:/Users/fallo/OneDrive/Documents/Pete/R-files"
input_path = paste(path, "/Input", sep="")
output_path = paste(path, "/R_output", sep="")
setwd(path)

# Specify packages (libraries) that are used
library(dplyr)
library(lubridate)
library(tidyverse)
library(scales)
library(janitor)
library(rvest)    # Reading tables from a web page


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Parameters 

# From 1908 to 2023
seasons = c(seq(1908, 1996, by = 1), rep(1997,2), seq(1998, 2023, by = 1))

no_teams_finals = c(rep(4,2), rep(2,2), rep(0,14),  # no finals except play-offs 1912-25
                    rep(4,47), rep(5,22), rep(8,2), 7, 5, 10, rep(8,25))
no_teams_finals[which(seasons == 1937)] = 0

super_league_ind = c(rep(0,90), 1, rep(0,length(seasons)-91))

# Note: need to update this line each year to value of table number in wikipedia for latest season
wiki_table_no = c(rep(1,47), rep(2,8), rep(1,8), rep(2,11), rep(1,3), rep(2,10), rep(1,7), rep(2,5), 4, rep(1,7),
                  rep(3,10))
wiki_table_no[which(seasons %in% c(1958, 1974, 1980, 1992, 1993, 2004))] = 1
wiki_table_no[which(seasons %in% c(1943, 1952, 1966, 1969, 1996, 2008, 2011, 2013))] = 2

wiki_name = c(rep("_NSWRFL_season",76), rep("_NSWRL_season",11), rep("_ARL_season",3),
              "_Super_League_(Australia)_season", rep("_NRL_season",26))

wiki_table_format = c(1, rep(0,11), rep(1, 10), rep(0,5), rep(1, 3), rep(0,46), rep(1,4), 
                      rep(0,12), 1, rep(0,2), rep(1,22))  # 0 - no bye, 1 = with bye


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Functions 
make_graph = function(team_abbrev) {
  data_for_graph = nrl_tables %>% 
    filter(abbrev == team_abbrev)
  
  max_teams_in_season = max(data_for_graph$count_teams)
  start_yr = min(data_for_graph$season)
  end_yr = max(data_for_graph$season)
  min_yr = min(data_for_graph$yr_end)
  max_yr = max(data_for_graph$yr_end)
  discont_yr = ifelse(team_abbrev %in% c("SOU", "MAN"), 1999, 2099)
  graph_range = max_yr - min_yr
  
  team_name = data_for_graph$team_full_name[1]
  sl_team = sum(data_for_graph$super_league_ind)
  
  #Breaks for background rectangles, other formatting
  # Update these values whenever the no. of teams in the league changes
  rects = data.frame(xstart = c(-Inf, 1909.5, 1919.5, 1929.5, 1934.5, 1937.5, 1946.5, 1966.5, 1981.5, 1983.5, 1987.5, 1994.5, 1996.5, 1997.5, 1998.5, 1999.5, 2001.5, 2006.5, 2022.5), 
                     xend = c(1909.5, 1919.5, 1929.5, 1934.5, 1937.5, 1946.5, 1966.5, 1981.5, 1983.5, 1987.5, 1994.5, 1996.5, 1997.5, 1998.5, 1999.5, 2001.5, 2006.5, 2022.5, Inf),
                     ystart = c(rep(20,19)), 
                     yend = c(9, 8, 9, 8, 9, 8, 10, 12, 14, 13, 16, 20, 12 - 2 * sl_team, 20, 17, 14, 15, 16, 17))
  x_intercepts = data_for_graph$yr_end[(data_for_graph$yr_end %% ifelse(graph_range > 60, 10, 5)) == 0]
  x_intercepts = x_intercepts[!(x_intercepts ==max_yr)]
  
  # Graph of league position
  graph_1 = ggplot(data_for_graph, aes(x = yr_end, y = Pos, group=yr_end<=discont_yr)) +
    geom_line(linewidth=1.15, colour = data_for_graph$team_colours[1]) +
    geom_point(aes(colour=as.factor(premiers), size = as.factor(premiers))) +
    scale_colour_manual(values = c(data_for_graph$second_colour[1], data_for_graph$prem_colour[1])) +
    scale_size_manual(values = c(2,4)) +
    
    # axes
    geom_rect(data = rects, aes(xmin = xstart, xmax = xend, ymin = Inf, ymax = yend+0.1),  # 0.1 for margin
              fill = "white", alpha = 1.0, inherit.aes = FALSE) +
    scale_y_continuous(trans = "reverse", expand = c(0,0.1), breaks= pretty_breaks()) +
    scale_x_continuous(breaks= pretty_breaks()) +
    coord_cartesian(xlim = c(min_yr, max_yr), ylim = c(max_teams_in_season, 1)) +
    geom_vline(xintercept=x_intercepts,  linetype="dotted") +
    theme(panel.border = element_rect(fill=NA)) +
    
    # titles
    ggtitle(paste("Position of", team_name, "in 1st Grade from", start_yr, "to", end_yr)) +
    theme(plot.title = element_text(lineheight=1.0, face="bold", hjust = 0.5)) +
    labs(x="Year", y="Position") +
    theme(axis.title = element_text(face = "bold")) +
    theme(plot.margin=unit(c(0.5,1,1.5,1.2),"cm")) +
    theme(legend.position = "none") +
    
    # horizontal lines for number of finals teams (approximated as 4 in years pre-1925)
    {if(min_yr<1973)geom_segment(aes(x = min(yr_end), xend = min(max_yr,1972.5), y = 4.5, yend = 4.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<1973)&(max_yr>1973))geom_segment(aes(x = 1972.5, xend = 1972.5, y = 4.5, yend = 5.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if(max_yr>1973)geom_segment(aes(x = 1972.5, xend = 1994.5, y = 5.5, yend = 5.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if((min_yr<1995)&(max_yr>1995))geom_segment(aes(x = 1994.5, xend = 1994.5, y = 5.5, yend = 8.5), linetype="dotted", colour = "black", linewidth = 1)} +
    {if(max_yr>1995)geom_segment(aes(x = max(1994.5,min_yr), xend = max(yr_end), y = 8.5, yend = 8.5), linetype="dotted", colour = "black", linewidth = 1)}

  graph_1
}

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Read in input files
setwd(input_path)
rugby_league_teams = read_csv("rugby_league_teams.csv") %>%
  mutate(team_full_name = paste(current_name, nickname)) %>%
  select(current_name:nickname, team_full_name, abbrev:team_url_women)

# read all league tables in one loop
# to read a league table manually, see code at end, e.g. read_html("https://en.wikipedia.org/wiki/1921_NSWRFL_season")
tables = list()

for (j in 1:91) {
  table = read_html(paste("https://en.wikipedia.org/wiki/", seasons[j], wiki_name[j], sep = ""))
  tables_wiki <- table %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)
  
  tables[[j]] <- tables_wiki[[wiki_table_no[j]]]
  colnames(tables[[j]])[1] = "Pos"

  tables[[j]] <- tables[[j]] %>% # added to my list
    mutate(season_no = j, season = seasons[j])

  if (j%%5==0) print(paste("season = ", seasons[j])) 
}

# Different code for NRL seasons - with wikipedia data in template form
for (j in 92:103) {
  table = read_html(paste("https://en.wikipedia.org/wiki/", seasons[j], wiki_name[j], sep = ""))
  tables_wiki <- table %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)
  
  tables[[j]] <- tables_wiki[[wiki_table_no[j]]] %>%
    row_to_names(1)  %>%
    select(Pos:Pts) %>%
    mutate(season_no = j, season = seasons[j])
  
  if (j%%5==0) print(paste("season = ", seasons[j])) 
}

for (j in 104:length(seasons)) {
  table = read_html(paste("https://en.wikipedia.org/wiki/", seasons[j], wiki_name[j], sep = ""))
  tables_wiki <- table %>%
    html_nodes(".wikitable") %>%
    html_table(fill = TRUE)
  
  tables[[j]] <- tables_wiki[[wiki_table_no[j]]] %>%
    row_to_names(1) %>%
    mutate(season_no = j, season = seasons[j])
  
  if (j%%5==0) print(paste("season = ", seasons[j])) 
}


# Review headers in each of the tables - need consistency of names for combining tables
headers_all = c()
for (j in which(wiki_table_format == 0)) {
  header_fmt1 = colnames(tables[[j]])
  headers_all = rbind(header_fmt1, headers_all)
}
for (j in which(wiki_table_format == 1)) {
  header_fmt2 = colnames(tables[[j]])
  headers_all = rbind(header_fmt2, headers_all)
}

for (j in 1:length(seasons)) {  
  colnames(tables[[j]]) = header_fmt1
}
for (j in which(wiki_table_format == 1)) {
  colnames(tables[[j]]) = header_fmt2              # exception - seasons with byes
}

# convert from list to data frame
tables_all_fmt1 = do.call(rbind, lapply(tables[wiki_table_format == 0], as.data.frame))
tables_all_fmt2 = do.call(rbind, lapply(tables[wiki_table_format == 1], as.data.frame))

tables_all_fmt1_adj = tables_all_fmt1 %>%
  mutate(B = 0) %>%
  select(Pos:L, B, PF:season)

tables_all = rbind(tables_all_fmt1_adj, tables_all_fmt2) %>%
  arrange(season_no, Pos)

# read in premiers
table_premiers_clean = read_html("https://en.wikipedia.org/wiki/Australian_rugby_league_premiers") %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)

table_other = table_premiers_clean[[1]] %>%
  select(c(Season:Premiers, "Runners-up")) %>%
  filter(Season %in% seasons) %>%
  rename("Runners_up" = "Runners-up")
  

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Select relevant data, and then data manipulations
# Firstly clean the team names - some are entered as the team name and some with the nickname as well
nrl_tables_adj_names = tables_all %>% 
  mutate(Pts = str_replace(Pts, "\\*", ""),            
         across(c(Pos, Pld:PA, Pts), as.numeric),
         Pts = ifelse(Pts > 75, round(Pts/10,0), Pts),
         Pts = ifelse(Team == "Melbourne Storm" & season == 2010, 0, Pts),
         Team = str_replace(Team, "\\[.*\\]", ""),            # remove text inside square brackets
         Team = str_replace(Team, " \\(.*\\)", ""),           # remove text inside brackets
         current_name = Team) %>%
  mutate(current_name = case_when(                            # to get consistency of team names
    current_name == "Norths" ~ "North Sydney",
    current_name == "Souths" ~ "South Sydney",
    current_name == "Wests" ~ "Western Suburbs",
    current_name == "University" ~ "Sydney University",
    current_name == "St George" ~ "St. George",
    current_name == "St George Illawarra Dragons" ~ "St. George Illawarra Dragons",
    grepl("Canterbury", current_name) ~ "Canterbury-Bankstown",
    current_name == "Sydney Bulldogs" ~ "Canterbury-Bankstown Bulldogs",
    current_name == "Manly" ~ "Manly-Warringah",
    grepl("Manly Warringah", current_name) ~ "Manly-Warringah",
    current_name == "Manly Sea Eagles" ~ "Manly-Warringah Sea Eagles",
    grepl("Cronulla", current_name) ~ "Cronulla-Sutherland",
    current_name == "Gold Coast-Tweed Giants" ~ "Gold Coast Chargers",
    current_name == "Gold Coast Seagulls" ~ "Gold Coast Chargers",
    current_name == "Easts" ~ "Sydney Roosters",
    grepl("Eastern Suburbs", current_name) ~ "Sydney Roosters",
    grepl("Sydney City", current_name) ~ "Sydney Roosters",
    current_name == "Sydney Tigers" ~ "Balmain Tigers",
    current_name == "Western Reds" ~ "Perth Reds",
    grepl("Auckland", current_name) ~ "New Zealand",
    current_name == "Dolphins" ~ "NRL Dolphins",
    TRUE ~ current_name)) %>%
  # First join where the raw data is just the team name
  left_join(rugby_league_teams, by = c("current_name" = "current_name")) %>%
  select(Pos:current_name, abbrev) %>%
  # secondly where the raw data is just the team name & the nickname
  left_join(rugby_league_teams, by = c("current_name" = "team_full_name")) %>%
  mutate(abbrev = ifelse(!(is.na(abbrev.x)), abbrev.x, abbrev.y)) %>%
  select(Pos:current_name, abbrev) %>%
  # lastly, join on abbrev picked from the previous joins
  left_join(rugby_league_teams, by = c("abbrev" = "abbrev")) %>%
  arrange(season_no, Pos) %>%
  rename("current_name" = "current_name.x",
         "club" = "current_name.y")

nrl_tables_name_join_fails = nrl_tables_adj_names %>%
  filter(is.na(abbrev))


# Repeat data cleansing for team name in premiers list
table_other_adj = table_other %>% 
  mutate(Premiers = str_replace(Premiers, "\\(.*?\\)", ""),
         Premiers = trimws(Premiers),
         Premiers = str_replace_all(Premiers, "[:digit:]", ""),
         premiers_clean = Premiers,
         Runners_up = str_replace(Runners_up, "\\(.*?\\)", ""),
         Runners_up = trimws(Runners_up)) %>%
  distinct() %>%
  mutate(Premiers = case_when(
    grepl("Bulldogs", Premiers) ~ "Canterbury-Bankstown Bulldogs",
    grepl("Eastern Suburbs", Premiers) ~ "Sydney Roosters",
    TRUE ~ Premiers)) %>%
  # First join where the raw data is just the team name
  left_join(rugby_league_teams, by = c("Premiers" = "current_name")) %>%
  select(Season:premiers_clean, abbrev) %>%
  # secondly where the raw data is just the team name & the nickname
  left_join(rugby_league_teams, by = c("Premiers" = "team_full_name")) %>%
  mutate(abbrev = ifelse(!(is.na(abbrev.x)), abbrev.x, abbrev.y)) %>%
  select(Season:premiers_clean, abbrev) %>%
  # lastly, join on abbrev picked from the previous joins
  left_join(rugby_league_teams, by = c("abbrev" = "abbrev")) %>%
  select(Season, premiers_clean, Premiers, team_full_name, Runners_up) %>%
  rename("premiers_full_name" = "team_full_name") %>%
  mutate(Runners_up = case_when(
    Runners_up == "Cronulla Sharks" ~ "Cronulla-Sutherland Sharks",
    Runners_up == "Canterbury Bulldogs" ~ "Canterbury-Bankstown Bulldogs",
    Runners_up == "Eastern Suburbs" ~ "Sydney Roosters",
    Runners_up == "Eastern Suburbs Roosters" ~ "Sydney Roosters",
    TRUE ~ Runners_up)) %>%
  # Repeat joins for runners-up
  # First join where the raw data is just the team name
  left_join(rugby_league_teams, by = c("Runners_up" = "current_name")) %>%
  select(Season:Runners_up, abbrev) %>%
  # secondly where the raw data is just the team name & the nickname
  left_join(rugby_league_teams, by = c("Runners_up" = "team_full_name")) %>%
  mutate(abbrev = ifelse(!(is.na(abbrev.x)), abbrev.x, abbrev.y)) %>%
  select(Season:Runners_up, abbrev) %>%
  # lastly, join on abbrev picked from the previous joins
  left_join(rugby_league_teams, by = c("abbrev" = "abbrev")) %>%
  select(Season:Runners_up, team_full_name) %>%
  rename("runners_up_full_name" = "team_full_name") %>%
  mutate(Season = as.numeric(Season), 
         season_no = seq(1, length(seasons), by = 1), super_league_ind, no_teams_finals)

nrl_premiers_name_join_fails = table_other_adj %>%
  filter(is.na(premiers_full_name))

nrl_runners_up_name_join_fails = table_other_adj %>%
  filter(is.na(runners_up_full_name))

nrl_tables_all = nrl_tables_adj_names %>% 
  filter(!(team_full_name == "Newcastle Knights" & season <= 1909)) %>%
  filter(!(team_full_name == "Newcastle Rebels" & season >= 1989)) %>%
  filter(!(team_full_name == "Gold Coast Titans" & season <= 1998)) %>%
  filter(!(team_full_name == "Gold Coast Chargers" & season >= 2006)) %>%
  left_join(table_other_adj, by = c("season" = "Season",
                                    "season_no" = "season_no")) %>%
  mutate(premiers = ifelse(team_full_name == premiers_full_name, 1, 0),
         premiers = ifelse(team_full_name == "Melbourne Storm" & season %in% c(2007, 2009), 0, premiers),
         runners_up = ifelse(team_full_name == runners_up_full_name, 1, 0),
         minor_premiers = ifelse(Pos == 1, 1, 0),
         minor_premiers = ifelse(team_full_name == "Melbourne Storm" & season %in% c(2006, 2007, 2008), 0, minor_premiers),
         finals = ifelse(Pos <= no_teams_finals, 1, 0),
         finals = case_when(                               # i.e. made finals through the play-offs
           Pos == 4 & season %in% c(1947, 1956, 1958, 1960) ~ 0,
           Pos == 5 & season %in% c(1947, 1956, 1958, 1960) ~ 1,
           Pos == 5 & season %in% c(1975, 1988, 1989, 1991) ~ 0,
           Pos == 6 & season %in% c(1975, 1988, 1989, 1991) ~ 1,
           TRUE ~ finals),
         Pts = as.numeric(str_replace(Pts, "\\*|\\[.*\\]", "")),
         pts_deducted = as.numeric(Pts) - (2 * W + D + 2 * B),
         pts_forfeit = ifelse(season == 1996 & pts_deducted == 2, 2, 0),
         pts_deducted = pts_deducted - pts_forfeit,
         max_avail_pts = (Pld + B) * 2,
         pts_achieved_perc = Pts / max_avail_pts,
         points_diff = PF - PA,
         PD_prefix = substr(PD,1,1),
         PD_sign = case_when(
           PD_prefix == "+" ~ 1,
           PD_prefix == "-" ~ -1,
           PD_prefix == "0" ~ 1,
           TRUE ~ -1),
         PD_numeric_raw = as.numeric(PD),
         PD_numeric = ifelse(!is.na(PD_numeric_raw), PD_numeric_raw, as.numeric(substr(PD,2,nchar(PD))) * PD_sign),
         PD_check = PD_numeric - points_diff, 
         yr_end = as.numeric(season)) %>%
  group_by(season_no) %>%
  mutate(count_teams = n(),
         wooden_spoon = ifelse(Pos == max(Pos), 1, 0)) %>%
  ungroup() %>%
  select(Pos:finals, count_teams:wooden_spoon, pts_deducted:yr_end)

# Add additional information of previous season's finishing position
nrl_tables = nrl_tables_all %>%
  arrange(team_full_name, season_no) %>%
  mutate(prev_pos = ifelse(team_full_name == lag(team_full_name), lag(Pos), NA)) %>%
  mutate(next_pos = ifelse(team_full_name == lead(team_full_name), lead(Pos), NA)) %>%
  arrange(season_no, Pos) %>%
  mutate(pos_diff = ifelse(is.na(prev_pos), NA, -(Pos - prev_pos)),
         pos_abs_diff = abs(pos_diff)) %>%
  group_by(team_full_name) %>%
  mutate(cum_premiers = cumsum(premiers),
         cum_minor_premiers = cumsum(minor_premiers),
         cum_finals = cumsum(finals),
         streak_finals = c(ave(c(0, finals), cumsum(c(0, finals) == 0), FUN = seq_along) - 1)[-1],
         streak_missed_finals = c(ave(c(0, finals), cumsum(c(0, finals) > 0), FUN = seq_along) - 1)[-1]) %>%
  ungroup()

rm("nrl_tables_all")

# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Analysis of NSWRL / ARL / NRL tables data
# Make all-time league table
nrl_all_time = group_by(nrl_tables, team_full_name) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_B = sum(B),
            Total_forf = sum(pts_forfeit),
            Total_Ded = sum(pts_deducted),
            Total_PF = sum(PF),
            Total_PA = sum(PA),
            Total_PD = sum(points_diff),
            Total_Pts = sum(Pts),
            win_perc = round(Total_W / Total_Pld * 100, 2),
            result_perc = round((Total_W + 0.5 * Total_D) / Total_Pld * 100, 2),
            count_premiers = sum(premiers),
            count_runners_up = sum(runners_up),
            count_minor_premiers = sum(minor_premiers),
            count_finals = sum(finals),
            count_1st = sum(Pos == 1),
            count_2nd = sum(Pos == 2),
            count_3rd = sum(Pos == 3),
            best = min(Pos),
            count_spoon = sum(wooden_spoon),
            first_season = min(season),
            last_season = max(season)) %>%
  arrange(desc(Total_Pts), desc(Total_PD), desc(Total_PF))
names(nrl_all_time) <- gsub(x = names(nrl_all_time), pattern = "_", replacement = " ")

# premiers by final position
premiers = filter(nrl_tables, premiers == 1)
premiers_by_Pos = group_by(premiers, Pos) %>%
  summarise(count = n())

# totals by season
season_totals = group_by(nrl_tables, season, yr_end) %>%
  summarise(count = n(),
            Total_Pld = sum(Pld),
            Total_W = sum(W),
            Total_D = sum(D),
            Total_L = sum(L),
            Total_B = sum(B),
            Total_PF = sum(PF),
            Total_PA = sum(PA),
            Total_PD = sum(points_diff),
            Total_Pts = sum(Pts))

title_race_totals = group_by(nrl_tables, season, yr_end) %>%
  summarise(count = n(),
            Total_Pts_1 = sum(Pts[Pos == 1]),
            Total_Pts_2 = sum(Pts[Pos == 2]),
            Total_PD_1 = sum(points_diff[Pos == 1]),
            Total_PD_2 = sum(points_diff[Pos == 2]),
            Total_PF_1 = sum(PF[Pos == 1]),
            Total_PF_2 = sum(PF[Pos == 2])) %>%
  mutate(margin_pts = Total_Pts_1 - Total_Pts_2,
         margin_PD = Total_PD_1 - Total_PD_2,
         margin_PF = Total_PF_1 - Total_PF_2)

# Records for a single season - not adjusted for no. of games
# most & least points
most_pts_season = arrange(nrl_tables, desc(Pts)) %>%
  select(season, team_full_name, Pld, Pts)
head(most_pts_season, 5)

least_pts_season = arrange(nrl_tables, Pts) %>%
  select(season, team_full_name, Pld, Pts)
head(least_pts_season, 5)

# most & least wins
most_wins_season = arrange(nrl_tables, desc(W)) %>%
  select(season, Team, Pld, W)
head(most_wins_season, 5)

least_wins_season = arrange(nrl_tables, W) %>%
  select(season, team_full_name, Pld, W)
head(least_wins_season, 5)

# most & least losses
most_losses_season = arrange(nrl_tables, desc(L)) %>%
  select(season, team_full_name, Pld, L)
head(most_losses_season, 5)

least_losses_season = arrange(nrl_tables, L) %>%
  select(season, team_full_name, Pld, L)
head(least_losses_season, 5)

# most & least draws
most_draws_season = arrange(nrl_tables, desc(D)) %>%
  select(season, team_full_name, Pld, D)
head(most_draws_season, 5)

# most & least points scored
most_points_season = arrange(nrl_tables, desc(PF)) %>%
  select(season, team_full_name, Pld, PF)
head(most_points_season, 5)

least_points_season = arrange(nrl_tables, PF) %>%
  select(season, team_full_name, Pld, PF)
head(least_points_season, 5)

# most & least points conceded
most_points_against_season = arrange(nrl_tables, desc(PA)) %>%
  select(season, team_full_name, Pld, PA)
head(most_points_against_season, 5)

least_points_against_season = arrange(nrl_tables, PA) %>%
  select(season, team_full_name, Pld, PA)
head(least_points_against_season, 5)

# best & worst points difference
best_points_diff_season = arrange(nrl_tables, desc(points_diff)) %>%
  select(season, team_full_name, Pld, points_diff)
head(best_points_diff_season, 5)

worst_points_diff_season = arrange(nrl_tables, points_diff) %>%
  select(season, team_full_name, Pld, points_diff)
head(worst_points_diff_season, 5)

# most points to not be minor premiers
most_pts_not_minor_premiers_season = arrange(nrl_tables, desc(Pts)) %>%
  filter(minor_premiers == 0) %>%
  select(season, team_full_name, Pld, Pts) 
head(most_pts_not_minor_premiers_season, 5)

# least points to be minor premiers
least_pts_minor_premiers_season = arrange(nrl_tables, Pts) %>%
  filter(minor_premiers == 1) %>%
  select(season, team_full_name, Pld, Pts)
head(least_pts_minor_premiers_season, 5)

# biggest & smallest winning margin in league
most_winning_margin_season = title_race_totals %>%
  arrange(desc(margin_pts), desc(margin_PD), desc(margin_PF)) %>%
  left_join(nrl_tables, by = c("season" = "season")) %>%
  filter(Pos == 1 & minor_premiers == 1) %>%
  select(season, team_full_name, margin_pts, margin_PD, margin_PF)
head(most_winning_margin_season, 5)

least_winning_margin_season = title_race_totals %>%
  arrange(margin_pts, margin_PD, margin_PF) %>%
  left_join(nrl_tables, by = c("season" = "season")) %>%
  filter(Pos == 1 & minor_premiers == 1) %>%
  select(season, team_full_name, margin_pts, margin_PD, margin_PF)
head(least_winning_margin_season, 5)


# highest & lowest points achieved percentage
highest_pts_perc_season = arrange(nrl_tables, desc(pts_achieved_perc)) %>%
  select(season, team_full_name, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(highest_pts_perc_season, 5)

lowest_pts_perc_season = arrange(nrl_tables, pts_achieved_perc) %>%
  select(season, team_full_name, Pld, Pts, max_avail_pts, pts_achieved_perc)
head(lowest_pts_perc_season, 5)


# highest movement in final position
highest_mvmt_up_season = arrange(nrl_tables, desc(pos_diff)) %>%
  select(season, team_full_name, Pos, prev_pos, pos_diff)
head(highest_mvmt_up_season, 5)

highest_mvmt_down_season = arrange(nrl_tables, pos_diff) %>%
  select(season, team_full_name, Pos, prev_pos, pos_diff)
head(highest_mvmt_down_season, 5)


# lowest position to premiers in one season
prev_pos_premiers = nrl_tables %>%
  filter(premiers == 1) %>%
  select(season, team_full_name, prev_pos) %>%
  arrange(desc(prev_pos), season)
head(prev_pos_premiers, 5)

# lowest position after being premiers in one season
next_pos_premiers = nrl_tables %>%
  filter(premiers == 1) %>%
  select(season, team_full_name, next_pos) %>%
  arrange(desc(next_pos), season)
head(next_pos_premiers, 5)


# volatility of position from year to year
pos_changes = nrl_tables %>%
  group_by(current_name) %>%
  summarise(count_seasons = n(),
            total_pos_diff = sum(pos_abs_diff, na.rm = TRUE)) %>%
  mutate(ave_mvmt = total_pos_diff / (count_seasons - 1)) %>%
  arrange(desc(ave_mvmt))
pos_changes


# Longest streaks
longest_streaks_finals = arrange(nrl_tables, desc(streak_finals)) %>%
  select(season, team_full_name, streak_finals)
head(longest_streaks_finals, 5)

longest_streaks_missed_finals = arrange(nrl_tables, desc(streak_missed_finals)) %>%
  select(season, team_full_name, streak_missed_finals)
head(longest_streaks_missed_finals, 5)


# list of all team abbreviations
teams_unique = unique(nrl_tables$abbrev)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# checks on data for consistency
error_check_pts = nrl_tables %>% 
  filter(!Pts == (2 * W + D + 2 * B))

error_check_pld = nrl_tables %>%
  filter(!Pld == (W + D + L))

error_check_results = season_totals %>%
  filter(!Total_W == Total_L)

error_check_pd_season = season_totals %>%
  filter(!Total_PD == 0)

error_check_pd = nrl_tables %>%
  filter(!(PD_check == 0))

error_check_pos = group_by(nrl_tables, season_no, season) %>%
  summarise(count = n(),
            sum_pos = sum(Pos)) %>%
  mutate(exp_sum_pos = count * (count + 1) / 2,
         pos_diff = sum_pos - exp_sum_pos) %>%   # error if calculated difference (pos_diff) is not zero
  filter(!(pos_diff == 0))

error_check_teams = nrl_tables_name_join_fails

error_check_premiers = nrl_premiers_name_join_fails

error_check_runners_up = nrl_runners_up_name_join_fails


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# run function to produce graph for a specific team
make_graph("ANN") 
make_graph("BAL")
make_graph("CBY")
#make_graph("CUM")    Cumberland - 1 season only
make_graph("SYD")
make_graph("GLE") 
make_graph("REB")
make_graph("JET")
make_graph("NOR")
make_graph("SOU")
make_graph("STG") 
make_graph("UNI")
make_graph("WES")
make_graph("MAN")
make_graph("PAR")
make_graph("CRO")
make_graph("PEN")
make_graph("CAN")
make_graph("ILA")
make_graph("BRI")
make_graph("GCC")
make_graph("NEW")
make_graph("NZL")
make_graph("NQL")
make_graph("SQC")
make_graph("PER")
make_graph("ADE")
#make_graph("HUN")    Hunter Mariners - 1 season only
make_graph("MEL")
make_graph("SGI")
make_graph("WTI")
make_graph("NTE")
make_graph("GCT")
make_graph("DOL")


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# export file to csv format
setwd(output_path)
save(tables, file = "nrl_tables_raw.Rdata")
save(nrl_tables, file = "nrl_tables.Rdata")
write.csv(nrl_tables, file = "nrl_tables_full.csv")
write.csv(nrl_all_time, file = "nrl_all_time.csv")
setwd(path) 

# export single graph or data frame
setwd(output_path)
write.csv(error_check_pts, file = "error_check_pts.csv")
ggsave("graph_ggsave.pdf")
setwd(path)

# export multiple graphs
for (i in 1:length(teams_unique)) {
  make_graph(teams_unique[i])
  setwd(output_path)
  #  ggsave(paste("graph_nrl_", teams_unique[i], ".pdf", sep=""))
  ggsave(paste("performance_chart_nrl_", teams_unique[i], ".png", sep=""))
  ggsave(paste("performance_chart_nrl_", teams_unique[i], ".svg", sep=""))
}
setwd(path)


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# End


# To do:
# Validate wikipedia amounts - compare to Rugby League Project


# - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - -
# Test
j = 1
seasons[j]
wiki_name[j]
wiki_table_no[j]
# read one league table manually
table = read_html("https://en.wikipedia.org/wiki/1981_NSWRFL_season")
#table = read_html("https://en.wikipedia.org/wiki/1989_NSWRL_season")
tables_all <- table %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)
table_yyyymm = tables_all[[2]]
table_yyyymm


table = read_html("https://en.wikipedia.org/wiki/2010_NRL_season")
tables_all <- table %>%
  html_nodes(".wikitable") %>%
  html_table(fill = TRUE)
table_yyyymm = tables_all[[1]] %>%
  row_to_names(1) %>%
  clean_names()
#  select(Pos:Pts)
table_yyyymm


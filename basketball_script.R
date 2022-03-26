#Loading Packages
library(rvest) #Scrapes Websites
library(dplyr) #Data Wranglings
library(ggplot2) #Nice Graphs
library(stringr) #Allows Us to Use Strings

#Reading HTML Data from Elon Mens Basketballl ESPN Schedule Page
schedule_url <- read_html("https://www.espn.com/mens-college-basketball/team/schedule/_/id/2210") 

#Reading in Data About Elon Basketball Schedule
eu_schedule <- schedule_url %>% 
  html_node("table") %>% 
  html_table(fill = TRUE)

#Finding All Unique GameIDs on Elon's Basketball Schedule
game_ids <- schedule_url %>% 
  str_extract_all('(?<=gameId/)(.*?)(?=")') %>%
  unlist() %>% 
  unique()

#Practicing By Harvesting Data From First Game (will be foundation of Dataset)
#Reading HTML Data from first Game (Florida)
team_stats_url <- read_html("https://www.espn.com/mens-college-basketball/matchup?gameId=401371839")

#Taking Game Stats From First Game
eu_team_stats_fl <- team_stats_url %>% 
 html_nodes("table") %>%
 html_table(fill = TRUE)
#Cleanest Team Stat Data From First Game
eu_team_stats_fl <- eu_team_stats_fl[[2]]

#Taking Elon Team Data
elon_stats_fl <- eu_team_stats_fl[2]
elon_stats_fl <- t(elon_stats_fl) #transposes Data for later
elon_stats_fl <- data.frame(as.list(elon_stats_fl)) #converts list to data frame
names(elon_stats_fl) <- c("FG", "FG%", "3PT", "3PT%", "FT", "FT%", "RB", "ORB", "DRB", "TRB", "ASSISTS", "STEALS", "BLOCKS", "TOS", "FOULS", "TFOULS", "FFOULS", "LEAD")
#^Renames Columns

#Taking Opponent Data, same process detailed above
opp_stats_fl <- eu_team_stats_fl[3]
opp_stats_fl <- t(opp_stats_fl)
opp_stats_fl <- data.frame(as.list(opp_stats_fl))
names(opp_stats_fl) <- c("OPP FG", "OPP FG%", "OPP 3PT", "OPP 3PT%", "OPP FT", "OPP FT%", "OPP RB", "OPP ORB", "OPP DRB", "OPP TRB", "OPP ASSISTS", "OPP STEALS", "OPP BLOCKS", "OPP TOS", "OPP FOULS", "OPP TFOULS", "OPP FFOULS", "OPP LEAD")



#Base URLs for each game
url <- "https://www.espn.com/mens-college-basketball/matchup?gameId="
#Creates a List with Game ID code using paste() Function
game_urls <- paste(url, game_ids, sep = "")


#Initializes Data frames that will be added to
data_home <- elon_stats_fl
data_opp <- opp_stats_fl

#iterates through every game data (except game 1)
for (x in game_urls[2:length(game_urls)]){
  #iterates through game 2 through 14 (can change to what value you want)
  #Reads HTML data and uses the second table
  team_stats <- x %>% 
     read_html()%>%
     html_nodes("table") %>%
     html_table(fill = TRUE)
  #CATCH ERROR IN WHAT SIDE
  if (team_stats[[1]][1,1] == "ELON"){
    eu_team_stats <- team_stats[[2]]
    
    #Does same thing as before, transposing and making new data set
    elon_stats <- eu_team_stats[2]
    elon_stats <- t(elon_stats)
    elon_stats <- data.frame(as.list(elon_stats))
    names(elon_stats) <- c("FG", "FG%", "3PT", "3PT%", "FT", "FT%", "RB", "ORB", "DRB", "TRB", "ASSISTS", "STEALS", "BLOCKS", "TOS", "FOULS", "TFOULS", "FFOULS", "LEAD")
    #Binds data set created in loop to data_home data set
    data_home <- rbind(data_home, elon_stats)
    
    #Does same thing but for opponents
    opp_stats <- eu_team_stats[3]
    opp_stats <- t(opp_stats)
    opp_stats <- data.frame(as.list(opp_stats))
    names(opp_stats) <- c("OPP FG", "OPP FG%", "OPP 3PT", "OPP 3PT%", "OPP FT", "OPP FT%", "OPP RB", "OPP ORB", "OPP DRB", "OPP TRB", "OPP ASSISTS", "OPP STEALS", "OPP BLOCKS", "OPP TOS", "OPP FOULS", "OPP TFOULS", "OPP FFOULS", "OPP LEAD")
    data_opp <- rbind(data_opp, opp_stats)
  }
  else{
    eu_team_stats <- team_stats[[2]]
    
    #Does same thing as before, transposing and making new data set
    elon_stats <- eu_team_stats[3]
    elon_stats <- t(elon_stats)
    elon_stats <- data.frame(as.list(elon_stats))
    names(elon_stats) <- c("FG", "FG%", "3PT", "3PT%", "FT", "FT%", "RB", "ORB", "DRB", "TRB", "ASSISTS", "STEALS", "BLOCKS", "TOS", "FOULS", "TFOULS", "FFOULS", "LEAD")
    #Binds data set created in loop to data_home data set
    data_home <- rbind(data_home, elon_stats)
    
    #Does same thing but for opponents
    opp_stats <- eu_team_stats[2]
    opp_stats <- t(opp_stats)
    opp_stats <- data.frame(as.list(opp_stats))
    names(opp_stats) <- c("OPP FG", "OPP FG%", "OPP 3PT", "OPP 3PT%", "OPP FT", "OPP FT%", "OPP RB", "OPP ORB", "OPP DRB", "OPP TRB", "OPP ASSISTS", "OPP STEALS", "OPP BLOCKS", "OPP TOS", "OPP FOULS", "OPP TFOULS", "OPP FFOULS", "OPP LEAD")
    data_opp <- rbind(data_opp, opp_stats)
  }
  
  
  
  
  
}

#Cleaning Elon (home) data
data_home$FG <- sub("-.*" , "", data_home[,1])
data_home$`3PT` <- sub("-.*" , "", data_home[,3])
data_home$FT <- sub("-.*" , "", data_home[,5])

#Cleaning Opponent Data
data_opp$`OPP FG` <- sub("-.*" , "", data_opp[,1])
data_opp$`OPP 3PT` <- sub("-.*" , "", data_opp[,3])
data_opp$`OPP FT` <- sub("-.*" , "", data_opp[,5])

#Mutate Variables in data_home to Numeric
data_home$FG <- as.numeric(data_home$FG)
data_home$`FG%` <- as.numeric(data_home$`FG%`)
data_home$`3PT` <- as.numeric(data_home$`3PT`)
data_home$`3PT%` <- as.numeric(data_home$`3PT%`)
data_home$FT <- as.numeric(data_home$FT)
data_home$`FT%` <- as.numeric(data_home$`FT%`)
data_home$RB <- as.numeric(data_home$RB)
data_home$ORB <- as.numeric(data_home$ORB)
data_home$DRB <- as.numeric(data_home$DRB)
data_home$TRB <- as.numeric(data_home$TRB)
data_home$ASSISTS <- as.numeric(data_home$ASSISTS)
data_home$STEALS <- as.numeric(data_home$STEALS)
data_home$BLOCKS <- as.numeric(data_home$BLOCKS)
data_home$TOS <- as.numeric(data_home$TOS)
data_home$FOULS <- as.numeric(data_home$FOULS)
data_home$TFOULS <- as.numeric(data_home$TFOULS)
data_home$FFOULS <- as.numeric(data_home$FFOULS)
data_home$LEAD <- as.numeric(data_home$LEAD)

#Mutate variables in opp_home to Numeric
data_opp$`OPP FG` <- as.numeric(data_opp$`OPP FG`)
data_opp$`OPP FG%` <- as.numeric(data_opp$`OPP FG%`)
data_opp$`OPP 3PT` <- as.numeric(data_opp$`OPP 3PT`)
data_opp$`OPP 3PT%` <- as.numeric(data_opp$`OPP 3PT%`)
data_opp$`OPP FT` <- as.numeric(data_opp$`OPP FT`)
data_opp$`OPP FT%` <- as.numeric(data_opp$`OPP FT%`)
data_opp$`OPP RB` <- as.numeric(data_opp$`OPP RB`)
data_opp$`OPP ORB` <- as.numeric(data_opp$`OPP ORB`)
data_opp$`OPP DRB` <- as.numeric(data_opp$`OPP DRB`)
data_opp$`OPP TRB` <- as.numeric(data_opp$`OPP TRB`)
data_opp$`OPP ASSISTS` <- as.numeric(data_opp$`OPP ASSISTS`)
data_opp$`OPP STEALS` <- as.numeric(data_opp$`OPP STEALS`)
data_opp$`OPP BLOCKS` <- as.numeric(data_opp$`OPP BLOCKS`)
data_opp$`OPP TOS` <- as.numeric(data_opp$`OPP TOS`)
data_opp$`OPP FOULS` <- as.numeric(data_opp$`OPP FOULS`)
data_opp$`OPP TFOULS` <- as.numeric(data_opp$`OPP TFOULS`)
data_opp$`OPP FFOULS` <- as.numeric(data_opp$`OPP FFOULS`)
data_opp$`OPP LEAD` <- as.numeric(data_opp$`OPP LEAD`)

#Mutating More Variables, such as 2 Pointers made and points scores
data_home <- data_home %>%
  mutate(`2FG` = FG - `3PT`)%>%
  mutate(POINTS = FT + (`3PT` * 3) + (`2FG` * 2))
data_opp <- data_opp %>%
  mutate(`OPP 2FG` = `OPP FG` - `OPP 3PT`)%>%
  mutate(`OPP POINTS` = `OPP FT` + (`OPP 3PT` * 3) + (`OPP 2FG` * 2))

total_data <- cbind(data_home, data_opp)

total_data <- total_data %>%
  mutate(WIN = if_else(POINTS > `OPP POINTS`, TRUE, FALSE), DIFF = POINTS - `OPP POINTS`)
         

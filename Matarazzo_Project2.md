Project \#2
================
Rocco Matarazzo

# Welcome to my API Vignette!

Hello! This vignette is aimed at interacting with an API. Here we will
interact with an MLB API. We will delve into multiple endpoints as well
as some Exploratory Data Analysis (EDA).

# Required Packages

The packages needed to create this markdown/GitHub post include
tidyverse, rmarkdown, ggplot2, and jsonlite. The main package that
interacts with the API itself is jsonlite. The former three aid in the
function creation for this document, the document itself, and the EDA.

# API Interaction Functions

Here are some API interactions.

**40-Man Roster for the 2023 Season**

This first interaction will return a Major League Baseball’s team’s
40-man roster for the 2023 season. Here we can specify which year the
season occurred in, as well as the team of interest. The team object
should be entered in full format i.e. “Philadelphia Phillies” rather
than “Phillies” or “Philadelphia.”

``` r
FortyManRoster <- function(team){
 
 ### This function is built to return a team's 40-man roster for the 2023 MLB season ###
 ### Inputs:
 ### team -- a Major League Baseball team's full display name, i.e. "Philadelphia Phillies"
 ### This function hits two API endpoints: The 'team_all_season' endpoint and the 'roster_40' endpoint

# First we grab team-related information from the API for the given season
initialRead <- 
  GET(paste0("http://lookup-service-prod.mlb.com/json/named.team_all_season.bam?sport_code='mlb'&all_star_sw='N'&season=2023"))

# We parse that data and get the data.frame object of interest
parsedJSON <- fromJSON(rawToChar(initialRead$content))
allTeams <- parsedJSON$team_all_season$queryResults$row

# Here we select the individual teamId for the team of interest from the allTeams data.frame
teamId <- allTeams %>% filter(name_display_long == team) %>% select(team_id)

# We plug the teamId back into the API to return their 40-man roster
team <- GET(paste0("http://lookup-service-prod.mlb.com/json/named.roster_40.bam?team_id='",teamId,"'"))

# We parse the data, and ultimately return the 40-man roster
parsedTeam <- fromJSON(rawToChar(team$content)) 
FortyManFull <- parsedTeam$roster_40$queryResults$row

# Here we select columns of interest and rename them
# We also convert the d.f. into a tibble
as_tibble(FortyManFull %>% select(Team = team_name,
                        FullName = name_full,
                        FirstName = name_first,
                        LastName = name_last,
                        Position = position_txt,
                        Bats = bats,
                        Throws = throws,
                        Number = jersey_number,
                        PlayerID = player_id))
}
```

**All Players In a Season** This function will return every player who
was on a MLB roster for a given season. I originally had this function
wrapped within the Player Statisitcs function below. However, I figured
it was smart to make it separate.

``` r
LeagueRosters <- function(year){

 ### This function is built to return each MLB player in a given season ###
 ### Inputs:
 ### year -- a season of interest, i.e. 2023
 ### This function hits two API endpoints: The 'team_all_season' endpoint and the 'roster_team_alltime' endpoint

# First we grab team-related information from the API for the given season
initialRead <- 
  GET(paste0("http://lookup-service-prod.mlb.com/json/named.team_all_season.bam?sport_code='mlb'&all_star_sw='N'&season=",year))

# We parse that data and get the data.frame object of interest
parsedJSON <- fromJSON(rawToChar(initialRead$content))
allTeams <- parsedJSON$team_all_season$queryResults$row
      
# Here we select each teamId and put it in a data frame
allTeamIds <- allTeams %>% select(team_id)
  
# We set a base object to be null
df <- NULL

# We create an inner-function called "rosterFunction" that we can eventually call for each team
    for(i in nrow(allTeamIds)){

     rosterFunction <- function(i){
       # We first get the JSON output for the given year for each team id
             team <- GET(paste0(
            "http://lookup-service-prod.mlb.com/json/named.roster_team_alltime.bam?start_season='"
            , year,"'&end_season='"
            , year,"'&team_id='"
            ,i,"'"))
            # We parse the data, and ultimately return the roster for that season
            parsedTeam <- fromJSON(rawToChar(team$content)) 
            RosterOuput <- parsedTeam$roster_team_alltime$queryResults$row
            # Here we select columns of interest and rename them
            roster <- RosterOuput %>% select(TeamId = team_id,
                                  FullName = name_last_first,
                                  Position = primary_position,
                                  PosGroup = position_desig,
                                  Bats = bats,
                                  Throws = throws,
                                  Number = jersey_number,
                                  PlayerID = player_id)
            # Finally we return the roster
          return(roster)}
    }
          
# Here we use do.call and apply to call the function
# This will run the rosterFunction above on each row in allTeamIds (which is a 30x1 d.f. with each team id)
# and ultimately rbind each output together.
AllPlayers <-  do.call(rbind,
                       apply(allTeamIds,
                             1, # applying the function on EACH row
                             rosterFunction)
)

# Here we cerate the final tibble
# We join back to the allTeams d.f. to map teamId to teamName 
FinalPlayers <- as_tibble(AllPlayers) %>% inner_join(allTeams %>% 
               select(name_display_long, team_id),
               by = join_by("TeamId" == "team_id")) %>%
  rename("TeamName" = "name_display_long")

return(FinalPlayers)
}
```

**Player Batting Statistics** This function will take a player and
return their batting statistics for a given season.

``` r
playerName <- "Harper, Bryce"
year <- 2021
gameType <- "R"

playerBattingStatistics <- function(playerName, year){
 
 ### This function is built to return a player's statistics during a certain game type in a season ###
 ### Inputs:
 ### playerName -- a Major League Baseball player's full name in Last, First format, i.e. "Harper, Bryce"
 ### team -- a Major League Baseball team's full display name, i.e. "Philadelphia Phillies"
 ### Ex. playerStatistics("Arozarena, Randy", 2022) will return Arozarena's statistics
 ### during the 2022 regular season. 

# First we call our LeagueRosters function to get all players for that year
AllPlayers  <- LeagueRosters(year)
# Then we filter down to our playerID of interest
PlayerId <- AllPlayers %>% filter(FullName ==  playerName) %>% select(PlayerID)
# Here is the final extraction step for the playerID
PlayerIdFinal <- PlayerId$PlayerID
         
# Here we read in the JSON from the hitting stats endpoint
statsRead <- 
  GET(paste0("http://lookup-service-prod.mlb.com/json/named.sport_hitting_tm.bam?league_list_id='mlb'&game_type='R'&season='"
      , year,"'&player_id='"
      , PlayerIdFinal, "'"))

# We parse that data and get the data.frame object of interest
statsParsed <- fromJSON(rawToChar(statsRead$content))
# We bind all of the relevant hitting statistics together 
statsBind <- cbind(
   Season =  as.numeric(statsParsed$sport_hitting_tm$queryResults$row$season),
   League = statsParsed$sport_hitting_tm$queryResults$row$league,
   Team = statsParsed$sport_hitting_tm$queryResults$row$team_full,
   PlayerId =  as.numeric(statsParsed$sport_hitting_tm$queryResults$row$player_id),
   Games = as.numeric(statsParsed$sport_hitting_tm$queryResults$row$g),
   PA = as.numeric(statsParsed$sport_hitting_tm$queryResults$row$tpa),
   HR = as.numeric(statsParsed$sport_hitting_tm$queryResults$row$hr),
   RBI = as.numeric(statsParsed$sport_hitting_tm$queryResults$row$rbi),
   AVG =  as.numeric(statsParsed$sport_hitting_tm$queryResults$row$avg),
   OBP = as.numeric(statsParsed$sport_hitting_tm$queryResults$row$obp),
   SLG = as.numeric(statsParsed$sport_hitting_tm$queryResults$row$slg),
   OPS = as.numeric(statsParsed$sport_hitting_tm$queryResults$row$ops),
   BABIP = as.numeric(statsParsed$sport_hitting_tm$queryResults$row$babip),
   SB = as.numeric(statsParsed$sport_hitting_tm$queryResults$row$sb),
   BB = as.numeric(statsParsed$sport_hitting_tm$queryResults$row$bb)
)
# We build the binded stats as a tibble, and also add the player's name
StatsTibble <- as_tibble(statsBind) %>% 
  mutate(PlayerName = playerName) %>% select(PlayerName, everything())

# Finally we return the stats tibble
return(StatsTibble)
}
```

**Player Pitching Statistics** This function will take a player and
return their pitching statistics for a given season.

``` r
playerName <- "Eflin, Zach"
year <- 2023
gameType <- "R"

playerPitchingStatistics <- function(playerName, year){
 
 ### This function is built to return a player's statistics during a certain game type in a season ###
 ### Inputs:
 ### playerName -- a Major League Baseball player's full name in Last, First format, i.e. "Kershaw, Clayton"
 ### team -- a Major League Baseball team's full display name, i.e. "Philadelphia Phillies"
 ### Ex. playerStatistics("Arozarena, Randy", 2022) will return Arozarena's statistics
 ### during the 2022 regular season. 

# First we call our LeagueRosters function to get all players for that year
AllPlayers  <- LeagueRosters(year)
# Then we filter down to our playerID of interest
PlayerId <- AllPlayers %>% filter(FullName ==  playerName) %>% select(PlayerID)
# Here is the final extraction step for the playerID
PlayerIdFinal <- PlayerId$PlayerID
         
# Here we read in the JSON from the pitching stats endpoint
statsReadPitching <- 
   GET(paste0("http://lookup-service-prod.mlb.com/json/named.sport_pitching_tm.bam?league_list_id='mlb'&game_type='R'&season='"
      , year,"'&player_id='"
      , PlayerIdFinal, "'"))

# We parse that data and get the data.frame object of interest
statsParsed <- fromJSON(rawToChar(statsReadPitching$content))
# We bind all of the relevant pitching statistics together 
statsBind <- cbind(
  Season = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$season),
  League = statsParsed$sport_pitching_tm$queryResults$row$league,
  TeamName = statsParsed$sport_pitching_tm$queryResults$row$team_full,
  Wins = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$w),
  Losses = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$l),
  Games = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$g),
  GamesStarted = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$gs),
  IP = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$ip),
  ERA = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$era),
  WHIP = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$whip),
  SOper9 = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$k9),
  HitPer9 = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$h9),
  CG = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$cg),
  SHO = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$sho),
  SV = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$sv),
  SVO = as.numeric(statsParsed$sport_pitching_tm$queryResults$row$svo)
)
# We build the binded stats as a tibble, and also add the player's name
StatsTibble <- as_tibble(statsBind) %>% 
  mutate(PlayerName = playerName) %>% select(PlayerName, everything())

# Finally we return the stats tibble
return(StatsTibble)
}
```

**Projected Statistics**

# Creating .md File

``` r
# 
# rmarkdown::render("~/repos/558_project2/Matarazzo_Project.rmd", 
#               output_options = list(
#                 name_value_pairs = "value", 
#                 toc = TRUE,
#                 toc_depth = 2,
#                 number_sections = TRUE,
#                 df_print = "tibble",
#                 or_something = TRUE
#                 ),
#                 output_file = "Matarazzo_Output"
#               )
```

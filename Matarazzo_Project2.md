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
40-man roster in any given season. Here we can specify which year the
season occurred in, as well as the team of interest. The team object
should be entered in full format i.e. “Philadelphia Phillies” rather
than “Phillies” or “Philadelphia.”

``` r
FortyManRoser <- function(team){
 
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

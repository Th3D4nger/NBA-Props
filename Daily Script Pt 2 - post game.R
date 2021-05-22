library(tidyverse)
library(stringr)
library(stringi)
library(dplyr)
library(jsonlite)
library(furrr)
library(future)
library(vroom)
library(httr)
library(hablar)
library(janitor)
library(scales)
library(odds.converter)
library(readxl)
library(nbastatR)


##### Daily Box Scores, via nbastatsR package

game_logs <- game_logs(seasons = 2021, league='NBA', result_types = 'team')

#game_logs <- game_logs(seasons = 2021,league="NBA",season_types="Playoffs",result_types = ("team"))



##### Change date of game logs #####
#stodaysgames <- subset(game_logs, subset=(game_logs$dateGame=="2021-05-15"), select=(c("dateGame","slugTeam","slugOpponent","idTeam","idGame","isB2B",
#                                                                                      "isB2BFirst","isB2BSecond","countDaysRestTeam")))

ids <- "0052000201"
#ids <- list(todaysgames$idGame)
ids2 <- ids
#ids2 <- unlist(ids)

bxsc2 <- box_scores(game_ids = ids2, box_score_types = c("Traditional"), result_types = c("player"), join_data = TRUE, assign_to_environment = FALSE, return_message = TRUE)

bxscr <- as.data.frame(bxsc2$dataBoxScore[1])

bxscr2 <- left_join(bxscr, game_logs,by=c("idGame","idTeam"))

bxscr2$Player <- sub(".*? ", "", bxscr2$namePlayer)

bxscr2 <- bxscr2 %>% 
  rename(
    Team = slugTeam.x,
    '3s' = fg3m,
    Assists = ast,
    Rebounds = treb,
    Points = pts,
    Opponent = slugOpponent
  )

bxscrday <- subset(bxscr2, select=c("Player","Team","Opponent","dateGame","Points","Assists",
                                    "Rebounds","3s","isB2B","isB2BFirst","isB2BSecond","countDaysRestTeam"))

bxscrday$Date <- as.Date(bxscrday$dateGame)

bxscrday <- bxscrday[!duplicated(bxscrday$Player), ]

dailyscrs <- left_join(daily,bxscrday, by="Player")

# Calculating overs and unders

dailyscrs$Difference <- ifelse(dailyscrs$Stat=="Points",dailyscrs$Line-dailyscrs$Points,
                               ifelse(dailyscrs$Stat=="Assists",dailyscrs$Line-dailyscrs$Assists,
                                      ifelse(dailyscrs$Stat=="Rebounds",dailyscrs$Line-dailyscrs$Rebounds,
                                             ifelse(dailyscrs$Stat=="3s",dailyscrs$Line-dailyscrs$'3s',NA))))
dailyscrs$Result <- ifelse(dailyscrs$Difference>0,"Under","Over") 

dailyscrs <- dailyscrs %>% 
  rename(
    Date = Date.x,
    Team = Team.x
  )

dailyscrs$Date.y <- NULL
dailyscrs$Team.y <- NULL
dailyscrs$dateGame <- NULL
dailyscrs$Opponent <- NULL


##### Change Date in CSV #####
write.csv(dailyscrs, "props and scores, 05.20.2021.csv")



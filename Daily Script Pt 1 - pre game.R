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

##### Put File Here #####

rawprops <- read_excel("Daily copied props and lines.xlsx", sheet = "05.21.2021")

str_right <- function(string, n) {
  substr(rawprops$Mess, nchar(rawprops$Mess) - (n - 1), nchar(rawprops$Mess))
}

str_left <- function(string, n) {
  substr(string, 1, n)
}

### Extracting Decimal odds from the messy shit

rawprops$odds <- str_right(rawprops$Mess, 8)

rawprops$Oodds <- str_left(rawprops$odds,4)
rawprops$Uodds <- str_right(rawprops$odds,4)

rawprops$Line <- str_sub(rawprops$Mess,1,nchar(rawprops$Mess)-8)

rawprops$Player1 <-  rawprops$Player

rawprops$Player <- substring(rawprops$Player1, 4)

rawprops$team1 <- str_sub(rawprops$Game,1,nchar(rawprops$Game)-6)
rawprops$team2 <- str_sub(rawprops$Game,7,nchar(rawprops$Game)-0)


vars <- c("Player","team1","team2","Stat","Oodds","Uodds","Line")

props <- rawprops[vars]


#Convert odds, just in case I fucked up

#props$Oodds <- as.numeric(props$Oodds)
#props$Oodds <- odds.us2dec(props$Oodds)
#props$Uodds <- as.numeric(props$Uodds)
#props$Uodds <- odds.us2dec(props$Uodds)

#Bringing in the NBA stats data

headers <- c(
  `Connection` = 'keep-alive',
  `Accept` = 'application/json, text/plain, */*',
  `x-nba-stats-token` = 'true',
  `X-NewRelic-ID` = 'VQECWF5UChAHUlNTBwgBVw==',
  `User-Agent` = 'Mozilla/5.0 (Macintosh; Intel Mac OS X 10_14_6) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/78.0.3904.87 Safari/537.36',
  `x-nba-stats-origin` = 'stats',
  `Sec-Fetch-Site` = 'same-origin',
  `Sec-Fetch-Mode` = 'cors',
  `Referer` = 'https://stats.nba.com/players/shooting/',
  `Accept-Encoding` = 'gzip, deflate, br',
  `Accept-Language` = 'en-US,en;q=0.9'
)

# Can adjust last N games, I keep it at 10
url <- "https://stats.nba.com/stats/leaguedashplayerstats?College=&Conference=&Country=&DateFrom=&DateTo=&Division=&DraftPick=&DraftYear=&GameScope=&GameSegment=&Height=&LastNGames=10&LeagueID=00&Location=&MeasureType=Base&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=&TwoWay=0&VsConference=&VsDivision=&Weight="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
l10 <- data.frame(json_resp$resultSets$rowSet)

colnames(l10) <- json_resp[["resultSets"]][["headers"]][[1]]  

# scratch for later
teamcodes <- subset(l10, select=c("TEAM_ID","TEAM_ABBREVIATION"))
teamcodes <- unique(teamcodes)
teamcodes$TEAM_ABBREVIATION <- ifelse(teamcodes$TEAM_ABBREVIATION=="PHX","PHO",teamcodes$TEAM_ABBREVIATION)
teamcodes$TEAM_ABBREVIATION <- ifelse(teamcodes$TEAM_ABBREVIATION=="UTH","UTA",teamcodes$TEAM_ABBREVIATION)


l10$Points <- l10$PTS
l10$Rebounds <- l10$REB
l10$Assists <- l10$AST
l10$'3s' <- l10$FG3M
l10$Player <- l10$PLAYER_NAME
l10$Team <- l10$TEAM_ABBREVIATION
l10$Team <- ifelse(l10$Team=="PHX","PHO",l10$Team)


l10 <- subset(l10, select=c("Player","Team","Points","Assists","Rebounds","3s"))

last10 <-  gather(l10, "Stat","l10",3:6)

vars <- c("Player","Team","Stat","l10")
last10 <- last10[vars]

last10$Player <- sub(".*? ", "", last10$Player)


last10$joiner <- paste(last10$Player,last10$Stat,sep = "-")

props$joiner <- paste(props$Player,props$Stat, sep ="-")



join1 <- left_join(props, last10, by = "joiner")

join1$TEAM <- ifelse(join1$Team==join1$team1|join1$Team==join1$team2,join1$Team,NA)

join1 <- join1[complete.cases(join1), ]

join1$Opp <- ifelse(join1$TEAM==join1$team1,join1$team2,join1$team1)

join1$Stat <- join1$Stat.x
join1$Team <- join1$TEAM
join1$Player <- join1$Player.x


daily <- subset(join1, select=c("Team","Opp","Player","Stat","Oodds","Uodds","Line","l10"))

daily$Line <- as.numeric(daily$Line)
daily$l10 <- as.numeric(daily$l10)

daily$l10diff <- daily$Line - daily$l10

#### Adding positions

pos <- read.csv("Cleaned positions per player.csv")

daily$Identifier <- paste(daily$Team,daily$Player,sep="-")

daily2 <- left_join(daily,pos,by="Identifier")

daily <- daily2 %>% 
  rename(
    Player = Player.x
  )
daily$X <- NULL
daily$Player.y <- NULL

###### Insert any Strategies here #####

#daily$bet <- ifelse(daily$Stat=="Assists"&daily$l10diff<=-0.79,"Over",
#                    ifelse(daily$Stat=="Assists"&daily$l10diff>=0.79,"Under",NA))

history <- read.csv("NBA Props Data - Sheet1.csv")
history$ResultOv <- ifelse(history$Result=="Over",1,0)

points.glm <- glm(ResultOv~l10diff, data=history, subset=(history$Stat=="Points"), family = binomial)
summary(points.glm)
rebounds.glm <- glm(ResultOv~l10diff, data=history, subset=(history$Stat=="Rebounds"), family = binomial)
summary(rebounds.glm)
assists.glm <- glm(ResultOv~l10diff, data=history, subset=(history$Stat=="Assists"), family = binomial)
summary(assists.glm)
threes.glm <- glm(ResultOv~l10diff, data=history, subset=(history$Stat=="3s"), family = binomial)
summary(threes.glm)

daily$imp.Ov.Prob <- odds.dec2prob(as.numeric(daily$Oodds))
daily$imp.Un.Prob <- odds.dec2prob(as.numeric(daily$Uodds))

daily$Ast.Ov.prob <- predict(assists.glm, newdata=daily, type="response")
daily$Ast.Ov.prob.Diff <- daily$Ast.Ov.prob - daily$imp.Ov.Prob

daily$Reb.Ov.prob <- predict(rebounds.glm, newdata=daily, type="response")
daily$Reb.Ov.prob.Diff <- daily$Reb.Ov.prob - daily$imp.Ov.Prob

daily$Threes.Ov.prob <- predict(threes.glm, newdata=daily, type="response")
daily$Threes.Ov.prob.Diff <- daily$Threes.Ov.prob - daily$imp.Ov.Prob

daily$Ov.pred <- ifelse(daily$Stat=="Assists",daily$Ast.Ov.prob,
                        ifelse(daily$Stat=="Rebounds",daily$Reb.Ov.prob,
                               ifelse(daily$Stat=="3s",daily$Threes.Ov.prob,NA)))
daily$Ov.P.Diff <- ifelse(daily$Stat=="Assists",daily$Ast.Ov.prob.Diff,
                        ifelse(daily$Stat=="Rebounds",daily$Reb.Ov.prob.Diff,
                               ifelse(daily$Stat=="3s",daily$Threes.Ov.prob.Diff,NA)))

daily$Ast.Ov.prob <- NULL
daily$Ast.Ov.prob.Diff <- NULL
daily$Reb.Ov.prob <- NULL
daily$Reb.Ov.prob.Diff <- NULL
daily$Threes.Ov.prob <- NULL
daily$Threes.Ov.prob.Diff <- NULL

daily$betrec <- ifelse(daily$Stat=="Assists" & daily$Ov.pred < 0.41 & daily$Ov.P.Diff< -0.1,"Under",
                          ifelse(daily$Stat=="Assists" & daily$Ov.pred > 0.59 & daily$Ov.P.Diff>0.1,"Over",
                                 ifelse(daily$Stat=="Rebounds" & daily$Ov.pred < 0.41 & daily$Ov.P.Diff< -0.1,"Under",
                                        ifelse(daily$Stat=="Rebounds" & daily$Ov.pred > 0.59 & daily$Ov.P.Diff>0.1,"Over", 
                                               ifelse(daily$Stat=="Threes" & daily$Ov.pred < 0.41 & daily$Ov.P.Diff< -0.1,"Under",
                                                      ifelse(daily$Stat=="Threes" & daily$Ov.pred > 0.59 & daily$Ov.P.Diff>0.1,"Over", NA))))))



### Adding opponent team metrics as controls for the past 10 games as well

url <- "https://stats.nba.com/stats/leaguedashteamstats?Conference=&DateFrom=&DateTo=&Division=&GameScope=&GameSegment=&LastNGames=10&LeagueID=00&Location=&MeasureType=Advanced&Month=0&OpponentTeamID=0&Outcome=&PORound=0&PaceAdjust=N&PerMode=PerGame&Period=0&PlayerExperience=&PlayerPosition=&PlusMinus=N&Rank=N&Season=2020-21&SeasonSegment=&SeasonType=Regular+Season&ShotClockRange=&StarterBench=&TeamID=0&TwoWay=0&VsConference=&VsDivision="

res <- GET(url = url, add_headers(.headers=headers))
json_resp <- fromJSON(content(res, "text"))
teaml10 <- data.frame(json_resp$resultSets$rowSet)

colnames(teaml10) <- json_resp[["resultSets"]][["headers"]][[1]]  

teaml10 <- full_join(teaml10,teamcodes,by="TEAM_ID")

teaml10 <- teaml10 %>% 
  rename(
    Opp = TEAM_ABBREVIATION
  )

teaml10$Opp <- ifelse(teaml10$Opp=="PHX","PHO",teaml10$Opp)


teaml10 <- teaml10 %>% relocate(Opp, .before = TEAM_ID)
teaml10$TEAM_ID <- NULL
teaml10$TEAM_NAME <- NULL
teaml10$CFID <- NULL
teaml10$CFPARAMS <- NULL

daily1 <- left_join(daily,teaml10,by="Opp")

daily <- daily1

####### Change this Date #####
daily$Date <- "2021-05-21"

daily <- daily %>% relocate(Date, .before = Team)


###### Change write to date #####

write.csv(daily, "05 21 2021 lines l10 and bets.csv")

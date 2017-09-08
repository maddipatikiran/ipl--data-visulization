setwd("C:/Users/Kiran/Desktop/IPL FINAL")

 deliveries<-read.csv("deliveries.csv",header=T,na.strings="")
matches<-read.csv("matches.csv",header=T,na.strings="")


library(ggplot2)
library(plotly) 
require(data.table)
require(sqldf)
require("plotrix")


#1)top batsman runs in all seasons

batsman_run <- aggregate(batsman_runs~batsman,data=deliveries,FUN = "sum")
top_batsman <-head(batsman_run[order(batsman_run$batsman_runs,decreasing=TRUE),],10)

top_batsman_graph<-ggplot(top_batsman, aes(batsman, batsman_runs,fill=batsman)) + geom_col()+ggtitle("Top Batsman Runs In All Seasons")
ggplotly(top_batsman_graph)


#2)most number of repeated umpire_1 in all seasons

no_of_umpire1<-aggregate(cbind(number_of_matches=umpire1)~umpire1,data=matches,FUN=function(x){NROW(x)})
top_umpire1<-head(no_of_umpire1[order(no_of_umpire1$number_of_matches,decreasing = TRUE),],10 )

u1_graph<-ggplot(top_umpire1, aes(umpire1,number_of_matches,fill=umpire1)) + geom_col()+ggtitle("Most number of repeated umpire_1 in all seasons")
ggplotly(u1_graph)


#3)most number of repeated umpire 2
no_of_umpire2<-aggregate(cbind(number_of_matches=umpire2)~umpire2,data=matches,FUN=function(x){NROW(x)})
top_umpire2<-head(no_of_umpire2[order(no_of_umpire2$number_of_matches,decreasing = TRUE),],10 )

u2_graph<-ggplot(top_umpire2, aes(umpire2,number_of_matches,fill=umpire2)) + geom_col()+ggtitle("Most number of repeated umpire_2 in all seasons")
ggplotly(u2_graph)


#4)top 4's hit by a batsman

foursdata <- subset(deliveries, batsman_runs == "4")
fours<-aggregate(cbind(No_of_Fours = batsman_runs)~batsman,data=foursdata,FUN=function(x){NROW(x)})
top_fours <- head(fours[order(fours$No_of_Fours,decreasing=TRUE),],10)

top_fours_graph<-ggplot(top_fours, aes(batsman,No_of_Fours,color=factor(batsman)))+geom_point()+ggtitle("Top 4's hit by a batsman")
ggplotly(top_fours_graph)


#5)top 6's hit by a batsman

sixdata <- subset(deliveries, batsman_runs == "6")
sixes<-aggregate(cbind(No_of_sixes = batsman_runs)~batsman,data=sixdata,FUN=function(x){NROW(x)})
top_sixes <- head(sixes[order(sixes$No_of_sixes,decreasing=TRUE),],10)

top_sixes_graph<-ggplot(top_sixes, aes(batsman,No_of_sixes,color=factor(batsman)))+geom_point()+ggtitle("Top 6's hit by a batsman")
ggplotly(top_sixes_graph)


#6)win by runs

win_by_run <-aggregate(win_by_runs~winner,data=matches,FUN=max)
top_winner_team <-head(win_by_run[order(win_by_run$win_by_runs,decreasing=TRUE),],10)

graph <- plot_ly(top_winner_team, labels = top_winner_team$winner, values = top_winner_team$win_by_runs, type = 'pie') %>%
  layout(title = 'Win by runs with percentages',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
graph


#7)win by wickets

win_by_wicket <-aggregate(win_by_wickets~winner,data=matches,FUN=max)
top_wicket_winner <-head(win_by_wicket[order(win_by_wicket$win_by_wickets,decreasing=TRUE),],10)

graph <- plot_ly(top_wicket_winner, labels = top_wicket_winner$winner, values = top_wicket_winner$win_by_wickets, type = 'pie') %>%
  layout(title = 'Win by wickets with percentages',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
graph


#8)highest number of catches by a fielder

catchesdata <- subset(deliveries, dismissal_kind =="caught")
field_kind<-aggregate(cbind(no_of_catches = dismissal_kind)~fielder,data=catchesdata,FUN=function(x){NROW(x)})
top_catches <- head(field_kind[order(field_kind$no_of_catches,decreasing=TRUE),],10)

plot_ly(labels = top_catches$fielder , values = top_catches$no_of_catches) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Highest catches by a fielder with percentage",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#9)no of matches  in every venue

library(htmltools)
require(ggmap)
library(shiny)
library(leaflet)

stadiumIcon <- makeIcon(
  iconUrl = "C:/Users/LENOVO/Documents/final_r_p/www/stad2.jpg",
  iconWidth = 15, iconHeight = 15,
  iconAnchorX = 10, iconAnchorY = 50
)

matches_in_each_venue <-aggregate(cbind(No_Of_Matches = venue)~venue,data=matches,FUN=function(x){NROW(x)})
venues<-data.frame(matches_in_each_venue$venue)

geo_location<-geocode(as.character(venues$matches_in_each_venue.venue))
venue_location<-cbind(matches_in_each_venue,geo_location)

leaflet(venue_location) %>% addTiles() %>%
  addMarkers(~lon, ~lat, popup = ~as.character(No_Of_Matches), label = ~htmlEscape(venue) ,icon=stadiumIcon)


#10)top 10 players who are awarded man-of-the matches

player_of_match<-aggregate(cbind(No_of_Matches=player_of_match)~player_of_match,data=matches,FUN=function(x){NROW(x)})
best_player<-head(player_of_match[order(player_of_match$No_of_Matches,decreasing = TRUE),],10)

graph<-ggplot(best_player, aes(player_of_match,No_of_Matches,color=factor(player_of_match)))+geom_point()+ggtitle("Top 10 players awarded with man of the match")
ggplotly(graph)


#11) no of the matches win by each team in all seasons

match_winner<-aggregate(cbind(No_of_Matches_won = winner)~winner,data=matches,FUN=function(x){NROW(x)})

plot_ly(labels = match_winner$winner , values = match_winner$No_of_Matches_won) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Top 10 winners in all seasons with percentage",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#12)toss winners who are final winners

toss_winner <- subset(matches, toss_winner == winner)
toss_final_winner<-aggregate(cbind(Final_Winners = winner)~toss_winner,data=toss_winner,FUN=function(x){NROW(x)})

graph <- plot_ly(toss_final_winner, labels = toss_final_winner$toss_winner, values = toss_final_winner$Final_Winners, type = 'pie') %>%
  layout(title = 'Toss winners who are final winners',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
graph


#13)no of matches  in every city

library(htmltools)
require(ggmap)
library(shiny)
library(leaflet)
require(ggplot2)

win_each_season <-aggregate(cbind(No_Of_Matches = city)~city,data=matches,FUN=function(x){NROW(x)})
cities<-data.frame(win_each_season$city)
geo_location<-geocode(as.character(cities$win_each_season.city))
city_location<-cbind(win_each_season,geo_location)

leaflet(city_location) %>% addTiles() %>%
  addMarkers(~lon, ~lat,popup = ~as.character(No_Of_Matches), label = ~htmlEscape(city))



#14)no of wins by a team in  seasons(2008:2016)*(in other file,top_10_teams.graph)

win_season<-aggregate(cbind(No_Of_Matches_Won =winner)~winner+season,data=matches,FUN=function(x){NROW(x)})
winner_season <-head(win_season[order((win_season$season),(win_season$winner),decreasing =TRUE ),],90)
count_winner<-sqldf("SELECT * from winner_season ORDER BY season,No_Of_Matches_Won DESC ")

graph<-ggplot(count_winner,aes(season,max(No_Of_Matches_Won),fill=winner))+geom_bar(stat="identity")+ggtitle("No of wins by a team in  seasons(2008:2016)")
ggplotly(graph)


#15)maximum win percentage by a team in each season

max_winner<-sqldf("SELECT  winner,season,max(No_Of_Matches_Won) as Matches_won from count_winner group by season")

graph<-ggplot(max_winner,aes(season,Matches_won,fill=winner))+geom_bar(stat="identity")+ggtitle("Maximum Wins By A Team In Each Season")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplotly(graph)


#16) no of winning matches when selecting toss field

toss_field <- subset(matches,toss_decision == "field")
field_win<-aggregate(cbind(no_of_matches =winner)~winner,data=toss_field,FUN=function(x){NROW(x)})

graph<-ggplot(field_win,aes(winner,no_of_matches,fill=winner))+geom_col()+ggtitle("NO OF WINNING MATCHES WHEN SELECTING TOSS AS FIELDING")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplotly(graph)


#17)no of winning matches when selecting toss bat

toss_bat <- subset(matches,toss_decision == "bat")
bat_win<-aggregate(cbind(no_of_matches =winner)~winner,data=toss_bat,FUN=function(x){NROW(x)})

bat_graph<-ggplot(field_win,aes(winner,no_of_matches,fill=winner))+geom_col()+ggtitle("NO OF WINNING MATCHES WHEN SELECTING TOSS AS BATTING")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplotly(bat_graph)


#18)top 10 bowlers in all seasons(in the next file)

dismissed<-sqldf("SELECT * from deliveries where dismissal_kind ='bowled' or dismissal_kind ='caught' or dismissal_kind ='caught and bowled' or dismissal_kind = 'hit wicket' or dismissal_kind = 'lbw' or dismissal_kind = 'stumped' ")
top_bowler<-subset(dismissed,select=c(dismissal_kind,bowler))
bowler_top<-aggregate(cbind(Wickets_Taken = dismissal_kind)~bowler,data=top_bowler,FUN=function(x){NROW(x)})
top_10_bowlers<- head(bowler_top[order(bowler_top$Wickets_Taken,decreasing=TRUE),],10)

plot_ly(labels = top_10_bowlers$bowler , values = top_10_bowlers$Wickets_Taken) %>%
  add_pie(hole = 0.6) %>%
  layout(title = "Top 10 bowlers in all seasons with percentages",  showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))


#19)Stadium in which a team won highest matches

#data3<-subset(matches,select=c(winner,venue))
venue1<-aggregate(cbind(count=winner)~winner+venue,data=matches,FUN=function(x){NROW(x)})
max_winner<-sqldf("SELECT  winner,venue,max(count) as maxc from venue1 group by winner")

graph<-ggplot(max_winner,aes(venue,maxc,fill=winner))+geom_bar(stat="identity")+ggtitle("Stadium in which a team won highest matches")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplotly(graph)


#20)number of times a batsman is dismissed by a bowler

dismissed<-sqldf("SELECT * from deliveries where dismissal_kind ='bowled' or dismissal_kind ='caught' or dismissal_kind ='caught and bowled' or dismissal_kind = 'hit wicket' or dismissal_kind = 'lbw' or dismissal_kind = 'stumped' ")
top_bowler<-subset(dismissed,select=c(dismissal_kind,bowler,player_dismissed))
bowler_top<-aggregate(cbind(Wickets_Taken = dismissal_kind)~bowler+player_dismissed,data=top_bowler,FUN=function(x){NROW(x)})
top_10_bowlers<- head(bowler_top[order(bowler_top$Wickets_Taken,decreasing=TRUE),],10)

graph<-ggplot(top_10_bowlers,aes(bowler,Wickets_Taken,fill=player_dismissed))+geom_bar(stat="identity")+ggtitle("Heighest no of times a batsman dismissed by a bowler")
ggplotly(graph)


#21)count of a person who acted both as umpire1,umpire2

umpire_12<-subset(matches,select=c(umpire1,umpire2))
common_umpire<-aggregate(cbind(count=umpire1)~umpire2,data=umpire_12,FUN=function(x){NROW(x)})

umpire_graph<-ggplot(common_umpire,aes(umpire2,count,fill=umpire2))+geom_col()+ggtitle("Count of a person who acted both as umpire1,umpire2")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplotly(umpire_graph)


#22)top 30 batsman who hit maximum runs in a match

season_list<-sqldf("SELECT id,season from matches")
colnames(season_list) <- c("match_id","season")
deliveries_season<- sqldf("SELECT * from deliveries left join season_list using(match_id)")
batsman_runs<- subset(deliveries_season,select=c(batsman,batsman_runs,season,match_id))
partnership<-aggregate(cbind(max_runs =batsman_runs)~batsman+season+match_id,data=batsman_runs,FUN="sum")
top_batsman_match<- head(partnership[order(partnership$max_runs,decreasing=TRUE),],30)

graph<-ggplot(top_batsman_match,aes(season,max_runs,fill=batsman))+geom_bar(stat="identity")+ggtitle("Top 30 batsmen who hit maximum runs in a match")
ggplotly(graph)


#23) top 30 partnership

#l<-sqldf("SELECT id,season from matches")
#colnames(l) <- c("match_id","season")
#am<- sqldf("SELECT * from deliveries left join l using(match_id)")
bat<- subset(deliveries_season,select=c(batsman,batsman_runs,season,match_id,non_striker))
partnership<-aggregate(cbind(max_runs =batsman_runs)~batsman+season+match_id+non_striker,data=bat,FUN="sum")
top_partnership<- head(partnership[order(partnership$max_runs,decreasing=TRUE),],30)

graph<-ggplot(top_partnership, aes(x=max_runs, y=non_striker, color=as.factor(batsman))) +ggtitle("Top 30 partnerships in all seasons")+
  geom_point() + 
  facet_grid(. ~ season)
ggplotly(graph)


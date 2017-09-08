deliveries<-read.csv("C:/Users/Kiran/Desktop/IPL FINAL/deliveries.csv",header=T,na.strings="")
matches<-read.csv("C:/Users/Kiran/Desktop/IPL FINAL/matches.csv",header=T,na.strings="")

  
library(ggplot2)
library(plotly) 
require(data.table)
require(sqldf)
require("plotrix")

# 1)no of matches  in every city

library(htmltools)
require(ggmap)
library(shiny)
library(leaflet)

win_each_season <-aggregate(cbind(No_Of_Matches = city)~city,data=matches,FUN=function(x){NROW(x)})
cities<-data.frame(win_each_season$city)
geo_location<-geocode(as.character(cities$win_each_season.city))
city_location<-cbind(win_each_season,geo_location)

leaflet(city_location) %>% addTiles() %>%
  addMarkers(~lon, ~lat,popup = ~as.character(No_Of_Matches), label = ~htmlEscape(city))


# 2)toss winners who are final winners

toss_winner <- subset(matches, toss_winner == winner)
toss_final_winner<-aggregate(cbind(Final_Winners = winner)~toss_winner,data=toss_winner,FUN=function(x){NROW(x)})

graph <- plot_ly(toss_final_winner, labels = toss_final_winner$toss_winner, values = toss_final_winner$Final_Winners, type = 'pie') %>%
  layout(title = 'Toss winners who are final winners',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
graph


# 3) Top 10 bowlers in each season

library(sqldf)
all_seasons<-sqldf("SELECT id,season from matches")
colnames(all_seasons) <- c("match_id","season")
deliverie_season<- sqldf("SELECT * from deliveries left join all_seasons using(match_id)")

dismissed<-sqldf("SELECT * from deliverie_season where dismissal_kind ='bowled' or dismissal_kind ='caught' or dismissal_kind ='caught and bowled' or dismissal_kind = 'hit wicket' or dismissal_kind = 'lbw' or dismissal_kind = 'stumped' ")
top_bowler<-subset(dismissed,select=c(dismissal_kind,bowler,season))
bowler_top<-aggregate(cbind(Wickets_Taken = dismissal_kind)~bowler+season,data=top_bowler,FUN=function(x){NROW(x)})

id<-c(1,2,3,4,5,6,7,8,9,10)

season1<-subset(bowler_top,season==2008)
top_10_s1<- head(season1[order(season1$Wickets_Taken,decreasing=TRUE),],10)
colnames(top_10_s1) <- c("bowler1","season1","Wickets_Taken1")
top_10_s1_id<-cbind(top_10_s1,id)


season2<-subset(bowler_top,season==2009)
top_10_s2<- head(season2[order(season2$Wickets_Taken,decreasing=TRUE),],10)
colnames(top_10_s2) <- c("bowler2","season2","Wickets_Taken2")
top_10_s2_id<-cbind(top_10_s2,id)


season3<-subset(bowler_top,season==2010)
top_10_s3<- head(season3[order(season3$Wickets_Taken,decreasing=TRUE),],10)
colnames(top_10_s3) <- c("bowler3","season3","Wickets_Taken3")
top_10_s3_id<-cbind(top_10_s3,id)


season4<-subset(bowler_top,season==2011)
top_10_s4<- head(season4[order(season4$Wickets_Taken,decreasing=TRUE),],10)
colnames(top_10_s4) <- c("bowler4","season4","Wickets_Taken4")
top_10_s4_id<-cbind(top_10_s4,id)


season5<-subset(bowler_top,season==2012)
top_10_s5<- head(season5[order(season5$Wickets_Taken,decreasing=TRUE),],10)
colnames(top_10_s5) <- c("bowler5","season5","Wickets_Taken5")
top_10_s5_id<-cbind(top_10_s5,id)


season6<-subset(bowler_top,season==2013)
top_10_s6<- head(season6[order(season6$Wickets_Taken,decreasing=TRUE),],10)
colnames(top_10_s6) <- c("bowler6","season6","Wickets_Taken6")
top_10_s6_id<-cbind(top_10_s6,id)


season7<-subset(bowler_top,season==2014)
top_10_s7<- head(season7[order(season7$Wickets_Taken,decreasing=TRUE),],10)
colnames(top_10_s7) <- c("bowler7","season7","Wickets_Taken7")
top_10_s7_id<-cbind(top_10_s7,id)


season8<-subset(bowler_top,season==2015)
top_10_s8<- head(season8[order(season8$Wickets_Taken,decreasing=TRUE),],10)
colnames(top_10_s8) <- c("bowler8","season8","Wickets_Taken8")
top_10_s8_id<-cbind(top_10_s8,id)


season9<-subset(bowler_top,season==2016)
top_10_s9<- head(season9[order( season9$Wickets_Taken,decreasing=TRUE),],10)
colnames(top_10_s9) <- c("bowler9","season9","Wickets_Taken9")
top_10_s9_id<-cbind(top_10_s9,id)


t1<-merge(top_10_s1_id,top_10_s2_id,by="id")
t2<-merge(top_10_s3_id,top_10_s4_id,by="id")
t3<-merge(top_10_s5_id,top_10_s6_id,by="id")
t4<-merge(top_10_s7_id,top_10_s8_id,by="id")
t5<-merge(t1,t2,by="id")
t6<-merge(t3,t4,by="id")
t7<-merge(t5,t6,by="id")
t8<-merge(t7,top_10_s9_id,by="id")


library(plotly)
library(dplyr) 

 plot_ly() %>%
  add_pie(data = count(t8, season1), labels = ~(t8$bowler1), values = ~(t8$Wickets_Taken1),name = "season1", domain = list(x = c(0, 0.2), y = c(0.8, 1))) %>%      
  add_pie(data = count(t8, season2), labels = ~(t8$bowler2), values = ~(t8$Wickets_Taken2),name = "season2", domain = list(x = c(0.4,0.6), y = c(0.8, 1))) %>%       
  add_pie(data = count(t8, season3), labels = ~(t8$bowler3), values = ~(t8$Wickets_Taken3),name = "season3", domain = list(x = c(0.8,1 ), y = c(0.8, 1))) %>%        
  add_pie(data = count(t8, season4), labels = ~(t8$bowler4), values = ~(t8$Wickets_Taken4),name = "season4", domain = list(x = c(0,0.2), y = c(0.4,0.6))) %>%       
  add_pie(data = count(t8, season5), labels = ~(t8$bowler5), values = ~(t8$Wickets_Taken5),name = "season5", domain = list(x = c(0.4,0.6), y = c(0.4, 0.6))) %>%       
  add_pie(data = count(t8, season6), labels = ~(t8$bowler6), values = ~(t8$Wickets_Taken6),name = "season6", domain = list(x = c(0.8, 1), y = c(0.4, 0.6))) %>%      
  add_pie(data = count(t8, season7), labels = ~(t8$bowler7), values = ~(t8$Wickets_Taken7),name = "season7", domain = list(x = c(0, 0.2), y = c(0, 0.2))) %>%     
  add_pie(data = count(t8, season8), labels = ~(t8$bowler8), values = ~(t8$Wickets_Taken8),name = "season8", domain = list(x = c(0.4,0.6), y = c(0, 0.2))) %>%     
  add_pie(data = count(t8, season9), labels = ~(t8$bowler9), values = ~(t8$Wickets_Taken9),name = "season9", domain = list(x = c(0.8,1), y = c(0, 0.2))) %>%
  
  
  
  layout(title = "Top 10 wicket taker in each season", showlegend = F,
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))



# 4) Wickets by a bowler in a stadium

all_seasons1<-sqldf("SELECT id,season,venue from matches")
colnames(all_seasons1) <- c("match_id","season","venue")
deliverie_season1<- sqldf("SELECT * from deliveries left join all_seasons1 using(match_id)")
subset1<-subset(deliverie_season1,select=c(match_id,venue,season,bowler,dismissal_kind))
dismissed<-sqldf("SELECT * from subset1 where dismissal_kind ='bowled' or dismissal_kind ='caught' or dismissal_kind ='caught and bowled' or dismissal_kind = 'hit wicket' or dismissal_kind = 'lbw' or dismissal_kind = 'stumped' ")
bowler_top<-aggregate(cbind(Wickets_Taken = dismissal_kind)~bowler+venue+season+match_id,data=dismissed,FUN=function(x){NROW(x)})
#agg1<-aggregate(cbind(max_runs=total_runs)~venue+match_id+season+batting_team,data=subs,FUN="sum")
top_wickets_bowler<-head(bowler_top[order(bowler_top$Wickets_Taken,decreasing=TRUE),],10)
k<-ggplot(top_wickets_bowler, aes(x=Wickets_Taken, y=venue, color=as.factor(bowler))) +
  geom_point() + 
  facet_grid(. ~ season)
ggplotly(k)


#5)win by runs

win_by_run <-aggregate(win_by_runs~winner,data=matches,FUN=max)
top_winner_team <-head(win_by_run[order(win_by_run$win_by_runs,decreasing=TRUE),],10)

graph <- plot_ly(top_winner_team, labels = top_winner_team$winner, values = top_winner_team$win_by_runs, type = 'pie') %>%
  layout(title = 'Win by runs with percentages',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
graph



# 6) Stadium in which a team won highest matches

#data3<-subset(matches,select=c(winner,venue))
venue1<-aggregate(cbind(count=winner)~winner+venue,data=matches,FUN=function(x){NROW(x)})
max_winner<-sqldf("SELECT  winner,venue,max(count) as maxc from venue1 group by winner")

graph<-ggplot(max_winner,aes(venue,maxc,fill=winner))+geom_bar(stat="identity")+ggtitle("Stadium in which a team won highest matches")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
ggplotly(graph)


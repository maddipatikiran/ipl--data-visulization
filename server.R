
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#
library(shiny)
library(ggplot2)
library(plotly)
require("sqldf")
require(data.table)

deliveries<-read.csv("C:/Users/Kiran/Desktop/IPL FINAL/final_rshiny/deliveries.csv",header=T,na.strings="")
matches<-read.csv("C:/Users/Kiran/Desktop/IPL FINAL/final_rshiny/matches.csv",header=T,na.strings="")
id_season<-sqldf("SELECT id,season from matches")
colnames(id_season) <- c("match_id","season")
deliveries_season<- sqldf("SELECT * from deliveries left join id_season using(match_id)")

#1)win by a team in each season

win_season<-aggregate(cbind(No_Of_Matches_Won =winner)~winner+season,data=matches,FUN=function(x){NROW(x)})
names(win_season)<-c("Winner","Season","No_of_matches_won")

#2)Team wise maximum runs
team_subset<-subset(deliveries_season,select=c(match_id,batting_team,total_runs,season))
teams_aggregate<-aggregate(cbind(max_runs=total_runs)~batting_team+match_id+season,data=team_subset,FUN="sum")

#3)top batsman in all seasons
batsman_bruns <- subset(deliveries,select=c(batsman,batsman_runs))
abatsman_bruns <- aggregate(batsman_runs~batsman,data=batsman_bruns,FUN = "sum")

#4)top batsman in each season
batsman_bruns_each <- subset(deliveries_season,select=c(batsman,batsman_runs,season))
abatsman_bruns_each <- aggregate(batsman_runs~batsman+season,data=batsman_bruns_each,FUN = "sum")

#5)umpire1 repetition
season_u1<- subset(matches,select=c(season,umpire1))
umpire_dt = data.table(season_u1)
count_u1<-aggregate(cbind(No_of_umpire1= umpire1)~umpire1+season,data=umpire_dt,FUN=function(x){NROW(x)})

#6)umpire2 repetition
season_u2<- subset(matches,select=c(season,umpire2))
umpire_dt1 = data.table(season_u2)
count_u2<-aggregate(cbind(No_of_umpire2= umpire2)~umpire2+season,data=umpire_dt1,FUN=function(x){NROW(x)})


#7)top 4's hit by a batsman
foursdata <- subset(deliveries_season, batsman_runs == "4")
fours_bman <-subset(foursdata,select=c(batsman_runs,batsman,season))
fours<-aggregate(cbind(No_of_Fours = batsman_runs)~batsman+season,data=fours_bman,FUN=function(x){NROW(x)})



#8)top 6's hit by a batsman
sixdata <- subset(deliveries_season, batsman_runs == "6")
six_bman <-subset(sixdata,select=c(batsman_runs,batsman,season))
sixes<-aggregate(cbind(No_of_sixes = batsman_runs)~batsman+season,data=six_bman,FUN=function(x){NROW(x)})


#9)highest number of catches by a fielder
catchesdata <- subset(deliveries_season, dismissal_kind =="caught")
catch <-subset(catchesdata,select=c(dismissal_kind,fielder,season))
kind_field<-aggregate(cbind(Dismissal_Count = dismissal_kind)~fielder+season,data=catch,FUN=function(x){NROW(x)})



#10)top 10 bowlers in all seasons
dismissed<-sqldf("SELECT * from deliveries_season where dismissal_kind ='bowled' or dismissal_kind ='caught' or dismissal_kind ='caught and bowled' or dismissal_kind = 'hit wicket' or dismissal_kind = 'lbw' or dismissal_kind = 'stumped' ")
top_bowler<-subset(dismissed,select=c(dismissal_kind,bowler,season))
bowler_top<-aggregate(cbind(Wickets_Taken = dismissal_kind)~bowler+season,data=top_bowler,FUN=function(x){NROW(x)})

#11)maximum runs by a batsman in each season he played
bat<- subset(deliveries_season,select=c(batsman,batsman_runs,season,match_id))

#12)stadium wise max runs
id_venue<-sqldf("SELECT id,season,venue from matches")
colnames(id_venue) <- c("match_id","season","venue")
deliveries_venue<- sqldf("SELECT * from deliveries left join id_venue using(match_id)")
stadium_runs<-subset(deliveries_venue,select=c(match_id,venue,season,total_runs,batting_team))
batsman_runs<-aggregate(cbind(max_runs=total_runs)~venue+match_id+season+batting_team,data=stadium_runs,FUN="sum")

#13)number of times a batsman is dismissed by a bowler
dismissed<-sqldf("SELECT * from deliveries_season where dismissal_kind ='bowled' or dismissal_kind ='caught' or dismissal_kind ='caught and bowled' or dismissal_kind = 'hit wicket' or dismissal_kind = 'lbw' or dismissal_kind = 'stumped' ")
top_bowler<-subset(dismissed,select=c(dismissal_kind,bowler,player_dismissed,season))
bowler_top<-aggregate(cbind(Wickets_Taken = dismissal_kind)~bowler+player_dismissed+season,data=top_bowler,FUN=function(x){NROW(x)})


shinyServer(function(input, output) {
  output$value<-renderPlotly({ 
    teams_max_runs<-subset(teams_aggregate,batting_team==input$bat_team)
    team_max_runs<-aggregate(cbind(count=max_runs)~season+batting_team,data=teams_max_runs,FUN=max)
    graph_teams<-ggplot(team_max_runs,aes(season,count,fill=batting_team))+geom_col() +ggtitle("Maxium runs scored by a team")
    ggplotly(graph_teams)
  })
  
  output$value1<-renderPlotly({
    teams<-subset(win_season,Season==input$season1)
    k<-ggplot(teams,aes(Winner,No_of_matches_won,fill=Winner))+geom_col()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Number of matches won by a team in a season")
    ggplotly(k)
    
  })
  
  output$value2<-renderTable({ 
    teams<-subset(matches,select=c(team1,season))
    agg<-aggregate(cbind(count=team1)~season,data=teams,FUN=function(x){NROW(x)})
    teams1<-subset(agg,season==input$season2)
  })
  
  output$value3<-renderPlotly({ 
    print(input$bins)
    top_batsman <-head(abatsman_bruns[order(abatsman_bruns$batsman_runs,decreasing=TRUE),],input$bins)
    p<-ggplot(top_batsman, aes(batsman, batsman_runs,fill=batsman)) + geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Top Runs by a batsman in all Seasons")
    ggplotly(p)
  })
  output$value4<-renderPlotly({ 
    teams5<-subset(abatsman_bruns_each,season==input$season5)
    top_batsman <-head(teams5[order(teams5$batsman_runs,decreasing=TRUE),],input$bins1)
    p<-ggplot(top_batsman, aes(batsman, batsman_runs,fill=batsman))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Top Runs by a batsman in each season")
    ggplotly(p)
  })
  output$value5<-renderPlotly({ 
    
    count_umpire1<-subset(count_u1,season==input$season6)
    top_u1<-head(count_umpire1[order(count_umpire1$No_of_umpire1,decreasing = TRUE),],input$range )
    p<-ggplot(top_u1, aes(umpire1,No_of_umpire1,fill=umpire1)) + geom_col()+ggtitle("Number of times a person acted as umpire 1")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    
    
    count_umpire2<-subset(count_u2,season==input$season6)
    top_u2<-head(count_umpire2[order(count_umpire2$No_of_umpire2,decreasing = TRUE),],input$range )
    p1<-ggplot(top_u2, aes(umpire2,No_of_umpire2,fill=umpire2)) + geom_col()+ggtitle("Number of times a person acted as umpire 2")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    
    
    fours_each<-subset(fours,season==input$season6)
    top_fours<-head(fours_each[order(fours_each$No_of_Fours,decreasing=TRUE),],input$range)
    k<-ggplot(top_fours, aes(batsman,No_of_Fours,color=factor(batsman)))+geom_point()+ggtitle("Top 4's hit by a batsman")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    
    
    sixes_each<-subset(sixes,season==input$season6)
    top_sixes<-head(sixes_each[order(sixes_each$No_of_sixes,decreasing=TRUE),],input$range)
    k1<-ggplot(top_sixes, aes(batsman,No_of_sixes,color=factor(batsman)))+geom_point()+ggtitle("Top 6's hit by a batsman")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    
    
    catches_fielder<-subset(kind_field,season==input$season6)
    top<-head(catches_fielder[order(catches_fielder$Dismissal_Count,decreasing=TRUE),],input$range)
    
    top_bowlers<-subset(bowler_top,season==input$season6)
    top_x_bowlers<-head(top_bowlers[order(top_bowlers$Wickets_Taken,decreasing=TRUE),],input$range)
    
    
    switch(input$show_vars,
           '1'=ggplotly(p),
           '2'=ggplotly(p1),
           '3'=ggplotly(k),
           '4'=ggplotly(k1),
           '5'=plot_ly(labels = top$fielder , values = top$Dismissal_Count) %>%
             add_pie(hole = 0.6) %>%
             layout(title = "Highest Catches by a Fielder with Percentage",  showlegend = F,
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
           '6'=plot_ly(labels = top_x_bowlers$bowler , values = top_x_bowlers$Wickets_Taken) %>%
             add_pie(hole = 0.6) %>%
             layout(title = "Top Bowlers in all seasons with percentages",  showlegend = F,
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
           
    )
  })
  
  
  output$value6<-renderPlotly({ 
    
    
    top_fours<-head(fours[order(fours$No_of_Fours,decreasing=TRUE),],input$range1)
    k<-ggplot(top_fours, aes(batsman,No_of_Fours,color=factor(batsman)))+geom_point()+ggtitle("Top 4's hit by a batsman in all seasons")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    
    top_sixes<-head(sixes[order(sixes$No_of_sixes,decreasing=TRUE),],input$range1)
    k1<-ggplot(top_sixes, aes(batsman,No_of_sixes,color=factor(batsman)))+geom_point()+ggtitle("Top 6's hit by a batsman in all seasons")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))
    
    top <- head(kind_field[order(kind_field$Dismissal_Count,decreasing=TRUE),],input$range1)
    
    top_10_bowlers<- head(bowler_top[order(bowler_top$Wickets_Taken,decreasing=TRUE),],input$range1)
    
    switch(input$show_vars1,
           
           '1'=ggplotly(k),
           '2'=ggplotly(k1),
           '3'=plot_ly(labels = top$fielder , values = top$Dismissal_Count) %>%
             add_pie(hole = 0.6) %>%
             layout(title = "Highest catches by a fielder with percentage",  showlegend = F,
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE)),
           '4'=plot_ly(labels = top_10_bowlers$bowler , values = top_10_bowlers$Wickets_Taken) %>%
             add_pie(hole = 0.6) %>%
             layout(title = "Top bowlers in all seasons with percentages",  showlegend = F,
                    xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                    yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))
           
    )
  })
  output$value7<-renderPlotly({ 
    max_runs<-aggregate(cbind(max_runs =batsman_runs)~batsman+season+match_id,data=bat,FUN="sum")
    max_runs_batsman<-subset(max_runs,batsman==input$batsman1)
    maximum_in_season<-aggregate(cbind(maximum =max_runs)~season,data=max_runs_batsman,FUN=max)
    g<-ggplot(maximum_in_season,aes(season,maximum,fill=maximum))+geom_col()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Maximum runs by a batsman in each season he played")
    ggplotly(g)
  })
  
  output$value8<-renderPlotly({ 
    max_runs_stadium<-subset(batsman_runs,venue==input$venue)
    max_stadium<-aggregate(cbind(max_runs1 =max_runs)~batting_team+season,data=max_runs_stadium,FUN=max)
    top_stadiums<-head(max_stadium[order(max_stadium$max_runs1,decreasing=TRUE),],5)
    g<-ggplot(top_stadiums,aes(season,max_runs1,fill=batting_team))+geom_bar(stat="identity")+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Top 5 teams scoring max runs in a stadium")
    ggplotly(g)
    
  })
  output$value9<-renderPlotly({ 
    batsman_dismissed<-subset(bowler_top,player_dismissed==input$dismissed)
    top <- head(batsman_dismissed[order(batsman_dismissed$Wickets_Taken,decreasing=TRUE),],10)
    g<-ggplot(top,aes(bowler,Wickets_Taken,fill=bowler))+geom_col()+theme(axis.text.x=element_text(angle=90,hjust=1,vjust=0.5))+ggtitle("Top 10 times a batsman is dismissed by a bowler")
    ggplotly(g)
    
  })
  
  
})


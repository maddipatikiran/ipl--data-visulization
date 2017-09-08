setwd('C:/Users/Kiran/Desktop/IPL FINAL/final_rshiny')
library(shiny)

require(plotly)


shinyUI(fluidPage(
  
headerPanel(title=HTML("<img src='IPL1.png' height='80px' width='100px' align='left'></img><img src='sa.gif' height='80px' width='100px' align='right'></img> <center><i><font color='white'><b>IPL DATA VISUALIZATION</font></i></center>")),
  
  tags$style("body {background:url('kk.jpg') no-repeat center center fixed;
             background-size:cover;
             filter: grayscale(100%);}"),

  
  tags$header(tags$style("header {background-color: #800000;height:100px;}")),
  
  # 1) 
  headerPanel(title=HTML("<br><font color='black'><h3><i><b>1. Visualization for each season</h3></i></font><br>")),
  sidebarLayout(
    sidebarPanel(
      radioButtons('show_vars', 'Choices for you:',
                   choices=c("Umpire1"=1,"Umpire2"=2,"Fours by a Batsman"=3,"Sixes by a Batsman"=4,"Catches by a Fielder"=5,"Top Bowlers"=6)),
      sliderInput("range",
                  "select range:",
                  min = 1,
                  max = 50,
                  value = 10),
      selectInput("season6", "Select season",sort(unique(fours_each$season))),
      submitButton("submit",icon=icon("thumbs-up"))
      
    ),
    mainPanel(
      #plotlyOutput("value")
      plotlyOutput("value5")
    )
  ),
  
  
  
  
  # 2)  
  headerPanel(title=HTML("<br><font color='black'><h3><i><b>2. No .of matches won by a team in a  season</h3></i></font><br>")),
  sidebarLayout(
    sidebarPanel(
      selectInput("season1", "Select Season",sort(unique(win_season$Season))),
      submitButton("submit",icon=icon("bar-chart-o")),
      HTML("<center><img src='alls2.gif' height='250px' width='400px' align='center'></center>")
    ),
    mainPanel(
      plotlyOutput("value1")
    )
  ),
  
  
  # 3)  
  headerPanel(title=HTML("<br><font color='black'><i><h3><b>3. No .of matches held in a season</h3></i></font><br>")),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("season2", "Select Season",sort(unique(agg$season))),
      submitButton("submit",icon=icon("thumbs-up"))
    ),
    
    mainPanel(
      HTML("<table align='center'><tr><td>"),
      tableOutput("value2"), HTML("</td><td>'   '</td><td align='center'><img src='Cup.jpg' height='180px' width='400px'></td></tr></table>")
    )
  ),
  
  
  # 4)
  headerPanel(title=HTML("<br><font color='black'><h3><i><b>4. Total runs of  a batsman in all seasons </h3></i></font><br>")),
  sidebarLayout(
    sidebarPanel(
      sliderInput("bins",
                  "Select Range:",
                  min = 1,
                  max = 50,
                  value = 10),
      submitButton("submit",icon=icon("thumbs-up")),
      HTML("<center><img src='mkk.gif' height='232px' width='400px' align='center'></center>")
    ),
    
    mainPanel(
      plotlyOutput("value3")
    )
  ),
  
  
  # 5)
  headerPanel(title=HTML("<br><font color='black'><h3><i><b>5. Total runs of a batsman in each seasons</h3></i></font><br>")),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("season5", "Select Season",sort(unique(abatsman_bruns_each$season))),
      sliderInput("bins1",
                  "Select Range:",
                  min = 1,
                  max = 50,
                  value = 10),
      submitButton("submit",icon=icon("thumbs-up")),
      HTML("<center><img src='5.png' height='165px' width='400px' align='center'></center>")
    ),
    
    mainPanel(
      plotlyOutput("value4")
    )
  ),
  
  # 6)
  
  headerPanel(title=HTML("<font color='black'><h3><b><i>6.Maxium runs scored by a team</h3></i></font><br>")),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("bat_team", "Select Season",sort(unique(teams_aggregate$batting_team))),
      submitButton("submit",icon=icon("thumbs-up")),
      HTML("<center><img src='high.jpg' height='250px' width='400px' align='center'></center>")
    ),
    
    mainPanel(
      plotlyOutput("value")
     
    )
  ),
  
  # 7)
  headerPanel(title=HTML("<br><font color='black'><h3><i><b>7. Visualization for all seasons</h3></i></font><br>")),
  
  sidebarLayout(
    sidebarPanel(
      radioButtons('show_vars1', 'Choices for you:',
                   choices=c("Fours by a Batsman"=1,"Sixes by a Batsman"=2,"Catches by a Fielder"=3,"Top 10 Bowlers"=4)),
      sliderInput("range1",
                  "select Range:",
                  min = 1,
                  max = 50,
                  value = 10),
      submitButton("submit",icon=icon("thumbs-up"))
      
    ),
    mainPanel(
      
      plotlyOutput("value6")
    )
  ),
  
  
  # 8)
  headerPanel(title=HTML("<br><font color='black'><h3><i><b>8.Maximum runs by a batsman in each season he played </h3></i></font><br>")),
  
  sidebarLayout(
    sidebarPanel(
      
      selectInput("batsman1", "Select Batsman",sort(unique(bat$batsman))),
      submitButton("submit",icon=icon("thumbs-up")),
      HTML("<center><img src='8runs.jpg' height='250px' width='400px' align='center'></center>")
    ),
    
    mainPanel(
      plotlyOutput("value7")
    )
  ),
  
  
  # 9)
  headerPanel(title=HTML("<br><font color='black'><h3><i><b>9.Top 5 teams scoring max runs in a stadium</h3></i></font><br>")),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("venue", "Select stadium",sort(unique(batsman_runs$venue))),
      submitButton("submit",icon=icon("thumbs-up")),
      HTML("<center><img src='9stadium.jpg' height='250px' width='400px' align='center'></center>")
    ),
    
    mainPanel(
      plotlyOutput("value8")
    )
  ),
  
  
  # 10)
  headerPanel(title=HTML("<br><font color='black'><h3><i><b>10.Number of times a batsman is dismissed by a bowler</h3></i></font><br>")),
  sidebarLayout(
    sidebarPanel(
      
      selectInput("dismissed", "Select player_dismissed",sort(unique(bowler_top$player_dismissed))),
      submitButton("submit",icon=icon("thumbs-up")),
      HTML("<center><img src='hm.gif' height='250px' width='400px' align='center'></center>")
    ),
    
    mainPanel(
      plotlyOutput("value9"),HTML("<br><br><br>")
    )
  ),
  

  tags$div(HTML("<br><br><footer><P style='BACKGROUND-COLOR: #800000'align='center'><font color='white'>&copy;MSS MLTeam 2017</footer>"))
  
  
  
  ))

#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#######
library(shiny)
library(ggplot2)
library(plyr)

##########################
train_basic = read.csv('C:/Users/Jihan Wei/Documents/Data/Dota2.csv', header = T)
player_ratings = read.csv('C:/Users/Jihan Wei/Documents/Data/player_ratings_all.csv', header = T)
logit1 = readRDS("C:/Users/Jihan Wei/Documents/Data/prediction_logit.rds")

# Return rating of the data.frame
get.rating = function(id) {
  idx = findInterval(id, player_ratings$account_id)
  idx = ifelse(player_ratings$account_id[idx] == id, idx, 1)
  return (c(rating_mu = player_ratings[idx, 2], rating_sigma = player_ratings[idx, 3]))
}

# Hero-based statistics table for preprocess()
hero_winrate_table = tapply(train_basic$Win, train_basic$hero_id, mean)
hero_winttl_table = table(train_basic$hero_id)
hero_ave_lh_table = tapply(train_basic$last_hits, train_basic$hero_id, mean)

preprocess = function(df){ # Given basic df, add hero-based win_rate, total_win and last_hits, rating_mu, rating_sigma
  stopifnot(!is.null(df$hero_id) & !is.null(df$account_id))
  hero_id_str_v = as.character(df$hero_id) 
  df$hero_winrate = as.numeric(hero_winrate_table[hero_id_str_v])
  df$hero_winttl = as.numeric(hero_winttl_table[hero_id_str_v])
  df$hero_ave_lh = as.numeric(hero_ave_lh_table[hero_id_str_v])
  m_rating = t(mapply(get.rating, df$account_id))
  return (data.frame(df, m_rating))
}

featureEngineering = function(df){ # Transfrom every block of df (10 rows) into a vector of features for training/testing/prediction
  stopifnot(!is.null(df$rating_mu) & !is.null(df$rating_sigma) & !is.null(df$hero_winrate) & !is.null(df$hero_ave_lh))
  radWin = ifelse(is.null(df$Win), NA, df$Win[1])
  # 1. Total rating (mu & sigma)
  u1 = sum(df$rating_mu[1:5]); u2 = sum(df$rating_mu[6:10]);
  s1 = sqrt(sum(df$rating_sigma[1:5]^2)); s2 = sqrt(sum(df$rating_sigma[6:10]^2));
  
  # 2. Total winrate
  w1 = sum(df$hero_winrate[1:5]); w2 = sum(df$hero_winrate[6:10]);
  # tot.gm1 = sum(df$hero.total[1:5]); tot.gm2 = sum(df$hero.total[6:10]);
  
  # 3. Total last_hits
  lh1 = sum(df$hero_ave_lh[1:5]); lh2 = sum(df$hero_ave_lh[6:10]);
  
  v = c(radWin, u1, s1, u2, s2, w1, w2, lh1, lh2)
  return (v)
}
colName = c('radWin', 'u1', 's1', 'u2', 's2', 'w1', 'w2', 'lh1', 'lh2')

###########################
wr_plot<-function(x){
  W_R<-x
  ##Prepare for donut plot
  dat2<-data.frame(rate=c(W_R,1-W_R),category=c("Win Rate", "Defeated Rate"))
  dat2$ymax = cumsum(dat2$rate)
  dat2$ymin = c(0, head(dat2$ymax, n=-1))
  ggplot(dat2, aes(fill=category, colors =c("yellow",10),ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
    geom_rect() +
    coord_polar(theta="y") +
    xlim(c(0, 4)) +
    theme(panel.grid=element_blank()) +
    theme(axis.text=element_blank()) +
    theme(axis.ticks=element_blank()) +
    annotate("text", x = 0, y = 0, label = paste("Predicted Win Rate","=",round(100*W_R,2),"%"))
}


################################################


ui<-shinyUI(fluidPage(
  
  titlePanel("Prediction"),
  
  sidebarLayout(
    
    sidebarPanel(
      fluidRow(
        column(3,
               numericInput("AID1", "Team A Player 1", 1),
               numericInput("AID2", "Team A Player 2",2),
               numericInput("AID3", "Team A Player 3", 3),
               numericInput("AID4", "Team A Player 4", 4),
               numericInput("AID5", "Team A Player 5", 5)
               
        ),
        column(3,
               numericInput("BID1", "Team B Player 1", 10000),
               numericInput("BID2", "Team B Player 2",10001),
               numericInput("BID3", "Team B Player 3", 10002),
               numericInput("BID4", "Team B Player 4", 10003),
               numericInput("BID5", "Team B Player 5", 10004)
         ),
        column(3,
               numericInput("AH1", "Team A Hero 1", 86),
               numericInput("AH2", "Team A Hero 2",51),
               numericInput("AH3", "Team A Hero 3", 83),
               numericInput("AH4", "Team A Hero 4", 11),
               numericInput("AH5", "Team A Hero 5", 67)
        ),
        column(3,
               numericInput("BH1", "Team B Hero 1", 106),
               numericInput("BH2", "Team B Hero 2",102),
               numericInput("BH3", "Team B Hero 3", 46),
               numericInput("BH4", "Team B Hero 4", 7),
               numericInput("BH5", "Team B Hero 5", 73)
               
        )
      )
    ),
    
    mainPanel(
      h4("Players"),
      tableOutput("p"),
      h4("Heros"),
      tableOutput("h"),
      h4("Win Probability for Team A"),
      plotOutput("PW")
    )
  )
))
# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  x <- reactive({
    a<-c(input$AID1,input$AID2,input$AID3,input$AID4,input$AID5)
    b<-c(input$BID1,input$BID2,input$BID3,input$BID4,input$BID5)
    c<-cbind(a,b)
    colnames(c)<-c("Team A Player","Team B Player")
    c
  })
  y<- reactive({
    a1<-c(input$AH1,input$AH2,input$AH3,input$AH4,input$AH5)
    b1<-c(input$BH1,input$BH2,input$BH3,input$BH4,input$BH5)
    c1<-cbind(a1,b1)
    colnames(c1)<-c("Team A Heroes","Team B Heros")
    c1
  })
  z1<- reactive({
   c(input$AID1,input$AID2,input$AID3,input$AID4,input$AID5,input$BID1,input$BID2,input$BID3,input$BID4,input$BID5)
    
  })
  
  z2<- reactive({
    c(input$AH1,input$AH2,input$AH3,input$AH4,input$AH5,input$BH1,input$BH2,input$BH3,input$BH4,input$BH5)
    
  })
  
  
  output$p <- renderTable({
    x()
  })
  
  output$h <- renderTable({
    y()
  })
  
  output$PW<- renderPlot({
    a<-z1()
    b<-z2()
    new_basic = data.frame(account_id = a, hero_id = b) 
    new_add = preprocess(new_basic)
    
    new_final_feature = featureEngineering(new_add)
    new_final_feature = data.frame(t(new_final_feature))
    colnames(new_final_feature) = colName
    pre<-predict(logit1, newdata = data.frame(new_final_feature), type = "response")
    wr_plot(x=pre)
    
  })
})

# Run the application 
shinyApp(ui = ui, server = server)


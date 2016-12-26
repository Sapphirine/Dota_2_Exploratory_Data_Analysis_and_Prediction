#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

##################Require Packages
library(shiny)
library(corrgram)
library(plyr)
library(ggplot2)
library(fmsb)
library(plotly)

########################################################################################################Code
dotadata<-read.csv("C:/Users/Jihan Wei/Documents/Data/Dota2.csv",header = T,as.is = T)   ##Load the data
In<-dotadata$account_id==0    
dotadata<-dotadata[!In,]       ##Delete players with 0 id

##create KDA
dotadata$deaths<-ifelse(dotadata$deaths==0,1,dotadata$deaths)
dotadata$KDA<-(dotadata$kills+dotadata$assists)/dotadata$deaths   

##selecte useful indicator for players' performance evaluation
ceritera<-c("account_id","KDA","last_hits","gold_per_min","xp_per_min","hero_damage","tower_damage","Win")
player_infor<-dotadata[,ceritera]

##Select Players who have participated in more than 5 matches
No_matches<-table(player_infor$account_id)
In2<-No_matches>5
sum(In2)
Player<-names(No_matches)[In2]
In3<-player_infor$account_id %in% Player
player_infor<-player_infor[In3,]

##Create variable  for Match Counting
player_infor$matches<-rep(1,nrow(player_infor))
Player_pefor2<-aggregate(player_infor[,c(8,9)],list(player_infor$account_id),sum)  
Player_pefor2$win_rate<-Player_pefor2$Win/Player_pefor2$matches

##Get summary of some variables for each player
Player_pefor1<-aggregate(player_infor[,c(2,3,4,5,6,7)],list(player_infor$account_id),mean)  

##Combine data and discard the useless variables
Player_pefor<-merge(Player_pefor1,Player_pefor2,by="Group.1")
Player_pefor<-Player_pefor[,-c(8)]     

names(Player_pefor)<-c("Player","KDA","Last_Hits","Gold", "Experience", "Hero_Damage","Tower_Damage","Matches","Win_Rate")       ##Rename the variable

##get limit for each variable:
lim1<-apply(Player_pefor,2,max)
lim2<-apply(Player_pefor,2,min)

##create function for radard plot
performance_plot<-function(Player_id){
  select<-Player_pefor$Player==Player_id
  dat<-rbind(lim1,lim2,Player_pefor[select,])
  dat<-dat[,c(2:7)]
  radarchart(dat, axistype=1, seg=5,pdensity=c(40), pcol=10,pfcol="Green",title=paste("Performace for Player",Player_id),caxislabels=c("Worst", "", "", "", "Best"))
}


wr_plot<-function(Player_id){
  ##get index
  select<-Player_pefor$Player==Player_id
  W_R<-Player_pefor$Win_Rate[select]
  ##Prepare for donut plot
  dat2<-data.frame(rate=c(W_R,1-W_R),category=c("Win Rate", "Defeated Rate"))
  dat2$ymax = cumsum(dat2$rate)
  dat2$ymin = c(0, head(dat2$ymax, n=-1))
  ggplot(dat2, aes(fill=category, ymax=ymax, ymin=ymin, xmax=4, xmin=3)) +
    geom_rect() +
    coord_polar(theta="y") +
    xlim(c(0, 4)) +
    theme(panel.grid=element_blank()) +
    theme(axis.text=element_blank()) +
    theme(axis.ticks=element_blank()) +
    annotate("text", x = 0, y = 0, label = paste("Win Rate","=",round(100*W_R,2),"%"))+
    labs(title=paste("Win Rate for Player",Player_id))
}

######################################################Shiny
ui<-shinyUI(fluidPage(
  
  titlePanel("Player's Performance Analysis"),
  
  sidebarLayout(
    
    sidebarPanel(
      fluidRow(
        column(7,
            numericInput("ID", "Please Type Player Id", 17)
            
        )
      )
      ),
    
    mainPanel(
      tabsetPanel(
        tabPanel("Radar-Plot", plotOutput("RP")), 
        tabPanel("Win-Rate", plotOutput("WR"))
      )
    )
  )
))

# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {

  output$RP <- renderPlot({
    if (input$ID %in% Player_pefor$Player){
    performance_plot(Player_id=input$ID)
    } else {
      plot(c(), c(), axes = FALSE, xlim = c( - 15,  15), ylim = c(- 15,  + 15),xlab="",ylab="")
      text(0,0,"Sorry, We don't have information for that player. Please try another one",col=10,cex = 1)
    }
})
  output$WR<- renderPlot({
    if (input$ID %in% Player_pefor$Player){
      wr_plot(Player_id=input$ID)
    } else {
      plot(c(), c(), axes = FALSE, xlim = c( - 15,  15), ylim = c(- 15,  + 15),xlab="",ylab="")
      text(0,0,"Sorry, We don't have information for that player. Please try another one",col=10, cex = 1)
    }
    
  })
})

# Run the application 
shinyApp(ui = ui, server = server)


#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

#########################################################Preperation Code
dota<-read.csv("C:/Users/Jihan Wei/Documents/Data/Dota2.csv",header = T,as.is = T) 

subdota<-dota[,c('match_id','hero_id','player_slot','Win')]
#head(subdota)
#length(unique(subdota$match_id))
subdota$player_slot<-ifelse(subdota$player_slot<5,0,1)

#######report win rate of each hero
s<-tapply(subdota$Win,factor(subdota$hero_id),sum)
n<-tapply(subdota$hero_id,factor(subdota$hero_id),length)
winrate<-s/n
#plot(winrate)
#######
hero_names<-read.csv('C:/Users/Jihan Wei/Documents/Data/hero_names.csv',header=TRUE,as.is=TRUE)
#head(hero_names)

#######requried function




##################################################

library(shiny)

ui<-shinyUI(fluidPage(
  
  titlePanel("Hero Recommendation"),
  
  sidebarLayout(
    
    sidebarPanel(
      fluidRow(
        column(6,
               numericInput("ID1", "First Selected Hero", 57),
               numericInput("ID2", "Second Selected Hero",2),
               numericInput("ID3", "Third Selected Hero", NULL),
               numericInput("ID4", "Forth Selected Hero", NULL),
               numericInput("ID5", "Fifth Selected Hero", NULL)
               
         ),
        column(6,
               numericInput("MID1", "My First Selected Hero",11),
               numericInput("MID2", "My Second Selected Hero", NULL),
               numericInput("MID3", "My Third Selected Hero", NULL),
               numericInput("MID4", "My Forth Selected Hero", NULL)
        )
       
      )
    ),
    
    mainPanel(
      h4("Their Selection"),
      tableOutput("Thier"),
      h4("My Selection"),
      tableOutput("My"),
      h4("Hero Recommendation"),
      tableOutput("Recommand")
      )
  )
))


# Define server logic required to draw a histogram
server <- shinyServer(function(input, output) {
  
  x <- reactive({
    a<-c(input$ID1,input$ID2,input$ID3,input$ID4,input$ID5)
    a<-cbind(hero_names$localized_name[a],a)
    colnames(a)<-c("Hero Name","Selected Id")
    a
  })
  y<- reactive({
    b<-c(input$MID1,input$MID2,input$MID3,input$MID4)
    b<- cbind(hero_names$localized_name[b],b)
    colnames(b)<-c("Hero Name","Selected Id")
    b
  })
  
  z<-reactive({
    x1<-c(input$ID1,input$ID2,input$ID3,input$ID4,input$ID5)
    y1<-c(input$MID1,input$MID2,input$MID3,input$MID4)
    return(list(x1=x1,y1=y1))
  })
  
  
  find.hero<-function(x,i){return(sum(x==i)==1)}
  
  hero.match<-function(x,data){
    return(sapply(data$hero_id,is.element,x))
  }
  find.match<-function(x,y,match_id){
    df<-subdota[subdota$match_id==match_id,]
    if (sum(is.na(y))!=1){
      match<-hero.match(x,df)
      if (sum(match)==0) {return(FALSE)}else{
        a<-df$player_slot[match]
        if (length(a)!=length(x)){return(FALSE)}else{
          if (sum(a)==0|sum(a)==length(x)) {
            df<-df[df$player_slot!=max(a),]
            b<-sum(hero.match(y,df))==length(y)
            return(b)
          } else {return(FALSE)}
        }}} else {
          match<-hero.match(x,df)
          a<-df$player_slot[match]
          if (length(a)!=length(x)){return(FALSE)}else{
            if (sum(a)==0|sum(a)==length(x)){return(TRUE)}
            else{return(FALSE)}
          }}}
  
  hero.recommend<-function(x,y){
    data<-subdota
    if (length(x)!=0) {index<-tapply(data$hero_id,factor(data$match_id),find.hero,i=x[1])
    index<-rep(index,each=10)
    data<-data[index,]
    unimatch<-sort(unique(data$match_id))
    index<-sapply(unimatch,find.match,x=x,y=y)
    index<-rep(index,each=10)
    matchinvolve<-data[index,]
    index2<-rep((1-matchinvolve$player_slot[matchinvolve$hero_id==x[1]]),each=10)==matchinvolve$player_slot
    winmatch<-matchinvolve[index2&matchinvolve$Win==1,]
    recommend<-sort(table(winmatch$hero_id),decreasing = TRUE)
    name<-as.numeric(names(recommend))
    names(recommend)<-hero_names$localized_name[name]
    if (sum(is.na(y))==1) {return(head(recommend,5))
    } else {
      return(head(recommend[-c(1:length(y))],5))
    }} else {
      s<-tapply(subdota$Win,factor(subdota$hero_id),sum)
      n<-tapply(subdota$hero_id,factor(subdota$hero_id),length)
      winrate<-s/n
      name<-as.numeric(names(winrate))
      names(winrate)<-hero_names$localized_name[name]
      return(sort(winrate,decreasing = TRUE)[1:5])
    }}
  
  output$Thier <- renderTable({
    x()
  })
  
  output$My <- renderTable({
    y()
  })
  
   output$Recommand <- renderTable({
     x1<-na.omit(z()$x1)
     y1<-na.omit(z()$y1)
     hero.recommend(x1,y1)
   })
})

# Run the application 
shinyApp(ui = ui, server = server)


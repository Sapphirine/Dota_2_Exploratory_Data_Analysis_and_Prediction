players<-read.csv("C:/Users/Jihan Wei/Documents/Data/players.csv",header = T, as.is = T)
matches<-read.csv("C:/Users/Jihan Wei/Documents/Data/match.csv",header = T, as.is = T)

A<-matches$radiant_win

creat<-function(a){
  if (a=="True"){
    A<-rep(rep(c(1,0),c(5,5)))
  } else {
    A<-rep(rep(c(0,1),c(5,5)))
  }
  return(A)
}

Win<-apply(as.matrix(A),1,creat)
Win<-matrix(Win,ncol = 1)

players$Win<-Win
write.csv(players,file = "C:/Users/Jihan Wei/Documents/Data/Dota2.csv",na = "")

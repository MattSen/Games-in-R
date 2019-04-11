################################################################################
###########################Points and Absolutue Terms###########################
################################################################################
col<-c(-0.18+0.1416667*c(1:5))    #Coordinates Columns
row<-c(-0.17+0.1108285*c(1:12))   #Coordinates Rows
##############################
xcord<-matrix(nrow=11,ncol=4)     #Coordinates Rows in Matrix
for (n in 1:11){
  xcord[n,]<-col[1:4]}
##############################
ycord<-matrix(nrow=11,ncol=4)     #Coordinates Columns in Matrix
for (n in 1:4){
  ycord[,n]<-row[1:11]}
##############################
xcordcon<-matrix(nrow=10,ncol=4)  #x-Coordinates Controls in Matrix
for (n in 1:10){                  
  for (m in 1:2){
    xcordcon[n,m]<-col[5]-0.02}
  for (m in 3:4){
    xcordcon[n,m]<-col[5]+0.02}
}
###############################
ycordcon<-matrix(nrow=10,ncol=4)  #y-Coordinates Controls in Matrix
for (n in 1:10){                  
  for (m in c(1,3)){
    ycordcon[n,m]<-row[n]+0.02}
  for (m in c(2,4)){
    ycordcon[n,m]<-row[n]-0.02}
}
###############################
colors<-c("green3","mediumblue","red1","gold","darkorange1","darkorchid2") #Colors
###############################
GameCountTotal<-0
AnalysisEasyRounds<-NA                                                     #Analysis
AnalysisHardRounds<-NA 
TimeElapsedEasy<-NA
TimeElapsedHard<-NA
Score<-0
Circles<-180
WinAnalysis<-0                                                           
################################################################################
#################################Functions######################################
################################################################################
NewGameWindow<-function(){
  dev.new(width=5, height=7)
  plot.new()
  par(mfcol=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
  lines(x=c(-0.2695479,1.157647),y=c(0.5,0.5), lwd=2, lty=2, col="ivory4")
  text(x=0.4365693,y=0.8802325,"New Game",cex=6, col="black")
  text(x=0.4605055,y=0.117151,"End",cex=10, col="black")
  d<-locator(1)
    if (d$y > 0.5){
    Game<-"yes"
    dev.off()
    }
    if (d$y < 0.5){
    Game<-"no"
    dev.off()
    }
return(Game)}
################################################################################
Difficulty<-function(){        
dev.new(width=5, height=7)
plot.new()
par(mfcol=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
lines(x=c(-0.2695479,1.157647),y=c(0.5,0.5), lwd=2, lty=2, col="ivory4")
text(x=0.4365693,y=0.8802325,"EASY",cex=10, col="black")
text(x=0.4605055,y=0.117151,"HARD",cex=10, col="black")
text(x=0.4605055,y=-0.117151,"(duplicate colours)",cex=3, col="black")
d<-locator(1)
if (d$y > 0.5){diff<-"easy"} else {diff<-"hard"}
dev.off()
return(diff)
}
################################################################################
GameWindow<-function(){
  dev.new(width=5, height=7)
  plot.new()
  par(mfcol=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
  polygon(x=c(-0.15,0.7,0.7,-0.15),y=c(-0.2,-0.2,1.13,1.13), col="grey",density=50)
  polygon(x=c(-0.18,0.67,0.67,-0.18),y=c(-0.17,-0.17,1.16,1.16), col="lightgoldenrod1")
  text(x=0.9212769, y=-0.22, "'Bulls and Cows' - by MattSen", col="red", cex=0.5)
    #Row Numbers#
    for (a in 1:10){text(x=-0.14, y=row[a], a, col="ivory4", cex=1.5)}
    #Row Lines#
    for (a in 1:9){lines(x=c(col[1],col[4]), y=rep((row[a]+row[a+1])/2,2), lty=2)}
    #Hidden Code#
    polygon(x=c(-0.14,-0.10+0.1416667*4,-0.10+0.1416667*4,-0.14), y=c(rep((row[10]+row[11])/2,2),rep((row[11]+row[12])/2,2)), lwd=2, lty=2, border="ivory4")
    #Empty Field#
    points(x=xcord,y=ycord,pch = 19, cex = 5, col = "lightgoldenrod")
    points(x=xcordcon,y=ycordcon,pch = 19, cex = 1.7, col = "lightgoldenrod")
    #Color choice#
    points(x=rep(0.9212769,6),y=row[c(11,9,7,5,3,1)],cex=10,pch = 19,col=colors)
    #Correction Button#
    points(x=1.0905,y=1.162,pch = 1, cex = 5, col = "black")
    text(x=1.091822, y=1.165843, "x", col="red", cex=1.8)
    #Solution Button#
    points(x=col[5],y=row[11],pch = 0, cex = 4, col = "ivory4")
    text(x=col[5], y=row[11], "!", col="ivory4", cex=1.6)
}
################################################################################
CleanLine<-function(x,rounds){
   if (x$x>1.034974  & x$x<1.142686 & x$y<1.202907  & x$y>1.122238)  {
    countcode<-0
    values[rounds,1:4]<-rep("lightgoldenrod",4)
    points(x=xcord[rounds,1:4],y=ycord[rounds,1:4],pch = 19, cex = 5, col = "lightgoldenrod")
    }
    return(c(countcode,values[rounds,1:4]))
}
################################################################################
Solution<-function(x){
    if (x$x>0.4844416  & x$x<0.5801863 & x$y>1.013227  & x$y<1.085174)  {
    rounds<-10
    countcode<-4
    points(x=xcord[11,],y=ycord[11,],pch = 19, cex = 5, col = values[11,])
    }
    return(c(countcode,rounds))
}
################################################################################
ColorChoice<-function(x){
  count<-0
  actcol<-"lightgoldenrod" #Start Value
       if (x$x>0.8105720 & x$x<1.0319817){
          if (x$y<1.130959    & x$y>0.97398254)  {
          actcol<-"green3"
          count<-1}
          if (x$y<0.90421509  & x$y>0.74941855)  {
          actcol<-"mediumblue"
          count<-1}
          if (x$y<0.68401156  & x$y>0.52921501)  {
          actcol<-"red1"
          count<-1}
          if (x$y<0.46162779  & x$y>0.30465101)  {
          actcol<-"gold"
          count<-1}
          if (x$y<0.24360449  & x$y>0.08226725)  {
          actcol<-"darkorange1"
          count<-1}
          if (x$y<0.02340096  & x$y > -0.1335758)  {
          actcol<-"darkorchid2"
          count<-1}
    }
    return(c(count,actcol))
}
################################################################################
################################################################################
##################################One Game######################################
################################################################################
################################################################################
while (NewGameWindow()=="yes"){
#######################Variables Gameplay at Start##############################
GameCountTotal<-GameCountTotal+1
win<-"no"
rounds<-1
values<-matrix(nrow=11,ncol=4,"lightgoldenrod")    #Pins lightgoldenrod
valuescon<-matrix(nrow=11,ncol=4,"lightgoldenrod") #Controls lightgoldenrod
###########################Degree of Difficulty#################################
diff<-Difficulty()
difficult<-diff
#############################The Secret Code####################################
if (diff=="easy"){values[11,]<-sample(colors,4,replace=F)}
if (diff=="hard"){values[11,]<-sample(colors,4,replace=T)}  
################################Play############################################
GameWindow()
if (difficult=="easy"){
TimeStartEasy<-Sys.time()
text(x= -0.2366357, y=-0.22, "easy", col="red", cex=0.5)
}
if (difficult=="hard"){
TimeStartHard<-Sys.time()
text(x= -0.2366357, y=-0.22, "hard", col="red", cex=0.5)
}
while (rounds<11 & win=="no"){
countcode<-0
 while(countcode<4){ 
  click<-locator(1)  
  #Clean Line
  countcode<-as.numeric(CleanLine(click,rounds)[1])  
  values[rounds,1:4]<-CleanLine(click,rounds)[2:5]
  #Solution
  countcode<-Solution(click)[1]
  rounds<-Solution(click)[2]
  #Colors
  if (as.numeric(ColorChoice(click)[1])==1){
  countcode<-countcode + as.numeric(ColorChoice(click)[1])
  values[rounds,countcode]<-ColorChoice(click)[2]
  points(x=xcord[rounds,countcode],y=ycord[rounds,countcode],pch = 19, cex = 5, col = values[rounds,countcode])
  }
  }  
  #Check
  evalcount<-NA
  for (veccol in levels(factor(values[11,]))) {
      given<-matrix(ncol=4,nrow=1,0)
      enter<-matrix(ncol=4,nrow=1,0)
      ######
      given[1,which (factor(values[11,]) == veccol)]<-2
      enter[1,which (factor(values[rounds,]) == veccol)]<-1
      diff<-given-enter
      #leere Stelle
      diff[1,which(diff[1,]==0)]<-NA
      #gleiche Stelle
      diff[1,which(diff[1,]==1)]<-"black"
      ###############      
  if (2 %in% diff==T & -1 %in% diff==T){
     if (length(which(diff==-1)) >= length(which(diff==2))){
     diff[1,which(diff[1,]==-1)]<-NA
     diff[1,which(diff[1,]==2)]<-"white"}
     if (length(which(diff==-1)) < length(which(diff==2))){   
     diff[1,which(diff==2)[1:length(which(diff==-1))]]<-"white"
     diff[1,which(diff[1,]==-1)]<-NA
     diff[1,which(diff[1,]==2)]<-NA}
     }
     diff[1,which(diff[1,]==2)]<-NA
     diff[1,which(diff[1,]==-1)]<-NA
     #Save Controls in one vector
     evalcount<-c(evalcount,diff)
     }
     if (length(na.omit(evalcount))>0){
     valuescon[rounds,1:length(na.omit(evalcount))]<-sort(na.omit(evalcount))
     points(x=xcordcon[rounds,],y=ycordcon[rounds,],pch = 19, cex = 1.7, col = valuescon[rounds,])}
#The Winning
if (values[rounds,1] == values[11,1] & values[rounds,2] == values[11,2] & values[rounds,3] == values[11,3] & values[rounds,4] == values[11,4] ) {
    win<-"yes"
    points(x=xcord[11,],y=ycord[11,],pch = 19, cex = 5, col = values[11,])
    WinAnalysis<-WinAnalysis+1
    if (difficult=="easy"){
    AnalysisEasyRounds[GameCountTotal]<-rounds
    TimeEndEasy<-Sys.time()
    TimeElapsedEasy[GameCountTotal]<-round(as.numeric(difftime(TimeEndEasy,TimeStartEasy,units="secs")),0)
    }
    if (difficult=="hard"){
    AnalysisHardRounds[GameCountTotal]<-rounds
    TimeEndHard<-Sys.time()
    TimeElapsedHard[GameCountTotal]<-round(as.numeric(difftime(TimeEndHard,TimeStartHard,units="secs")),0)
    }
    locator(1)
    dev.off()
    }
  #Roundcount
  rounds<-rounds+1  
  }
if (win=="no"){
  points(x=xcord[11,],y=ycord[11,],pch = 19, cex = 5, col = values[11,])
  if (difficult=="easy"){AnalysisEasyRounds[GameCountTotal]<-11}
  if (difficult=="hard"){AnalysisHardRounds[GameCountTotal]<-11}
  locator(1)
  dev.off()
  }  
##############################End of Game#######################################
}
################################################################################
################################Analysis########################################
################################################################################
dev.new(width=8, height=8)
par(mfrow=c(2,2))
par(las = 1)
boxplot(AnalysisEasyRounds[which(AnalysisEasyRounds<11)],AnalysisHardRounds[which(AnalysisHardRounds<11)], names=c("Easy","Hard"),col=c("blue","red"), border=c(1:2),main="Game Round-Analysis",xlab="Difficulty",ylim=c(1,11.57), ylab="Rounds")
legend("topright",legend=c(paste(length(na.omit(AnalysisEasyRounds[which(AnalysisEasyRounds<11)])),"of",length(na.omit(AnalysisEasyRounds))),paste(length(na.omit(AnalysisHardRounds[which(AnalysisHardRounds<11)])),"of",length(na.omit(AnalysisHardRounds)))),title="Number of Successful Games",bty="n",cex=0.5,col=c("blue","red"),fill=c("blue","red"))
if (length(na.omit(c(TimeElapsedEasy,TimeElapsedHard)))>0){boxTimeylim<-max(na.omit(c(TimeElapsedEasy,TimeElapsedHard)))}else{boxTimeylim<-100}
boxplot(TimeElapsedEasy,TimeElapsedHard, names=c("Easy","Hard"), border=c(1:2),main="Game Time-Analysis",xlab="Difficulty",ylim=c(0,(boxTimeylim+0.3*boxTimeylim)),col=c("blue","red"), ylab="Time [sec]")
legend("topright",legend=c(paste(length(na.omit(AnalysisEasyRounds[which(AnalysisEasyRounds<11)])),"of",length(na.omit(AnalysisEasyRounds))),paste(length(na.omit(AnalysisHardRounds[which(AnalysisHardRounds<11)])),"of",length(na.omit(AnalysisHardRounds)))),title="Number of Successful Games",bty="n",cex=0.5,col=c("blue","red"),fill=c("blue","red"))
#
if (length(na.omit(TimeElapsedEasy))> 1){
plot(y=na.omit(TimeElapsedEasy),x=seq(1,length(na.omit(TimeElapsedEasy))),type='b',axes=F,ylab="",xlab="Games",col="blue",lwd=3,main="Improvement - Easy")
axis(side=1, at = 1:length(na.omit(TimeElapsedEasy)), las=1)
axis(side=2, at = seq(0,max(na.omit(TimeElapsedEasy))+50,by=round(max(na.omit(TimeElapsedEasy))/10,0)),las=1,col="blue")
par(new=T)
plot.new()
plot(y=AnalysisEasyRounds[which(AnalysisEasyRounds<11)],x=seq(1,length(na.omit(AnalysisEasyRounds[which(AnalysisEasyRounds<11)]))),ylab="",xlab="",type='b',axes=F, col="cyan4",lwd=3)
axis(side=4, at = seq(1,10,by=1),las=1,col="cyan4")
legend("topright",c("Time","Rounds"),cex=1,fill=c("blue","cyan4"))
} else {
plot.new()
text(x=0.5,y=0.5,"not available",cex=2, col="black")
}
if (length(na.omit(TimeElapsedHard))>1){
plot(y=na.omit(TimeElapsedHard),x=seq(1,length(na.omit(TimeElapsedHard))),type='b',axes=F,ylab="",xlab="Games",col="red",lwd=3,main="Improvement - Hard")
axis(side=1, at = 1:length(na.omit(TimeElapsedHard)), las=1)
axis(side=2, at = seq(0,max(na.omit(TimeElapsedHard))+50,by=round(max(na.omit(TimeElapsedHard))/10,0)),las=1,col="red")
par(new=T)
plot.new()
plot(y=AnalysisHardRounds[which(AnalysisHardRounds<11)],x=seq(1,length(na.omit(AnalysisHardRounds[which(AnalysisHardRounds<11)]))),ylab="",xlab="",type='b',axes=F, col="orange",lwd=3)
axis(side=4, at = seq(1,10,by=1),las=1,col="orange")
legend("topright",c("Time","Rounds"),cex=1,fill=c("red","orange"))
} else {
plot.new()
text(x=0.5,y=0.5,"not available",cex=2, col="black")
}
locator(1)                                                                                                                                                               
dev.off()
################################################################################
##########################The Score#############################################
################################################################################
Score<-2*length(na.omit(AnalysisEasyRounds))+3*length(na.omit(AnalysisHardRounds))-(length(AnalysisEasyRounds)-length(na.omit(AnalysisEasyRounds)))-(length(AnalysisHardRounds)-length(na.omit(AnalysisHardRounds)))+round(sum(10/na.omit(AnalysisHardRounds[which(AnalysisHardRounds!=11)])),0)+round(sum(10/na.omit(AnalysisEasyRounds[which(AnalysisEasyRounds!=11)])),0)+round(sum(120/na.omit(TimeElapsedEasy)),0)+round(sum(120/na.omit(TimeElapsedHard)),0)-1
if (Score<0){Score<-0}
if (WinAnalysis==0){Score<-0}
dev.new(width=4, height=2)
par(mfcol=c(1,1), mar=c(0,0,0,0), oma=c(0,0,0,0))
plot.new()
for (farben in rep(colors,3)){
Circles<-Circles-10
points(x=0.5,y=0.5,col=farben,cex=Circles, ,pch = 19)}
text(labels=paste("Score:",Score),x=0.5,y=0.5,cex=4,col="white")
locator(1)                                                                                                                                                               
dev.off()





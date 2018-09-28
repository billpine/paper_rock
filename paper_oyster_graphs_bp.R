##########################
#Oyster Spat Counting 
##########################

#Function to read excel worsheets as list
library(readxl)    
read_excel_allsheets <- function(filename, tibble = FALSE) {
  # I prefer straight data.frames
  # but if you like tidyverse tibbles (the default with read_excel)
  # then just pass tibble = TRUE
  sheets <- readxl::excel_sheets(filename)
  x <- lapply(sheets, function(X) readxl::read_excel(filename, sheet = X))
  if(!tibble) x <- lapply(x, as.data.frame)
  names(x) <- sheets
  x
}

setwd("~/Git/paper_rock")

Oyster <- read_excel_allsheets("Oyster_Counting.xlsx")

Oyster$data_sentinel
Oyster$key
Oyster$data_transect
#Adding Key column to each dataframe
Oyster_sentinel<-merge.data.frame(Oyster$data_sentinel,Oyster$key,
                                  by=c("rock"))
Oyster_transect<-merge.data.frame(Oyster$data_transect,Oyster$key,
                                  by=c("rock"))

#########################
#Sentinel Rock stuff
#########################
plot(as.factor(Oyster_sentinel$rock),Oyster_sentinel$count,type="p", ylim=c(20,100), pch=16, las=1, ylab="N", main="Sentinel Rock Counts", xlab="Rock")
points(as.factor(Oyster_sentinel$rock), Oyster_sentinel$true_n, col=2, pch=16)
legend("topleft",inset=0.05, title="True v. Count", c("True", "Count"),col=c(2,1), pch=c(16,15))

#Catchability 
plot(as.factor(Oyster_sentinel$rock),Oyster_sentinel$count/Oyster_sentinel$true_n, type="p", ylim=c(0.7,1.2), pch=16, las=1, ylab="Catchability", main="Sentinel Rock Counts (Proportion of True N)", xlab="Rock")

#CV of Catchability
plot(aggregate(Oyster_sentinel$count/Oyster_sentinel$true_n, by=list(as.factor(Oyster_sentinel$rock)), FUN=function(x) {sd(x)/mean(x)}),ylim=c(0,0.2), pch=16, las=1, ylab="CV of Catchability", main="Sentinel Rock (CV)", xlab="Rock")

aggregate(Oyster_sentinel$count/Oyster_sentinel$true_n, by=list(as.factor(Oyster_sentinel$rock)), FUN=function(x) {sd(x)/mean(x)})
aggregate(Oyster_sentinel$count/Oyster_sentinel$true_n, by=list(as.factor(Oyster_sentinel$rock)), FUN=sd)
aggregate(Oyster_sentinel$count/Oyster_sentinel$true_n, by=list(as.factor(Oyster_sentinel$rock)), FUN=mean)

#Overall
mean(Oyster_sentinel$count/Oyster_sentinel$true_n)
sd(Oyster_sentinel$count/Oyster_sentinel$true_n)/mean(Oyster_sentinel$count/Oyster_sentinel$true_n)

#As a function of experience
plot(as.factor(Oyster_sentinel$experience),Oyster_sentinel$count/Oyster_sentinel$true_n,ylim=c(0.7,1.2), pch=16, las=1, ylab="Catchability", main="Sentinel Rock", xlab="Experience", col=c(2,5))
points(aggregate(Oyster_sentinel$count/Oyster_sentinel$true_n, by=list(as.factor(Oyster_sentinel$experience)), FUN=mean), col=4, pch=16)
plot(aggregate(Oyster_sentinel$count/Oyster_sentinel$true_n, by=list(as.factor(Oyster_sentinel$experience)), FUN=function(x) {sd(x)/mean(x)}),ylim=c(0,0.2), pch=16, las=1, ylab="CV of Catchability", main="Sentinel Rock (CV)", xlab="Experience")


#Catchability for each observer
plot(as.factor(Oyster_sentinel$person),Oyster_sentinel$count/Oyster_sentinel$true_n,ylim=c(0.8,1.2), pch=16, las=1, ylab="Catchability", main="Sentinel Rock", xlab="Person", col=c(5,2,2,2,5))
points(aggregate(Oyster_sentinel$count/Oyster_sentinel$true_n, by=list(as.factor(Oyster_sentinel$person)), FUN=mean), col=4, pch=16)
plot(aggregate(Oyster_sentinel$count/Oyster_sentinel$true_n, by=list(as.factor(Oyster_sentinel$person)), FUN=function(x) {sd(x)/mean(x)}),ylim=c(0,0.2), pch=16, las=1, ylab="CV of Catchability", main="Sentinel Rock (CV)", xlab="Experience")

##########################
#Transect
##########################
#Counts vs True Numbers
plot(as.factor(Oyster_transect$rock),Oyster_transect$count, ylim=c(0,250), xlab="Rock", las=1, ylab="Count", main="Count by rock from transect")
points(as.factor(Oyster_transect$rock),Oyster_transect$true_n, col=2, pch=16)

#Catchability
plot(as.factor(Oyster_transect$rock),Oyster_transect$count/Oyster_transect$true_n, ylim=c(0,1), xlab="Rock", las=1, ylab="Catchability")
points(aggregate(Oyster_transect$count/Oyster_transect$true_n, by=list(as.factor(Oyster_transect$rock)), FUN=mean, na.rm=T), col=4, pch=16)

#as a function of the number of true n on a rock
plot(as.factor(Oyster_transect$true_n), Oyster_transect$count/Oyster_transect$true_n, ylim=c(0,1), pch=16, ylab="Catchability", xlab="True N on Rock", las=1)
points(aggregate(Oyster_transect$count/Oyster_transect$true_n, by=list(as.factor(Oyster_transect$true_n)), FUN=mean, na.rm=T), col=4, pch=16)

#CV as a function if true n on a rock
plot(aggregate(Oyster_transect$count/Oyster_transect$true_n, by=list(as.factor(Oyster_transect$true_n)), FUN=function(x) {sd(x, na.rm=T)/mean(x, na.rm=T)}), ylim=c(0,0.5), pch=16, ylab="CV Catchability", xlab="True N on Rock", las=1)
#sd
plot(aggregate(Oyster_transect$count/Oyster_transect$true_n, by=list(as.factor(Oyster_transect$true_n)), FUN=function(x) {sd(x, na.rm=T)}), ylim=c(0,0.2), pch=16, ylab="SD Catchability", xlab="True N on Rock", las=1)

#Experience
plot(as.factor(Oyster_transect$experience),Oyster_transect$count/Oyster_transect$true_n, ylim=c(0,1), xlab="Experience", las=1, ylab="Catchability", col=c(2,5))

#Person
plot(as.factor(Oyster_transect$person),Oyster_transect$count/Oyster_transect$true_n, ylim=c(0,1), xlab="Person", las=1, ylab="Catchability", col=c(5,2,2,2,5,2,2,2))

#Close vs rspaced vs spaced
plot(c(Oyster_transect$count/Oyster_transect$true_n)~as.factor(Oyster_transect$type), ylim=c(0,1), las=1, ylab="Catchability", xlab="Type")
plot(aggregate(Oyster_transect$count/Oyster_transect$true_n, by=list(as.factor(Oyster_transect$type)), FUN=function(x) {sd(x, na.rm=T)/mean(x, na.rm=T)}), ylim=c(0,1), las=1, ylab="CV Catchability", xlab="Type")

plot(c(Oyster_transect[Oyster_transect$rock %in% c("a","b","e","g","h","i"),"count"]/Oyster_transect[Oyster_transect$rock %in% c("a","b","e","g","h","i"),"true_n"])~interaction(as.factor(Oyster_transect[Oyster_transect$rock %in% c("a","b","e","g","h","i"),"type"]),as.factor(Oyster_transect[Oyster_transect$rock %in% c("a","b","e","g","h","i"),"rock"])), ylim=c(0,1), las=2, ylab="Catchability",xaxt="n", use.cols = T,outline=F,las=1, xlab="")
abline(v=c(3.5,6.5,9.5,12.5,15.5))
axis(1,at=c(2,5,8,11,14,17),las=1,font=2,labels=c("A","B","E","G","H","I"))
mtext("Rock (Close-Rspaced-Spaced)",side=1, line=2.75)

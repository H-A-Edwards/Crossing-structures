#Script to look at outliers, zeros and autocorrelation

rm(list = ls())

#-----------------Set working directory--------------------
setwd("/Users/hannahedwards/Documents/Alberta Parks/Crossing-structures/data/")
#setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/")

#----------------Load libraries.-------------
library(MCMCglmm)
library(lmtest)
library(vioplot)
library(zoo)
library(circular)

#-----------------Load data--------------------
#ungulates.hourly <- read.csv("ungulates hourly.csv")
#ungulates.monthly <- read.csv("ungulates monthly.csv")
#ungulates.annual <- read.csv("ungulates annual.csv")

#carnivores.hourly <- read.csv("carnivores hourly.csv")
#carnivores.monthly <- read.csv("carnivores monthly.csv")
#carnivores.annual <- read.csv("carnivores annual.csv")

#Split by crossing structure type
ungulates.hourly <- read.csv("ungulates hourly.csv")
ungulates.hourly.under<-subset(ungulates.hourly, ungulates.hourly$Underpass.type != "Jumpout")
ungulates.hourly.jump<-subset(ungulates.hourly, ungulates.hourly$Underpass.type != "Underpass")
ungulates.monthly <- read.csv("ungulates monthly.csv")
ungulates.monthly.under<-subset(ungulates.monthly, ungulates.monthly$Underpass.type != "Jumpout")
ungulates.monthly.jump<-subset(ungulates.monthly, ungulates.monthly$Underpass.type != "Underpass")
ungulates.annual <- read.csv("ungulates annual.csv")
ungulates.annual.under<-subset(ungulates.annual, ungulates.annual$Underpass.type != "Jumpout")
ungulates.annual.jump<-subset(ungulates.annual, ungulates.annual$Underpass.type != "Underpass")

carnivores.hourly <- read.csv("carnivores hourly.csv")
carnivores.hourly.under<-subset(carnivores.hourly, carnivores.hourly$Underpass.type != "Jumpout")
carnivores.hourly.jump<-subset(carnivores.hourly, carnivores.hourly$Underpass.type != "Underpass")
carnivores.monthly <- read.csv("carnivores monthly.csv")
carnivores.monthly.under<-subset(carnivores.monthly, carnivores.monthly$Underpass.type != "Jumpout")
carnivores.monthly.jump<-subset(carnivores.monthly, carnivores.monthly$Underpass.type != "Underpass")
carnivores.annual <- read.csv("carnivores annual.csv")
carnivores.annual.under<-subset(carnivores.annual, carnivores.annual$Underpass.type != "Jumpout")
carnivores.annual.jump<-subset(carnivores.annual, carnivores.annual$Underpass.type != "Underpass")

#Look for outliers in guild count
boxplot(carnivores.annual$Total)#139 count outlier checked
hist(carnivores.annual$Total, xlab="Total annual carnivore count per crossing structure",
     main=NULL, ylim=c(0,50))
summary(carnivores.annual$Total)
boxplot(carnivores.monthly$Total)#42 count outlier checked
summary(carnivores.monthly$Total)
hist(carnivores.monthly$Total, xlab="Total monthly carnivore count per crossing structure",
     main=NULL, ylim=c(0,40))
boxplot(carnivores.hourly$Total)#41 count outlier checked
summary(carnivores.hourly$Total)
hist(carnivores.hourly$Total, xlab="Total hourly carnivore count per crossing structure",
     main=NULL, ylim=c(0,100))
boxplot(ungulates.annual$Total)
summary(ungulates.annual$Total)
hist(ungulates.annual$Total, xlab="Total annual ungulate count per crossing structure",
     main=NULL, ylim=c(0,50))
boxplot(ungulates.monthly$Total)#2233 count outlier checked
summary(ungulates.monthly$Total)
hist(ungulates.monthly$Total, xlab="Total monthly ungulate count per crossing structure",
     main=NULL, ylim=c(0,50))
boxplot(ungulates.hourly$Total)#971 count outlier checked
summary(ungulates.hourly$Total)
hist(ungulates.hourly$Total, xlab="Total hourly ungulate count per crossing structure",
     main=NULL, ylim=c(0,100))
#Look at human variable
hist(ungulates.annual$annual.human)
hist(ungulates.monthly$monthly.human)
hist(ungulates.hourly$hourly.human)#Some zeros in the hourly counts not inflated
hist(carnivores.annual$annual.human)
hist(carnivores.monthly$monthly.human)
hist(carnivores.hourly$hourly.human)#Some zeros in the hourly counts not inflated

#----------------------------Check for autocorrelation--------------------------

#Annual carnivore count
#build a model of the mean 
car.ann.count.jump<-carnivores.annual.jump$Total
car.ann.count.under<-carnivores.annual.under$Total
mod1 <- lm(car.ann.count.jump ~ 1)
mod2 <- lm(car.ann.count.under ~ 1)
dwtest(mod1) #DW test (tests the residuals)-not autocorrelated
dwtest(mod2) #not autocorrelated
#also do the Box-Pierce an Ljung-Box tests which test based on the data
Box.test(car.ann.count.jump, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(car.ann.count.under, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(car.ann.count.jump, lag = 1, type = c("Ljung-Box"), fitdf = 0)
Box.test(car.ann.count.under, lag = 1, type = c("Ljung-Box"), fitdf = 0)
acf(car.ann.count.jump, main="", xlab="Total carnivore hourly count for jumpouts") #plot acf
acf(car.ann.count.under, main="", xlab="Total carnivore hourly count for underpasses")
var(car.ann.count.jump)
var(car.ann.count.under)
mean(car.ann.count.jump)
mean(car.ann.count.under)

#Monthly carnivore count
car.mon.count.jump<-carnivores.monthly.jump$Total
car.mon.count.under<-carnivores.monthly.under$Total
mod3 <- lm(car.mon.count.jump ~ 1)
mod4 <- lm(car.mon.count.under ~ 1)
dwtest(mod3) #not autocorrelated
dwtest(mod4) #not autocorrelated
Box.test(car.mon.count.jump, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(car.mon.count.under, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(car.mon.count.jump, lag = 1, type = c("Ljung-Box"), fitdf = 0)
Box.test(car.mon.count.under, lag = 1, type = c("Ljung-Box"), fitdf = 0)
acf(car.mon.count.jump, main="", xlab="Total carnivore monthly count for jumpouts") 
acf(car.mon.count.under, main="", xlab="Total carnivore monthly count for underpasses")
var(car.mon.count.jump)
var(car.mon.count.under)
mean(car.mon.count.jump)
mean(car.mon.count.under)

#Hourly carnivore count
car.hou.count.jump<-carnivores.hourly.jump$Total
car.hou.count.under<-carnivores.hourly.under$Total
mod5 <- lm(car.hou.count.jump ~ 1)
mod6 <- lm(car.hou.count.under ~ 1)
dwtest(mod5) #not autocorrelated
dwtest(mod6) #underpass data autocorrelated
Box.test(car.hou.count.jump, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(car.hou.count.under, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(car.hou.count.jump, lag = 1, type = c("Ljung-Box"), fitdf = 0)
Box.test(car.hou.count.under, lag = 1, type = c("Ljung-Box"), fitdf = 0)
acf(car.hou.count.jump, main="", xlab="Total carnivore hourly count for jumpouts") 
acf(car.hou.count.under, main="", xlab="Total carnivore hourly count for underpasses")
var(car.hou.count.jump)
var(car.hou.count.under)
mean(car.hou.count.jump)
mean(car.hou.count.under)

#Annual ungulate count
ung.ann.count.jump<-ungulates.annual.jump$Total
ung.ann.count.under<-ungulates.annual.under$Total
mod7 <- lm(ung.ann.count.jump ~ 1)
mod8 <- lm(ung.ann.count.under ~ 1)
dwtest(mod7) #not autocorrelated
dwtest(mod8) #not autocorrelated
Box.test(ung.ann.count.jump, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(ung.ann.count.under, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(ung.ann.count.jump, lag = 1, type = c("Ljung-Box"), fitdf = 0)
Box.test(ung.ann.count.under, lag = 1, type = c("Ljung-Box"), fitdf = 0)
acf(ung.ann.count.jump, main="", xlab="Total ungulate annual count for jumpouts") 
acf(ung.ann.count.under, main="", xlab="Total ungulate annual count for underpasses")
var(ung.ann.count.jump)
var(ung.ann.count.under)
mean(ung.ann.count.jump)
mean(ung.ann.count.under)

#Monthly ungulate count
ung.mon.count.jump<-ungulates.monthly.jump$Total
ung.mon.count.under<-ungulates.monthly.under$Total
mod9 <- lm(ung.mon.count.jump ~ 1)
mod10 <- lm(ung.mon.count.under ~ 1)
dwtest(mod9) #not autocorrelated
dwtest(mod10) #not autocorrelated
Box.test(ung.mon.count.jump, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(ung.mon.count.under, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(ung.mon.count.jump, lag = 1, type = c("Ljung-Box"), fitdf = 0)
Box.test(ung.mon.count.under, lag = 1, type = c("Ljung-Box"), fitdf = 0)
acf(ung.mon.count.jump, main="", xlab="Total ungulate monthly count for jumpouts")
acf(ung.mon.count.under, main="", xlab="Total ungulate monthly count for underpasses")
var(ung.mon.count.jump)
var(ung.mon.count.under)
mean(ung.mon.count.jump)
mean(ung.mon.count.under)

#Hourly ungulate count
ung.hou.count.jump<-ungulates.hourly.jump$Total
ung.hou.count.under<-ungulates.hourly.under$Total
mod11 <- lm(ung.hou.count.jump ~ 1)
mod12 <- lm(ung.hou.count.under ~ 1)
dwtest(mod11) #not autocorrelated
dwtest(mod12) #not autocorrelated
Box.test(ung.hou.count.jump, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(ung.hou.count.under, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(ung.hou.count.jump, lag = 1, type = c("Ljung-Box"), fitdf = 0)
Box.test(ung.hou.count.under, lag = 1, type = c("Ljung-Box"), fitdf = 0)
acf(ung.hou.count.jump, main="", xlab="Total ungulate hourly count for jumpouts")
acf(ung.hou.count.under, main="", xlab="Total ungulate hourly count for underpasses")
var(ung.hou.count.jump)
var(ung.hou.count.under)
mean(ung.hou.count.jump)
mean(ung.hou.count.under)

#-----------------------------Violin plots--------------------------------
car.ann.count<-carnivores.annual$Total
ung.ann.count<-ungulates.annual$Total
car.mon.count<-carnivores.monthly$Total
ung.mon.count<-ungulates.monthly$Total
car.hou.count<-carnivores.hourly$Total
ung.hou.count<-ungulates.hourly$Total

annual.counts<-cbind(car.ann.count,ung.ann.count)
month.counts<-cbind(car.mon.count,ung.mon.count)
#Make the hour counts the same length by adding NAs to carnivore vector
n<-max(length(car.hou.count),length(ung.hou.count))
length(car.hou.count)<-n
hour.counts<-cbind(car.hou.count,ung.hou.count)

vioplot(annual.counts)
vioplot(month.counts)
vioplot(hour.counts)

#---------------------Look at whether there is circularity in the dataset--------------------
#Setup the data differently, get frequencies of count for hour/month/year

newdata<-read.csv("newdata.csv")
#Split by species and then crossing structure
ungulates.under<-subset(newdata, newdata$Species.grouped == "Ungulates" &
                                 newdata$Underpass.type == "Underpass")
ungulates.jump<-subset(newdata, newdata$Species.grouped == "Ungulates" &
                                 newdata$Underpass.type == "Jumpout")
carnivores.under<-subset(newdata, newdata$Species.grouped == "Carnivores" &
                                 newdata$Underpass.type == "Underpass")
carnivores.jump<-subset(newdata, newdata$Species.grouped == "Carnivores" &
                          newdata$Underpass.type == "Jumpout")
carnivores<-subset(newdata, newdata$Species.grouped == "Carnivores")
Bobcat<-subset(newdata, newdata$Species == "Bobcat")
Cougar<-subset(newdata, newdata$Species == "Cougar")
Coyote<-subset(newdata, newdata$Species == "Coyote")
GrizzlyBear<-subset(newdata, newdata$Species == "Grizzly Bear")
Lynx<-subset(newdata, newdata$Species == "Lynx")
Marten<-subset(newdata, newdata$Species == "Marten")
RedFox<-subset(newdata, newdata$Species == "Red Fox")
StripedSkunk<-subset(newdata, newdata$Species == "Striped Skunk")
UnknownBear<-subset(newdata, newdata$Species == "Unknown Bear")
Wolf<-subset(newdata, newdata$Species == "Wolf")
Wolverine<-subset(newdata, newdata$Species == "Wolverine")

#Make data circular with hourending
#Vector
unghour<-ungulates.under$HourEnding
carhour<-carnivores.under$HourEnding
unghourc <- circular(unghour, units="hours", template="clock24")
carhourc <- circular(carhour, units="hours", template="clock24")
#the radius of a segment is taken to be the square root of the relative frequency
plot(unghourc)
a<-rose.diag(unghourc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="Ungulate hour frequency")
plot(carhourc)
b<-rose.diag(carhourc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="Carnivore hour frequency")


#Make data circular with Month
#Vector
ungmon<-ungulates.under$Month
carmon<-carnivores.under$Month
ungmonc <- circular(ungmon, units="hours", template="clock12")
carmonc <- circular(carmon, units="hours", template="clock12")
#the radius of a segment is taken to be the square root of the relative frequency
plot(ungmonc)
c<-rose.diag(ungmonc, bins=12, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="Ungulate month frequency")
plot(carmonc)
d<-rose.diag(carmonc, bins=12, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="Carnivore month frequency")

#Make data circular with hour for each carnivore species
carnivores$Species<-as.factor(carnivores$Species)
summary(carnivores$Species)

#Bobcat
Bobcathou<-Bobcat$HourEnding
Bobcathouc <- circular(Bobcathou, units="hours", template="clock24")
plot(Bobcathouc)
c<-rose.diag(Bobcathouc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="Bobcat hour frequency")

#Coyote
Coyotehou<-Coyote$HourEnding
Coyotehouc <- circular(Coyotehou, units="hours", template="clock24")
plot(Coyotehouc)
c<-rose.diag(Coyotehouc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="Coyote hour frequency")

#Grizzly Bear
GrizzlyBearhou<-GrizzlyBear$HourEnding
GrizzlyBearhouc <- circular(GrizzlyBearhou, units="hours", template="clock24")
plot(GrizzlyBearhouc)
c<-rose.diag(GrizzlyBearhouc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="GrizzlyBear hour frequency")

#Lynx
Lynxhou<-Lynx$HourEnding
Lynxhouc <- circular(Lynxhou, units="hours", template="clock24")
plot(Lynxhouc)
c<-rose.diag(Lynxhouc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="Lynx hour frequency")

#Marten
Martenhou<-Marten$HourEnding
Martenhouc <- circular(Martenhou, units="hours", template="clock24")
plot(Martenhouc)
c<-rose.diag(Martenhouc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="Marten hour frequency")

#RedFox
RedFoxhou<-RedFox$HourEnding
RedFoxhouc <- circular(RedFoxhou, units="hours", template="clock24")
plot(RedFoxhouc)
c<-rose.diag(RedFoxhouc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="RedFox hour frequency")

#StripedSkunk
StripedSkunkhou<-StripedSkunk$HourEnding
StripedSkunkhouc <- circular(StripedSkunkhou, units="hours", template="clock24")
plot(StripedSkunkhouc)
c<-rose.diag(StripedSkunkhouc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="StripedSkunk hour frequency")

#UnknownBear
UnknownBearhou<-UnknownBear$HourEnding
UnknownBearhouc <- circular(UnknownBearhou, units="hours", template="clock24")
plot(UnknownBearhouc)
c<-rose.diag(UnknownBearhouc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="UnknownBear hour frequency")

#Wolf
Wolfhou<-Wolf$HourEnding
Wolfhouc <- circular(Wolfhou, units="hours", template="clock24")
plot(Wolfhouc)
c<-rose.diag(Wolfhouc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="Wolf hour frequency")

#Wolverine
Wolverinehou<-Wolverine$HourEnding
Wolverinehouc <- circular(Wolverinehou, units="hours", template="clock24")
plot(Wolverinehouc)
c<-rose.diag(Wolverinehouc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="Wolverine hour frequency")
#Cougar
Cougarhou<-Cougar$HourEnding
Cougarhouc <- circular(Cougarhou, units="hours", template="clock24")
plot(Cougarhouc)
c<-rose.diag(Cougarhouc, bins=24, col="darkgrey", cex=1.5, prop=1.3, add=FALSE,
             xlab="Cougar hour frequency")



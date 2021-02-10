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
hist(carnivores.annual$Total)
summary(carnivores.annual$Total)
boxplot(carnivores.monthly$Total)#42 count outlier checked
summary(carnivores.monthly$Total)
hist(carnivores.monthly$Total)
boxplot(carnivores.hourly$Total)#41 count outlier checked
summary(carnivores.hourly$Total)
hist(carnivores.hourly$Total)
boxplot(ungulates.annual$Total)
summary(ungulates.annual$Total)
hist(ungulates.annual$Total)
boxplot(ungulates.monthly$Total)#2233 count outlier checked
summary(ungulates.monthly$Total)
hist(ungulates.monthly$Total)
boxplot(ungulates.hourly$Total)#971 count outlier checked
summary(ungulates.hourly$Total)
hist(ungulates.hourly$Total)
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
acf(car.ann.count.jump) #plot acf
acf(car.ann.count.under)
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
acf(car.mon.count.jump) 
acf(car.mon.count.under)
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
acf(car.hou.count.jump) 
acf(car.hou.count.under)
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
acf(ung.ann.count.jump) 
acf(ung.ann.count.under)
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
acf(ung.mon.count.jump)
acf(ung.mon.count.under)
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
acf(ung.hou.count.jump)
acf(ung.hou.count.under)
var(ung.hou.count.jump)
var(ung.hou.count.under)
mean(ung.hou.count.jump)
mean(ung.hou.count.under)

#-----------------------------Violin plots--------------------------------

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

#Make data circular-not sure whether to plot just the count vector or the dataset
#Vector
car.hou.count.jumpc <- circular(car.hou.count.jump, units="hours", template="clock24")
#Hour col
car.hou.count.jump.hou<-carnivores.hourly.jump$HourEnding
car.hou.count.jumpc2 <- circular(car.hou.count.jump.hou, units="hours", template="clock24")
#Whole dataset
car.hou.count.jumpc3 <- circular(carnivores.hourly.jump, units="hours", template="clock24")
#Just count and hour
car.hou.count.jump.2col<-cbind(carnivores.hourly.jump$HourEnding, carnivores.hourly.jump$Total)
car.hou.count.jumpc4 <- circular(car.hou.count.jump.2col, units="hours", template="clock24")

plot(carnivores.annual.jump$Total, carnivores.annual.jump$HourEnding)
#Plot linear data
plot(car.hou.count.jumpc, pch=16, xlab="Annual count", ylab="")
plot(car.hou.count.jumpc2, pch=16, xlab="Annual count", ylab="")
plot(car.hou.count.jumpc3, pch=16, xlab="Annual count", ylab="")
plot(car.hou.count.jumpc4, pch=16, xlab="Annual count", ylab="")








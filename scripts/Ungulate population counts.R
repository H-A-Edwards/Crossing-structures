#To ensure that decreases in ungulate population size was not biasing the number of 
#wildlife-vehicle collisions we checked for stationarity using forecast 8.2 (Hyndman 2017)
#in the the Alberta Parks annual pre-hunting season population estimates for elk (Cervus 
#canadensis), white-tailed deer (Odocoileus virginianus), mule deer (Odocoileus hemionus)
#and moose (Alces alces) between 2001 and 2017. 

rm(list = ls())

#-----------------Set working directory--------------------
setwd("/Users/hannahedwards/Documents/Alberta Parks/Crossing-structures/data/")

#----------------Load libraries----------------------------
library(dplyr)
library(ggplot2)
library(forecast)
library(tseries)
library(fitdistrplus)
library(logspline)
library(AER)

#----------------Load datasets----------------------------
#data
getwd()
setwd("/Users/hannahedwards/Documents/Alberta parks/")

####data set
Pop<-read.csv("Historical Pop Estimates Request.csv",header=T)
str(Pop)

#####Combined average of all WMUs per year
Pop1<-aggregate(Estimate~Year, data=Pop, FUN='mean')

Pop1.1 <- subset(Pop1, select = -c(Year) )###drop year for ts analysis

#####Average for each WMU per year
Pop2<-aggregate(Estimate~Year+WMU.or.AMA, data=Pop, FUN='mean')

####Plot the trend over time
plot<-ggplot(data=Pop1, aes(x=Year, y=Estimate, group=1))+
  geom_line()+
  geom_point()+
  ylab("Annual ungulate population estimate")+
  xlab("")+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black'))+
  theme(text=element_text(size=10))+
  theme(axis.text=element_text(size=10))
plot

####Plot the trend over time per WMU
str(Pop2)
Pop2$WMU.or.AMA<-as.factor(Pop2$WMU.or.AMA)

plot1<-ggplot(data=Pop2, aes(x=Year, y=Estimate, group=WMU.or.AMA, colour=WMU.or.AMA))+
  geom_line()+
  geom_point()+
  ylab("Annual ungulate population estimate")+
  xlab("")+
  theme_bw() +
  theme(panel.border = element_blank())+
  theme(axis.line = element_line(color = 'black'))+
  theme(text=element_text(size=10))+
  theme(axis.text=element_text(size=10))+
  scale_color_manual(values=c('blue', 'red', 'green', 'yellow', 'pink'))
plot1

####Time series analysis with mean pop estimate across all WMUs
Pop1time<-ts(Pop1.1, start=2001, end=2017, frequency=1)###start is when the time series starts
                                                      #End is when the time series ends
                                                      #frequency is the number of observations
                                                      ##per unit time, e.g. 1=annual, 4=quarterly
Pop1time

plot(Pop1time)
acf(Pop1time)####Used in manuscript
Pop1timedecomp=decompose(Pop1time)

###Differencing can help stabilize the mean of a time series by removing 
#changes in the level of a time series, and so eliminating trend and seasonality.
##Test whether the data are non-stationary (mean variance and autocorrelation are not
##constant over time):

adf.test(Pop1time, alternative = "stationary")##large pvalue so non-stationary

###Tests for stationarity (mean variance and autocorrelation are constant over time):
kpss.test(Pop1time)##p<0.05 suggest differencing is required-used in manuscript

###Tests
Box.test(Pop1time,type="Ljung-Box")


####HE playing around
###Fit linear models to time series including trend and seasonality components

Poptime<-ts(Pop1, start=2001, end=2017, frequency=1)##Incude year in ts
Pop1model <- tslm(Estimate ~ Year, data=Poptime)
summary(Pop1model)

####Estimate data distribution

descdist(Pop1$Estimate, discrete = FALSE)###
fit.weibull <- fitdist(Pop1$Estimate, "weibull")
plot(fit.weibull)
fit.norm <- fitdist(Pop1$Estimate, "norm")
plot(fit.norm)
fit.lnorm <- fitdist(Pop1$Estimate, "lnorm")
plot(fit.lnorm)
fit.gamma <- fitdist(Pop1$Estimate, "gamma")
plot(fit.gamma)
fit.cau <- fitdist(Pop1$Estimate, "cauchy")
plot(fit.cau)#####best distribution fit

####Cox-Stuart test (cauchy dist)
cox.stuart.test =
  function (x)
  {
    method = "Cox-Stuart test for trend analysis"
    leng = length(x)
    apross = round(leng) %% 2
    if (apross == 1) {
      delete = (length(x)+1)/2
      x = x[ -delete ] 
    }
    half = length(x)/2
    x1 = x[1:half]
    x2 = x[(half+1):(length(x))]
    difference = x1-x2
    signs = sign(difference)
    signcorr = signs[signs != 0]
    pos = signs[signs>0]
    neg = signs[signs<0]
    if (length(pos) < length(neg)) {
      prop = pcauchy(length(pos), length(signcorr), 0.5)
      names(prop) = "Increasing trend, p-value"
      rval <- list(method = method, statistic = prop)
      class(rval) = "htest"
      return(rval)
    }
    else {
      prop = pbinom(length(neg), length(signcorr), 0.5)
      names(prop) = "Decreasing trend, p-value"
      rval <- list(method = method, statistic = prop)
      class(rval) = "htest"
      return(rval)
    }
  }
cox.stuart.test(Pop1$Estimate)####No significant trend p>0.05

#####GLM to look at trend
model <- glm(Pop1$Estimate ~ Pop1$Year, family = poisson(link=log))###Count data so use poisson dist
summary(model)
dispersiontest(model,trafo=1)###Overdispersion alpha>0. against the assumption of equidispersion
model2 <- glm(Pop1$Estimate ~ Pop1$Year, family = quasipoisson(link=log))####Use a quasi poisson to 
                                                                      ###account for overdispersion
summary(model2)
####+ve coefficient with year but not significant

#####Plot trends in the data
detrended<-Pop1$Count - predict(glm(Pop1$Count~I(1:length(Pop1$Count)), family = quasipoisson(link=log))) 
ts.plot(detrended)
acf(detrended)###Looks for cyclic trends

#Script to run GLMM to look at the temporal variation in animal use of underpasses and jumpouts
#We first separate out the day/season/annual counts by structure type and explore the influence
# time of day for the day counts, season for the season counts and year for the annual counts
# We also explore the influence of vehicles and humans. For all daily/seasonal underpass models
# only the day/season parameter is explored as the sample size for these models is too small.
# The traffic varible is dropped from all models as it was collinear with the day/season/year
# parameter

rm(list = ls())

#-----------------Set working directory--------------------
setwd("/Users/hannahedwards/Documents/Alberta Parks/Crossing-structures/data/")

#----------------Load libraries----------------------------
library(MCMCglmm)
library(HDInterval)

#----------------Load datasets----------------------------

bigungulates.day <- read.csv("Bigungulates hourly daynight.csv")
bigungulates.season <- read.csv("Bigungulates seasonal.csv")
bigungulates.annual <- read.csv("Bigungulates annual.csv")

bigungulates.day.under<-subset(bigungulates.day, bigungulates.day$Underpass.type != "Jumpout")
bigungulates.day.jump<-subset(bigungulates.day, bigungulates.day$Underpass.type != "Underpass")
bigungulates.season.under<-subset(bigungulates.season, bigungulates.season$Underpass.type != "Jumpout")
bigungulates.season.jump<-subset(bigungulates.season, bigungulates.season$Underpass.type != "Underpass")
bigungulates.annual.under<-subset(bigungulates.annual, bigungulates.annual$Underpass.type != "Jumpout")
bigungulates.annual.jump<-subset(bigungulates.annual, bigungulates.annual$Underpass.type != "Underpass")

# aim to store 1,000-2,000 iterations
# have the autocorrelation between successive stored iterations less than 0.1
# fixed effects: interesting in themselves
# random effects: only underlying population is of interest

# big daily ungulates

prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))

prioexp<- list(R = list(V = 1, nu=0.002), #residuals prior
               G = list(G1 = list(V = 1,nu= 0.02,alpha.mu=0,alpha.V=1000)))


#Underpass
bigungulates.day.under$Location2<-as.factor(bigungulates.day.under$Location2)
bigungulates.day.under$daynight<-as.factor(bigungulates.day.under$daynight)

#IG prior preferred
bigungulate.day.under <- MCMCglmm(Total ~
                                      daynight,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = bigungulates.day.under)

#bigungulate.day.under.exp <- MCMCglmm(Total ~
 #                                     daynight,
  #                                  random = ~ average.effort ,
   #                                 prior = prioexp, nitt=1003000, burnin=100000, thin=500,
    #                                verbose = TRUE,
     #                               family = "poisson",
      #                              data = bigungulates.day.under)

#Summary stats
summary(bigungulate.day.under)
summary(bigungulate.day.under.exp)
plot(bigungulate.day.under)
plot(bigungulate.day.under.exp)

heidel.diag(bigungulate.day.under$VCV)
heidel.diag(bigungulate.day.under.exp$VCV)
heidel.diag(bigungulate.day.under$Sol)
heidel.diag(bigungulate.day.under.exp$Sol)
autocorr(bigungulate.day.under$Sol)
autocorr(bigungulate.day.under$VCV)
geweke.diag(bigungulate.day.under$Sol) # Should be between +-1.96
geweke.diag(bigungulate.day.under.exp$Sol)

effectiveSize(bigungulate.day.under$Sol)
effectiveSize(bigungulate.day.under$VCV)
posterior.mode(bigungulate.day.under$Sol)
HPDinterval(bigungulate.day.under$Sol)
posterior.mode(bigungulate.day.under$VCV)
HPDinterval(bigungulate.day.under$VCV)

#VIF function, higher than 4 it is too correlated

vif.MCMCglmm <- function (fit, intercept.columns = c(1)) {
  nF <- fit$Fixed$nfl
  v <- cov(as.matrix(fit$X[,1:nF]))
  nam <- colnames(fit$Sol[,1:nF])
  
  v <- v[-intercept.columns, -intercept.columns, drop = FALSE]
  nam <- nam[-intercept.columns]
  
  d <- diag(v)^0.5
  v <- diag(solve(v/(d %o% d)))
  names(v) <- nam
  v
}

vif.MCMCglmm(bigungulate.day.under)
vif.MCMCglmm(bigungulate.day.under.exp)

#jumpout
bigungulates.day.jump$Location2<-as.factor(bigungulates.day.jump$Location2)
bigungulates.day.jump$daynight<-as.factor(bigungulates.day.jump$daynight)

#IG prior preferred
bigungulate.day.jump <- MCMCglmm(Total ~
                                      daynight + Location2 +
                                      +daynight.human,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = bigungulates.day.jump)


#bigungulate.day.jump.exp <- MCMCglmm(Total ~
 #                                         daynight + Location2 +
  #                                        +daynight.human,
   #                                     random = ~ average.effort ,
    #                                    prior = prioexp, nitt=1003000, burnin=100000, thin=500,
     #                                   verbose = TRUE,
      #                                  family = "poisson",
       #                                 data = bigungulates.day.jump)



summary(bigungulate.day.jump)
summary(bigungulate.day.jump.exp)
plot(bigungulate.day.jump)
plot(bigungulate.day.jump.exp)

heidel.diag(bigungulate.day.jump$VCV)
heidel.diag(bigungulate.day.jump.exp$VCV)
heidel.diag(bigungulate.day.jump$Sol)
heidel.diag(bigungulate.day.jump.exp$Sol)
autocorr(bigungulate.day.jump$Sol)
autocorr(bigungulate.day.jump$VCV)
geweke.diag(bigungulate.day.jump$Sol) # Should be between +-1.96
geweke.diag(bigungulate.day.jump.exp$Sol)

effectiveSize(bigungulate.day.jump$Sol)
effectiveSize(bigungulate.day.jump$VCV)
posterior.mode(bigungulate.day.jump$Sol)
HPDinterval(bigungulate.day.jump$Sol)
posterior.mode(bigungulate.day.jump$VCV)
HPDinterval(bigungulate.day.jump$VCV)

vif.MCMCglmm(bigungulate.day.jump)
vif.MCMCglmm(bigungulate.day.jump.exp)

# big seasonal ungulates
#Underpass
bigungulates.season.under$Location2<-as.factor(bigungulates.season.under$Location2)
bigungulates.season.under$Season<-as.factor(bigungulates.season.under$Season)

#IG prior preferred
bigungulate.season.under <- MCMCglmm(Total ~
                                         Season,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = bigungulates.season.under)


#bigungulate.season.under.exp <- MCMCglmm(Total ~
 #                                         Season,
  #                                      random = ~ average.effort ,
   #                                     prior = prioexp, nitt=1003000, burnin=100000, thin=500,
    #                                    verbose = TRUE,
     #                                   family = "poisson",
      #                                  data = bigungulates.season.under)


summary(bigungulate.season.under)
summary(bigungulate.season.under.exp)
plot(bigungulate.season.under)
plot(bigungulate.season.under.exp)

heidel.diag(bigungulate.season.under$VCV)
heidel.diag(bigungulate.season.under.exp$VCV)
heidel.diag(bigungulate.season.under$Sol)
heidel.diag(bigungulate.season.under.exp$Sol)
autocorr(bigungulate.season.under$Sol)
autocorr(bigungulate.season.under$VCV)
geweke.diag(bigungulate.season.under$Sol) # Should be between +-1.96
geweke.diag(bigungulate.season.under.exp$Sol)

effectiveSize(bigungulate.season.under$Sol)
effectiveSize(bigungulate.season.under$VCV)
posterior.mode(bigungulate.season.under$Sol)
HPDinterval(bigungulate.season.under$Sol)
posterior.mode(bigungulate.season.under$VCV)
HPDinterval(bigungulate.season.under$VCV)

vif.MCMCglmm(bigungulate.season.under)
vif.MCMCglmm(bigungulate.season.under.exp)

#Calc levels for season

Winter<-bigungulate.season.under$Sol[,"(Intercept)"]+bigungulate.season.under$Sol[,"SeasonWinter"]
Summer<-bigungulate.season.under$Sol[,"(Intercept)"]+bigungulate.season.under$Sol[,"SeasonSummer"]
Spring<-bigungulate.season.under$Sol[,"(Intercept)"]+bigungulate.season.under$Sol[,"SeasonSpring"]
Autumn<-bigungulate.season.under$Sol[,"(Intercept)"] #If there are other fixed effects I subtract them? # what do you mean?

logT6<-cbind(Autumn,Winter,Spring,Summer) # maybe better to oder cronologically

# you're running a  family = "poisson", so to backtransform you need the exp.
data_scale <- round(data.frame(mean=round(exp(colMeans(logT6)), 2), 
                                    lower=t(exp(hdi(logT6, credMass = 0.95)))[, 1], 
                                    upper=t(exp(hdi(logT6, credMass = 0.95)))[, 2]), 2)
# alternatively you can leave the results in the log scale
log_scale <- round(data.frame(mean=round(colMeans(logT6), 2), 
                                        lower=t(hdi(logT6, credMass = 0.95))[, 1], 
                                        upper=t(hdi(logT6, credMass = 0.95))[, 2]), 2)

#jumpouts
bigungulates.season.jump$Location2<-as.factor(bigungulates.season.jump$Location2)
bigungulates.season.jump$Season<-as.factor(bigungulates.season.jump$Season)

#IG prior preferred
bigungulate.season.jump <- MCMCglmm(Total ~
                                         Season + Location2 +
                                         seasonal.human,
                                       random = ~ average.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = bigungulates.season.jump)


#bigungulate.season.jump.exp <- MCMCglmm(Total ~
 #                                            Season + Location2 +
  #                                           seasonal.human,
   #                                        random = ~ average.effort ,
    #                                       prior = prioexp, nitt=1003000, burnin=100000, thin=500,
     #                                      verbose = TRUE,
      #                                     family = "poisson",
       #                                    data = bigungulates.season.jump)


summary(bigungulate.season.jump)
summary(bigungulate.season.jump.exp)
plot(bigungulate.season.jump)
plot(bigungulate.season.jump.exp)

heidel.diag(bigungulate.season.jump$VCV)
heidel.diag(bigungulate.season.jump.exp$VCV)
heidel.diag(bigungulate.season.jump$Sol)
heidel.diag(bigungulate.season.jump.exp$Sol)
autocorr(bigungulate.season.jump$Sol)
autocorr(bigungulate.season.jump$VCV)
geweke.diag(bigungulate.season.jump$Sol) # Should be between +-1.96
geweke.diag(bigungulate.season.jump.exp$Sol)

effectiveSize(bigungulate.season.jump$Sol)
effectiveSize(bigungulate.season.jump$VCV)
posterior.mode(bigungulate.season.jump$Sol)
HPDinterval(bigungulate.season.jump$Sol)
posterior.mode(bigungulate.season.jump$VCV)
HPDinterval(bigungulate.season.jump$VCV)

vif.MCMCglmm(bigungulate.season.jump)
vif.MCMCglmm(bigungulate.season.jump.exp)

# big annual ungulates
#Underpass
bigungulates.annual.under$Location2<-as.factor(bigungulates.annual.under$Location2)

#IG prior preferred
bigungulate.annual.under <- MCMCglmm(Total ~
                                         Year + Location2 +
                                         annual.human,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = bigungulates.annual.under)


#bigungulate.annual.under.exp <- MCMCglmm(Total ~
 #                                             Year + Location2 +
  #                                           annual.human,
   #                                        random = ~ annual.effort ,
    #                                       prior = prioexp, nitt=1003000, burnin=100000, thin=500,
     #                                      verbose = TRUE,
      #                                     family = "poisson",
       #                                    data = bigungulates.annual.under)


summary(bigungulate.annual.under)
summary(bigungulate.annual.under.exp)
plot(bigungulate.annual.under)
plot(bigungulate.annual.under.exp)

heidel.diag(bigungulate.annual.under$VCV)
heidel.diag(bigungulate.annual.under.exp$VCV)
heidel.diag(bigungulate.annual.under$Sol)
heidel.diag(bigungulate.annual.under.exp$Sol)
autocorr(bigungulate.annual.under$Sol)
autocorr(bigungulate.annual.under$VCV)
geweke.diag(bigungulate.annual.under$Sol) # Should be between +-1.96
geweke.diag(bigungulate.annual.under.exp$Sol)

effectiveSize(bigungulate.annual.under$Sol)
effectiveSize(bigungulate.annual.under$VCV)
posterior.mode(bigungulate.annual.under$Sol)
HPDinterval(bigungulate.annual.under$Sol)
posterior.mode(bigungulate.annual.under$VCV)
HPDinterval(bigungulate.annual.under$VCV)

vif.MCMCglmm(bigungulate.annual.under)
vif.MCMCglmm(bigungulate.annual.under.exp)

#Jumpouts
bigungulates.annual.jump$Location2<-as.factor(bigungulates.annual.jump$Location2)

#IG prior preferred
bigungulate.annual.jump <- MCMCglmm(Total ~
                                         Year + Location2 +
                                         annual.human,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = bigungulates.annual.jump)


#bigungulate.annual.jump.exp <- MCMCglmm(Total ~
 #                                            Year + Location2 +
  #                                           annual.human,
   #                                        random = ~ annual.effort ,
    #                                       prior = prioexp, nitt=1003000, burnin=100000, thin=500,
     #                                      verbose = TRUE,
      #                                     family = "poisson",
       #                                    data = bigungulates.annual.jump)


summary(bigungulate.annual.jump)
summary(bigungulate.annual.jump.exp)
plot(bigungulate.annual.jump)
plot(bigungulate.annual.jump.exp)

heidel.diag(bigungulate.annual.jump$VCV)
heidel.diag(bigungulate.annual.jump.exp$VCV)
heidel.diag(bigungulate.annual.jump$Sol)
heidel.diag(bigungulate.annual.jump.exp$Sol)
autocorr(bigungulate.annual.jump$Sol)
autocorr(bigungulate.annual.jump$VCV)
geweke.diag(bigungulate.annual.jump$Sol) # Should be between +-1.96
geweke.diag(bigungulate.annual.jump.exp$Sol)

effectiveSize(bigungulate.annual.jump$Sol)
effectiveSize(bigungulate.annual.jump$VCV)
posterior.mode(bigungulate.annual.jump$Sol)
HPDinterval(bigungulate.annual.jump$Sol)
posterior.mode(bigungulate.annual.jump$VCV)
HPDinterval(bigungulate.annual.jump$VCV)

vif.MCMCglmm(bigungulate.annual.jump)
vif.MCMCglmm(bigungulate.annual.jump.exp)

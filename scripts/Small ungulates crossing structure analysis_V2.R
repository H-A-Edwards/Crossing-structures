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

#----------------Load datasets----------------------------

smallungulates.day <- read.csv("Smallungulates hourly daynight.csv")
smallungulates.season <- read.csv("Smallungulates seasonal.csv")
smallungulates.annual <- read.csv("Smallungulates annual.csv")

#Separate out by crossing structure type
smallungulates.day.under<-subset(smallungulates.day, smallungulates.day$Underpass.type != "Jumpout")
smallungulates.day.jump<-subset(smallungulates.day, smallungulates.day$Underpass.type != "Underpass")
smallungulates.season.under<-subset(smallungulates.season, smallungulates.season$Underpass.type != "Jumpout")
smallungulates.season.jump<-subset(smallungulates.season, smallungulates.season$Underpass.type != "Underpass")
smallungulates.annual.under<-subset(smallungulates.annual, smallungulates.annual$Underpass.type != "Jumpout")
smallungulates.annual.jump<-subset(smallungulates.annual, smallungulates.annual$Underpass.type != "Underpass")

# aim to store 1,000-2,000 iterations
# have the autocorrelation between successive stored iterations less than 0.1
# fixed effects: interesting in themselves
# random effects: only underlying population is of interest

# Small daily ungulates

prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))

prioexp<- list(R = list(V = 1, nu=0.002), #residuals prior
               G = list(G1 = list(V = 1,nu= 0.02,alpha.mu=0,alpha.V=1000)))


#Underpass
smallungulates.day.under$Location2<-as.factor(smallungulates.day.under$Location2)
smallungulates.day.under$daynight<-as.factor(smallungulates.day.under$daynight)

#IG prior preferred
smallungulate.day.under <- MCMCglmm(Total ~
                                      daynight,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallungulates.day.under)

#smallungulate.day.under.exp <- MCMCglmm(Total ~
 #                                     daynight,
  #                                  random = ~ average.effort ,
   #                                 prior = prioexp, nitt=1003000, burnin=100000, thin=500,
    #                                verbose = TRUE,
     #                               family = "poisson",
      #                              data = smallungulates.day.under)

#Summary stats
summary(smallungulate.day.under)
summary(smallungulate.day.under.exp)
plot(smallungulate.day.under)
plot(smallungulate.day.under.exp)

heidel.diag(smallungulate.day.under$VCV)
heidel.diag(smallungulate.day.under.exp$VCV)
heidel.diag(smallungulate.day.under$Sol)
heidel.diag(smallungulate.day.under.exp$Sol)
autocorr(smallungulate.day.under$Sol)
autocorr(smallungulate.day.under$VCV)
geweke.diag(smallungulate.day.under$Sol) # Should be between +-1.96
geweke.diag(smallungulate.day.under.exp$Sol) 

effectiveSize(smallungulate.day.under$Sol)
effectiveSize(smallungulate.day.under$VCV)
posterior.mode(smallungulate.day.under$Sol)
HPDinterval(smallungulate.day.under$Sol)
posterior.mode(smallungulate.day.under$VCV)
HPDinterval(smallungulate.day.under$VCV)

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

vif.MCMCglmm(smallungulate.day.under)
vif.MCMCglmm(smallungulate.day.under.exp)

#jumpout
smallungulates.day.jump$Location2<-as.factor(smallungulates.day.jump$Location2)
smallungulates.day.jump$daynight<-as.factor(smallungulates.day.jump$daynight)

#IG prior preferred
smallungulate.day.jump <- MCMCglmm(Total ~
                                      daynight + Location2 +
                                      +daynight.human,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallungulates.day.jump)


#smallungulate.day.jump.exp <- MCMCglmm(Total ~
 #                                         daynight + Location2 +
  #                                        +daynight.human,
   #                                     random = ~ average.effort ,
    #                                    prior = prioexp, nitt=1003000, burnin=100000, thin=500,
     #                                   verbose = TRUE,
      #                                  family = "poisson",
       #                                 data = smallungulates.day.jump)



summary(smallungulate.day.jump)
summary(smallungulate.day.jump.exp)
plot(smallungulate.day.jump)
plot(smallungulate.day.jump.exp)

heidel.diag(smallungulate.day.jump$VCV)
heidel.diag(smallungulate.day.jump.exp$VCV)
heidel.diag(smallungulate.day.jump$Sol)
heidel.diag(smallungulate.day.jump.exp$Sol)
autocorr(smallungulate.day.jump$Sol)
autocorr(smallungulate.day.jump$VCV)
geweke.diag(smallungulate.day.jump$Sol) # Should be between +-1.96
geweke.diag(smallungulate.day.jump.exp$Sol)

effectiveSize(smallungulate.day.jump$Sol)
effectiveSize(smallungulate.day.jump$VCV)
posterior.mode(smallungulate.day.jump$Sol)
HPDinterval(smallungulate.day.jump$Sol)
posterior.mode(smallungulate.day.jump$VCV)
HPDinterval(smallungulate.day.jump$VCV)

vif.MCMCglmm(smallungulate.day.jump)
vif.MCMCglmm(smallungulate.day.jump.exp)

# Small seasonal ungulates
#Underpass
smallungulates.season.under$Location2<-as.factor(smallungulates.season.under$Location2)
smallungulates.season.under$Season<-as.factor(smallungulates.season.under$Season)

contrasts(smallungulates.season.under$Season)
smallungulates.season.under$Season <- relevel(smallungulates.season.under$Season, ref=4)
contrasts(smallungulates.season.under$Season)


#IG prior preferred
smallungulate.season.under <- MCMCglmm(Total ~
                                         Season,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallungulates.season.under)


#smallungulate.season.under.exp <- MCMCglmm(Total ~
 #                                         Season,
  #                                      random = ~ average.effort ,
   #                                     prior = prioexp, nitt=1003000, burnin=100000, thin=500,
    #                                    verbose = TRUE,
     #                                   family = "poisson",
      #                                  data = smallungulates.season.under)


summary(smallungulate.season.under)
summary(smallungulate.season.under.exp)
plot(smallungulate.season.under)
plot(smallungulate.season.under.exp)

heidel.diag(smallungulate.season.under$VCV)
heidel.diag(smallungulate.season.under.exp$VCV)
heidel.diag(smallungulate.season.under$Sol)
heidel.diag(smallungulate.season.under.exp$Sol)
autocorr(smallungulate.season.under$Sol)
autocorr(smallungulate.season.under$VCV)
geweke.diag(smallungulate.season.under$Sol) # Should be between +-1.96
geweke.diag(smallungulate.season.under.exp$Sol)

effectiveSize(smallungulate.season.under$Sol)
effectiveSize(smallungulate.season.under$VCV)
posterior.mode(smallungulate.season.under$Sol)
HPDinterval(smallungulate.season.under$Sol)
posterior.mode(smallungulate.season.under$VCV)
HPDinterval(smallungulate.season.under$VCV)

vif.MCMCglmm(smallungulate.season.under)
vif.MCMCglmm(smallungulate.season.under.exp)

#jumpouts
smallungulates.season.jump$Location2<-as.factor(smallungulates.season.jump$Location2)
smallungulates.season.jump$Season<-as.factor(smallungulatess.season.jump$Season)

#IG prior preferred
smallungulate.season.jump <- MCMCglmm(Total ~
                                         Season + Location2 +
                                         seasonal.human,
                                       random = ~ average.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallungulates.season.jump)


#smallungulate.season.jump.exp <- MCMCglmm(Total ~
 #                                            Season + Location2 +
  #                                           seasonal.human,
   #                                        random = ~ average.effort ,
    #                                       prior = prioexp, nitt=1003000, burnin=100000, thin=500,
     #                                      verbose = TRUE,
      #                                     family = "poisson",
       #                                    data = smallungulates.season.jump)


summary(smallungulate.season.jump)
summary(smallungulate.season.jump.exp)
plot(smallungulate.season.jump)
plot(smallungulate.season.jump.exp)

heidel.diag(smallungulate.season.jump$VCV)
heidel.diag(smallungulate.season.jump.exp$VCV)
heidel.diag(smallungulate.season.jump$Sol)
heidel.diag(smallungulate.season.jump.exp$Sol)
autocorr(smallungulate.season.jump$Sol)
autocorr(smallungulate.season.jump$VCV)
geweke.diag(smallungulate.season.jump$Sol) # Should be between +-1.96
geweke.diag(smallungulate.season.jump.exp$Sol)

effectiveSize(smallungulate.season.jump$Sol)
effectiveSize(smallungulate.season.jump$VCV)
posterior.mode(smallungulate.season.jump$Sol)
HPDinterval(smallungulate.season.jump$Sol)
posterior.mode(smallungulate.season.jump$VCV)
HPDinterval(smallungulate.season.jump$VCV)

vif.MCMCglmm(smallungulate.season.jump)
vif.MCMCglmm(smallungulate.season.jump.exp)

# Small annual ungulates
#Underpass
smallungulates.annual.under$Location2<-as.factor(smallungulates.annual.under$Location2)

#IG prior preferred
smallungulate.annual.under <- MCMCglmm(Total ~
                                         Year + Location2 +
                                         annual.human,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallungulates.annual.under)


#smallungulate.annual.under.exp <- MCMCglmm(Total ~
 #                                             Year + Location2 +
  #                                           annual.human,
   #                                        random = ~ annual.effort ,
    #                                       prior = prioexp, nitt=1003000, burnin=100000, thin=500,
     #                                      verbose = TRUE,
      #                                     family = "poisson",
       #                                    data = smallungulates.annual.under)


summary(smallungulate.annual.under)
summary(smallungulate.annual.under.exp)
plot(smallungulate.annual.under)
plot(smallungulate.annual.under.exp)

heidel.diag(smallungulate.annual.under$VCV)
heidel.diag(smallungulate.annual.under.exp$VCV)
heidel.diag(smallungulate.annual.under$Sol)
heidel.diag(smallungulate.annual.under.exp$Sol)
autocorr(smallungulate.annual.under$Sol)
autocorr(smallungulate.annual.under$VCV)
geweke.diag(smallungulate.annual.under$Sol) # Should be between +-1.96
geweke.diag(smallungulate.annual.under.exp$Sol) 

effectiveSize(smallungulate.annual.under$Sol)
effectiveSize(smallungulate.annual.under$VCV)
posterior.mode(smallungulate.annual.under$Sol)
HPDinterval(smallungulate.annual.under$Sol)
posterior.mode(smallungulate.annual.under$VCV)
HPDinterval(smallungulate.annual.under$VCV)

vif.MCMCglmm(smallungulate.annual.under)
vif.MCMCglmm(smallungulate.annual.under.exp)

#Jumpouts
smallungulates.annual.jump$Location2<-as.factor(smallungulates.annual.jump$Location2)

#IG prior preferred
smallungulate.annual.jump <- MCMCglmm(Total ~
                                         Year + Location2 +
                                         annual.human,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallungulates.annual.jump)


#smallungulate.annual.jump.exp <- MCMCglmm(Total ~
 #                                            Year + Location2 +
  #                                           annual.human,
   #                                        random = ~ annual.effort ,
    #                                       prior = prioexp, nitt=1003000, burnin=100000, thin=500,
     #                                      verbose = TRUE,
      #                                     family = "poisson",
       #                                    data = smallungulates.annual.jump)


summary(smallungulate.annual.jump)
summary(smallungulate.annual.jump.exp)
plot(smallungulate.annual.jump)
plot(smallungulate.annual.jump.exp)

heidel.diag(smallungulate.annual.jump$VCV)
heidel.diag(smallungulate.annual.jump.exp$VCV)
heidel.diag(smallungulate.annual.jump$Sol)
heidel.diag(smallungulate.annual.jump.exp$Sol)
autocorr(smallungulate.annual.jump$Sol)
autocorr(smallungulate.annual.jump$VCV)
geweke.diag(smallungulate.annual.jump$Sol) # Should be between +-1.96
geweke.diag(smallungulate.annual.jump.exp$Sol)

effectiveSize(smallungulate.annual.jump$Sol)
effectiveSize(smallungulate.annual.jump$VCV)
posterior.mode(smallungulate.annual.jump$Sol)
HPDinterval(smallungulate.annual.jump$Sol)
posterior.mode(smallungulate.annual.jump$VCV)
HPDinterval(smallungulate.annual.jump$VCV)

vif.MCMCglmm(smallungulate.annual.jump)
vif.MCMCglmm(smallungulate.annual.jump.exp)

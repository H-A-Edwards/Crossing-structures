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

smallcarnivores.day <- read.csv("Smallcarnivores hourly daynight.csv")
smallcarnivores.season <- read.csv("Smallcarnivores seasonal.csv")
smallcarnivores.annual <- read.csv("Smallcarnivores annual.csv")

#Separate out by crossing structure type
smallcarnivores.day.under<-subset(smallcarnivores.day, smallcarnivores.day$Underpass.type != "Jumpout")
smallcarnivores.day.jump<-subset(smallcarnivores.day, smallcarnivores.day$Underpass.type != "Underpass")
smallcarnivores.season.under<-subset(smallcarnivores.season, smallcarnivores.season$Underpass.type != "Jumpout")
smallcarnivores.season.jump<-subset(smallcarnivores.season, smallcarnivores.season$Underpass.type != "Underpass")
smallcarnivores.annual.under<-subset(smallcarnivores.annual, smallcarnivores.annual$Underpass.type != "Jumpout")
smallcarnivores.annual.jump<-subset(smallcarnivores.annual, smallcarnivores.annual$Underpass.type != "Underpass")

# aim to store 1,000-2,000 iterations
# have the autocorrelation between successive stored iterations less than 0.1
# fixed effects: interesting in themselves
# random effects: only underlying population is of interest

# Small daily carnivores

prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))

prioexp<- list(R = list(V = 1, nu=0.002), #residuals prior
               G = list(G1 = list(V = 1,nu= 0.02,alpha.mu=0,alpha.V=1000)))


#Underpass
smallcarnivores.day.under$Location2<-as.factor(smallcarnivores.day.under$Location2)
smallcarnivores.day.under$daynight<-as.factor(smallcarnivores.day.under$daynight)

contrasts(smallcarnivores.day.under$daynight)
smallcarnivores.day.under$daynight <- relevel(smallcarnivores.day.under$daynight, ref=2)
contrasts(smallcarnivores.day.under$daynight)

#IG prior preferred
smallcarnivore.day.under <- MCMCglmm(Total ~
                                      daynight,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallcarnivores.day.under)

#smallcarnivore.day.under.exp <- MCMCglmm(Total ~
                                  #    daynight,
                                   # random = ~ average.effort ,
                                    #prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                    #verbose = TRUE,
                                    #family = "poisson",
                                    #data = smallcarnivores.day.under)

#Summary stats
summary(smallcarnivore.day.under)
summary(smallcarnivore.day.under.exp)
plot(smallcarnivore.day.under)
plot(smallcarnivore.day.under.exp)

heidel.diag(smallcarnivore.day.under$VCV)
heidel.diag(smallcarnivore.day.under.exp$VCV)
heidel.diag(smallcarnivore.day.under$Sol)
heidel.diag(smallcarnivore.day.under.exp$Sol)
autocorr(smallcarnivore.day.under$Sol)
autocorr(smallcarnivore.day.under$VCV)
geweke.diag(smallcarnivore.day.under$Sol) # Should be between +-1.96
geweke.diag(smallcarnivore.day.under.exp$Sol)

effectiveSize(smallcarnivore.day.under$Sol)
effectiveSize(smallcarnivore.day.under$VCV)
posterior.mode(smallcarnivore.day.under$Sol)
HPDinterval(smallcarnivore.day.under$Sol)
posterior.mode(smallcarnivore.day.under$VCV)
HPDinterval(smallcarnivore.day.under$VCV)

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

vif.MCMCglmm(smallcarnivore.day.under)
vif.MCMCglmm(smallcarnivore.day.under.exp)

#jumpout
smallcarnivores.day.jump$Location2<-as.factor(smallcarnivores.day.jump$Location2)
smallcarnivores.day.jump$daynight<-as.factor(smallcarnivores.day.jump$daynight)

contrasts(smallcarnivores.day.jump$daynight)
smallcarnivores.day.jump$daynight <- relevel(smallcarnivores.day.jump$daynight, ref=2)
contrasts(smallcarnivores.day.jump$daynight)

#IG prior preferred
smallcarnivore.day.jump <- MCMCglmm(Total ~
                                      daynight + Location2 +
                                      +daynight.human,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallcarnivores.day.jump)


#smallcarnivore.day.jump.exp <- MCMCglmm(Total ~
 #                                         daynight + Location2 +
  #                                        +daynight.human,
   #                                     random = ~ average.effort ,
    #                                    prior = prioexp, nitt=1003000, burnin=100000, thin=500,
     #                                   verbose = TRUE,
      #                                  family = "poisson",
       #                                 data = smallcarnivores.day.jump)



summary(smallcarnivore.day.jump)
summary(smallcarnivore.day.jump.exp)
plot(smallcarnivore.day.jump)
plot(smallcarnivore.day.jump.exp)

heidel.diag(smallcarnivore.day.jump$VCV)
heidel.diag(smallcarnivore.day.jump.exp$VCV)
heidel.diag(smallcarnivore.day.jump$Sol)
heidel.diag(smallcarnivore.day.jump.exp$Sol)
autocorr(smallcarnivore.day.jump$Sol)
autocorr(smallcarnivore.day.jump$VCV)
geweke.diag(smallcarnivore.day.jump$Sol) # Should be between +-1.96

effectiveSize(smallcarnivore.day.jump$Sol)
effectiveSize(smallcarnivore.day.jump$VCV)
posterior.mode(smallcarnivore.day.jump$Sol)
HPDinterval(smallcarnivore.day.jump$Sol)
posterior.mode(smallcarnivore.day.jump$VCV)
HPDinterval(smallcarnivore.day.jump$VCV)

vif.MCMCglmm(smallcarnivore.day.jump)
vif.MCMCglmm(smallcarnivore.day.jump.exp)

# Small seasonal carnivores
#Underpass
smallcarnivores.season.under$Location2<-as.factor(smallcarnivores.season.under$Location2)
smallcarnivores.season.under$Season<-as.factor(smallcarnivores.season.under$Season)

contrasts(smallcarnivores.season.under$Season)
smallcarnivores.season.under$Season <- relevel(smallcarnivores.season.under$Season, ref=4)
contrasts(smallcarnivores.season.under$Season)

#IG prior preferred
smallcarnivore.season.under <- MCMCglmm(Total ~
                                         Season,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallcarnivores.season.under)


#smallcarnivore.season.under.exp <- MCMCglmm(Total ~
 #                                         Season,
  #                                      random = ~ average.effort ,
   #                                     prior = prioexp, nitt=1003000, burnin=100000, thin=500,
    #                                    verbose = TRUE,
     #                                   family = "poisson",
      #                                  data = smallcarnivores.season.under)


summary(smallcarnivore.season.under)
summary(smallcarnivore.season.under.exp)
plot(smallcarnivore.season.under)
plot(smallcarnivore.season.under.exp)

heidel.diag(smallcarnivore.season.under$VCV)
heidel.diag(smallcarnivore.season.under.exp$VCV)
heidel.diag(smallcarnivore.season.under$Sol)
heidel.diag(smallcarnivore.season.under.exp$Sol)
autocorr(smallcarnivore.season.under$Sol)
autocorr(smallcarnivore.season.under$VCV)
geweke.diag(smallcarnivore.season.under$Sol) # Should be between +-1.96
geweke.diag(smallcarnivore.season.under.exp$Sol) 

effectiveSize(smallcarnivore.season.under$Sol)
effectiveSize(smallcarnivore.season.under$VCV)
posterior.mode(smallcarnivore.season.under$Sol)
HPDinterval(smallcarnivore.season.under$Sol)
posterior.mode(smallcarnivore.season.under$VCV)
HPDinterval(smallcarnivore.season.under$VCV)

vif.MCMCglmm(smallcarnivore.season.under)
vif.MCMCglmm(smallcarnivore.season.under.exp)

#jumpouts
smallcarnivores.season.jump$Location2<-as.factor(smallcarnivores.season.jump$Location2)
smallcarnivores.season.jump$Season<-as.factor(smallcarnivores.season.jump$Season)

contrasts(smallcarnivores.season.jump$Season)
smallcarnivores.season.jump$Season <- relevel(smallcarnivores.season.jump$Season, ref=3)
contrasts(smallcarnivores.season.jump$Season)

#IG prior preferred
smallcarnivore.season.jump <- MCMCglmm(Total ~
                                         Season + Location2 +
                                         seasonal.human,
                                       random = ~ average.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallcarnivores.season.jump)


#smallcarnivore.season.jump.exp <- MCMCglmm(Total ~
 #                                            Season + Location2 +
  #                                           seasonal.human,
   #                                        random = ~ average.effort ,
    #                                       prior = prioexp, nitt=1003000, burnin=100000, thin=500,
     #                                      verbose = TRUE,
      #                                     family = "poisson",
       #                                    data = smallcarnivores.season.jump)


summary(smallcarnivore.season.jump)
summary(smallcarnivore.season.jump.exp)
plot(smallcarnivore.season.jump)
plot(smallcarnivore.season.jump.exp)

heidel.diag(smallcarnivore.season.jump$VCV)
heidel.diag(smallcarnivore.season.jump.exp$VCV)
heidel.diag(smallcarnivore.season.jump$Sol)
heidel.diag(smallcarnivore.season.jump.exp$Sol)
autocorr(smallcarnivore.season.jump$Sol)
autocorr(smallcarnivore.season.jump$VCV)
geweke.diag(smallcarnivore.season.jump$Sol) # Should be between +-1.96
geweke.diag(smallcarnivore.season.jump.exp$Sol)

effectiveSize(smallcarnivore.season.jump$Sol)
effectiveSize(smallcarnivore.season.jump$VCV)
posterior.mode(smallcarnivore.season.jump$Sol)
HPDinterval(smallcarnivore.season.jump$Sol)
posterior.mode(smallcarnivore.season.jump$VCV)
HPDinterval(smallcarnivore.season.jump$VCV)

vif.MCMCglmm(smallcarnivore.season.jump)
vif.MCMCglmm(smallcarnivore.season.jump.exp)

# Small annual carnivores
#Underpass
smallcarnivores.annual.under$Location2<-as.factor(smallcarnivores.annual.under$Location2)

#IG prior preferred
smallcarnivore.annual.under <- MCMCglmm(Total ~
                                         Year + Location2,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallcarnivores.annual.under)


#smallcarnivore.annual.under.exp <- MCMCglmm(Total ~
 #                                             Year + Location2 +
  #                                           annual.human,
   #                                       random = ~ annual.effort ,
   #                                        prior = prioexp, nitt=1003000, burnin=100000, thin=500,
    #                                       verbose = TRUE,
     #                                      family = "poisson",
      #                                     data = smallcarnivores.annual.under)


summary(smallcarnivore.annual.under)
summary(smallcarnivore.annual.under.exp)
plot(smallcarnivore.annual.under)
plot(smallcarnivore.annual.under.exp)

heidel.diag(smallcarnivore.annual.under$VCV)
heidel.diag(smallcarnivore.annual.under.exp$VCV)
heidel.diag(smallcarnivore.annual.under$Sol)
heidel.diag(smallcarnivore.annual.under.exp$Sol)
autocorr(smallcarnivore.annual.under$Sol)
autocorr(smallcarnivore.annual.under$VCV)
geweke.diag(smallcarnivore.annual.under$Sol) # Should be between +-1.96
geweke.diag(smallcarnivore.annual.under.exp$Sol)

effectiveSize(smallcarnivore.annual.under$Sol)
effectiveSize(smallcarnivore.annual.under$VCV)
posterior.mode(smallcarnivore.annual.under$Sol)
HPDinterval(smallcarnivore.annual.under$Sol)
posterior.mode(smallcarnivore.annual.under$VCV)
HPDinterval(smallcarnivore.annual.under$VCV)

vif.MCMCglmm(smallcarnivore.annual.under)
vif.MCMCglmm(smallcarnivore.annual.under.exp)

#Jumpouts
smallcarnivores.annual.jump$Location2<-as.factor(smallcarnivores.annual.jump$Location2)

#IG preferred
smallcarnivore.annual.jump <- MCMCglmm(Total ~
                                         Year + Location2,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallcarnivores.annual.jump)


#smallcarnivore.annual.jump.exp <- MCMCglmm(Total ~
 #                                            Year + Location2 +
  #                                           annual.human,
   #                                        random = ~ annual.effort ,
    #                                       prior = prioexp, nitt=1003000, burnin=100000, thin=500,
     #                                      verbose = TRUE,
      #                                     family = "poisson",
       #                                    data = smallcarnivores.annual.jump)


summary(smallcarnivore.annual.jump)
summary(smallcarnivore.annual.jump.exp)
plot(smallcarnivore.annual.jump)
plot(smallcarnivore.annual.jump.exp)

heidel.diag(smallcarnivore.annual.jump$VCV)
heidel.diag(smallcarnivore.annual.jump.exp$VCV)
heidel.diag(smallcarnivore.annual.jump$Sol)
heidel.diag(smallcarnivore.annual.jump.exp$Sol)
autocorr(smallcarnivore.annual.jump$Sol)
autocorr(smallcarnivore.annual.jump$VCV)
geweke.diag(smallcarnivore.annual.jump$Sol) # Should be between +-1.96
geweke.diag(smallcarnivore.annual.jump.exp$Sol)

effectiveSize(smallcarnivore.annual.jump$Sol)
effectiveSize(smallcarnivore.annual.jump$VCV)
posterior.mode(smallcarnivore.annual.jump$Sol)
HPDinterval(smallcarnivore.annual.jump$Sol)
posterior.mode(smallcarnivore.annual.jump$VCV)
HPDinterval(smallcarnivore.annual.jump$VCV)

vif.MCMCglmm(smallcarnivore.annual.jump)
vif.MCMCglmm(smallcarnivore.annual.jump.exp)

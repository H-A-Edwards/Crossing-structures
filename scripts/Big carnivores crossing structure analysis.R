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

bigcarnivores.day <- read.csv("Bigcarnivores hourly daynight.csv")
bigcarnivores.season <- read.csv("Bigcarnivores seasonal.csv")
bigcarnivores.annual <- read.csv("Bigcarnivores annual.csv")

#Separate out by crossing structure type
#Did not run for big carnivore jumpout sample size small use circular plots as evidence
bigcarnivores.day.under<-subset(bigcarnivores.day, bigcarnivores.day$Underpass.type != "Jumpout")
#bigcarnivores.day.jump<-subset(bigcarnivores.day, bigcarnivores.day$Underpass.type != "Underpass")
bigcarnivores.season.under<-subset(bigcarnivores.season, bigcarnivores.season$Underpass.type != "Jumpout")
#bigcarnivores.season.jump<-subset(bigcarnivores.season, bigcarnivores.season$Underpass.type != "Underpass")
bigcarnivores.annual.under<-subset(bigcarnivores.annual, bigcarnivores.annual$Underpass.type != "Jumpout")
bigcarnivores.annual.jump<-subset(bigcarnivores.annual, bigcarnivores.annual$Underpass.type != "Underpass")

# aim to store 1,000-2,000 iterations
# have the autocorrelation between successive stored iterations less than 0.1
# fixed effects: interesting in themselves
# random effects: only underlying population is of interest

# big daily carnivores

prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))

prioexp<- list(R = list(V = 1, nu=0.002), #residuals prior
               G = list(G1 = list(V = 1,nu= 0.02,alpha.mu=0,alpha.V=1000)))


#Underpass
bigcarnivores.day.under$Location2<-as.factor(bigcarnivores.day.under$Location2)
bigcarnivores.day.under$daynight<-as.factor(bigcarnivores.day.under$daynight)
#Change day/night comparison
contrasts(bigcarnivores.day.under$daynight)
bigcarnivores.day.under$daynight <- relevel(bigcarnivores.day.under$daynight, ref=2)
contrasts(bigcarnivores.day.under$daynight)

#IG favoured model
bigcarnivore.day.under <- MCMCglmm(Total ~
                                      daynight,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = bigcarnivores.day.under)

#bigcarnivore.day.under.exp <- MCMCglmm(Total ~
                                    #  daynight,
                                    #random = ~ average.effort ,
                                    #prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                    #verbose = TRUE,
                                    #family = "poisson",
                                    #data = bigcarnivores.day.under)

#Summary stats
summary(bigcarnivore.day.under)
summary(bigcarnivore.day.under.exp)
plot(bigcarnivore.day.under)
plot(bigcarnivore.day.under.exp)

heidel.diag(bigcarnivore.day.under$VCV)
heidel.diag(bigcarnivore.day.under.exp$VCV)
heidel.diag(bigcarnivore.day.under$Sol)
heidel.diag(bigcarnivore.day.under.exp$Sol)
autocorr(bigcarnivore.day.under$Sol)
autocorr(bigcarnivore.day.under$VCV)
geweke.diag(bigcarnivore.day.under$Sol) # Should be between +-1.96

effectiveSize(bigcarnivore.day.under$Sol)
effectiveSize(bigcarnivore.day.under$VCV)
posterior.mode(bigcarnivore.day.under$Sol)
HPDinterval(bigcarnivore.day.under$Sol)
posterior.mode(bigcarnivore.day.under$VCV)
HPDinterval(bigcarnivore.day.under$VCV)

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

vif.MCMCglmm(bigcarnivore.day.under)
vif.MCMCglmm(bigcarnivore.day.under.exp)

#jumpout
bigcarnivores.day.jump$Location2<-as.factor(bigcarnivores.day.jump$Location2)
bigcarnivores.day.jump$daynight<-as.factor(bigcarnivores.day.jump$daynight)

bigcarnivore.day.jump <- MCMCglmm(Total ~
                                      daynight + Location2 +
                                      +daynight.human,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = bigcarnivores.day.jump)


bigcarnivore.day.jump.exp <- MCMCglmm(Total ~
                                          daynight + Location2 +
                                          +daynight.human,
                                        random = ~ average.effort ,
                                        prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                        verbose = TRUE,
                                        family = "poisson",
                                        data = bigcarnivores.day.jump)



summary(bigcarnivore.day.jump)
summary(bigcarnivore.day.jump.exp)
plot(bigcarnivore.day.jump)
plot(bigcarnivore.day.jump.exp)

heidel.diag(bigcarnivore.day.jump$VCV)
heidel.diag(bigcarnivore.day.jump$Sol)
autocorr(bigcarnivore.day.jump$Sol)
autocorr(bigcarnivore.day.jump$VCV)
geweke.diag(bigcarnivore.day.jump$Sol) # Should be between +-1.96

effectiveSize(bigcarnivore.day.jump$Sol)
effectiveSize(bigcarnivore.day.jump$VCV)
posterior.mode(bigcarnivore.day.jump$Sol)
HPDinterval(bigcarnivore.day.jump$Sol)
posterior.mode(bigcarnivore.day.jump$VCV)
HPDinterval(bigcarnivore.day.jump$VCV)

vif.MCMCglmm(bigcarnivore.day.jump)
vif.MCMCglmm(bigcarnivore.day.jump.exp)

# big seasonal carnivores
#Underpass
bigcarnivores.season.under$Location2<-as.factor(bigcarnivores.season.under$Location2)
bigcarnivores.season.under$Season<-as.factor(bigcarnivores.season.under$Season)

contrasts(bigcarnivores.season.under$Season)
bigcarnivores.season.under$Season <- relevel(bigcarnivores.season.under$Season, ref=3)
contrasts(bigcarnivores.season.under$Season)

#IG prior preffered
bigcarnivore.season.under <- MCMCglmm(Total ~
                                         Season,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = bigcarnivores.season.under)


#bigcarnivore.season.under.exp <- MCMCglmm(Total ~
                                        #  Season,
                                        #random = ~ average.effort ,
                                        #prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                        #verbose = TRUE,
                                        #family = "poisson",
                                        #data = bigcarnivores.season.under)


summary(bigcarnivore.season.under)
summary(bigcarnivore.season.under.exp)
plot(bigcarnivore.season.under)
plot(bigcarnivore.season.under.exp)

heidel.diag(bigcarnivore.season.under$VCV)
heidel.diag(bigcarnivore.season.under.exp$VCV)
heidel.diag(bigcarnivore.season.under$Sol)
heidel.diag(bigcarnivore.season.under.exp$Sol)
autocorr(bigcarnivore.season.under$Sol)
autocorr(bigcarnivore.season.under$VCV)
geweke.diag(bigcarnivore.season.under$Sol) # Should be between +-1.96

effectiveSize(bigcarnivore.season.under$Sol)
effectiveSize(bigcarnivore.season.under$VCV)
posterior.mode(bigcarnivore.season.under$Sol)
HPDinterval(bigcarnivore.season.under$Sol)
posterior.mode(bigcarnivore.season.under$VCV)
HPDinterval(bigcarnivore.season.under$VCV)

vif.MCMCglmm(bigcarnivore.season.under)
vif.MCMCglmm(bigcarnivore.season.under.exp)

#jumpouts
bigcarnivores.season.jump$Location2<-as.factor(bigcarnivoress.season.jump$Location2)
bigcarnivores.season.jump$Season<-as.factor(bigcarnivores.season.jump$Season)

#IG prior preferred
bigcarnivore.season.jump <- MCMCglmm(Total ~
                                         Season + Location2 +
                                         seasonal.human,
                                       random = ~ average.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = bigcarnivores.season.jump)


#bigcarnivore.season.jump.exp <- MCMCglmm(Total ~
#                                             Season + Location2 +
#                                             seasonal.human,
#                                           random = ~ average.effort ,
#                                           prior = prioexp, nitt=1003000, burnin=100000, thin=500,
#                                           verbose = TRUE,
#                                           family = "poisson",
#                                           data = bigcarnivores.season.jump)


summary(bigcarnivore.season.jump)
summary(bigcarnivore.season.jump.exp)
plot(bigcarnivore.season.jump)
plot(bigcarnivore.season.jump.exp)

heidel.diag(bigcarnivore.season.jump$VCV)
heidel.diag(bigcarnivore.season.jump$Sol)
autocorr(bigcarnivore.season.jump$Sol)
autocorr(bigcarnivore.season.jump$VCV)
geweke.diag(bigcarnivore.season.jump$Sol) # Should be between +-1.96

effectiveSize(bigcarnivore.season.jump$Sol)
effectiveSize(bigcarnivore.season.jump$VCV)
posterior.mode(bigcarnivore.season.jump$Sol)
HPDinterval(bigcarnivore.season.jump$Sol)
posterior.mode(bigcarnivore.season.jump$VCV)
HPDinterval(bigcarnivore.season.jump$VCV)

vif.MCMCglmm(bigcarnivore.season.jump)
vif.MCMCglmm(bigcarnivore.season.jump.exp)

# big annual carnivores
#Underpass
bigcarnivores.annual.under$Location2<-as.factor(bigcarnivores.annual.under$Location2)


#IG prior preferred
bigcarnivore.annual.under <- MCMCglmm(Total ~
                                         Year + Location2 +
                                         annual.human,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = bigcarnivores.annual.under)


#bigcarnivore.annual.under.exp <- MCMCglmm(Total ~
                                            #  Year + Location2 +
                                            # annual.human,
                                           #random = ~ annual.effort ,
                                           #prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                           #verbose = TRUE,
                                           #family = "poisson",
                                           #data = bigcarnivores.annual.under)


summary(bigcarnivore.annual.under)
summary(bigcarnivore.annual.under.exp)
plot(bigcarnivore.annual.under)
plot(bigcarnivore.annual.under.exp)

heidel.diag(bigcarnivore.annual.under$VCV)
heidel.diag(bigcarnivore.annual.under.exp$VCV)
heidel.diag(bigcarnivore.annual.under$Sol)
heidel.diag(bigcarnivore.annual.under.exp$Sol)
autocorr(bigcarnivore.annual.under$Sol)
autocorr(bigcarnivore.annual.under$VCV)
geweke.diag(bigcarnivore.annual.under$Sol) # Should be between +-1.96
geweke.diag(bigcarnivore.annual.under.exp$Sol)

effectiveSize(bigcarnivore.annual.under$Sol)
effectiveSize(bigcarnivore.annual.under$VCV)
posterior.mode(bigcarnivore.annual.under$Sol)
HPDinterval(bigcarnivore.annual.under$Sol)
posterior.mode(bigcarnivore.annual.under$VCV)
HPDinterval(bigcarnivore.annual.under$VCV)

vif.MCMCglmm(bigcarnivore.annual.under)
vif.MCMCglmm(bigcarnivore.annual.under.exp)

#Jumpouts
bigcarnivores.annual.jump$Location2<-as.factor(bigcarnivores.annual.jump$Location2)

#IG prior preferred
bigcarnivore.annual.jump <- MCMCglmm(Total ~
                                         Year + Location2 +
                                         annual.human,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = bigcarnivores.annual.jump)


#bigcarnivore.annual.jump.exp <- MCMCglmm(Total ~
                                           #  Year + Location2 +
                                            # annual.human,
                                           #random = ~ annual.effort ,
                                           #prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                           #verbose = TRUE,
                                           #family = "poisson",
                                           #data = bigcarnivores.annual.jump)


summary(bigcarnivore.annual.jump)
summary(bigcarnivore.annual.jump.exp)
plot(bigcarnivore.annual.jump)
plot(bigcarnivore.annual.jump.exp)

heidel.diag(bigcarnivore.annual.jump$VCV)
heidel.diag(bigcarnivore.annual.jump.exp$VCV)
heidel.diag(bigcarnivore.annual.jump$Sol)
heidel.diag(bigcarnivore.annual.jump.exp$Sol)
autocorr(bigcarnivore.annual.jump$Sol)
autocorr(bigcarnivore.annual.jump$VCV)
geweke.diag(bigcarnivore.annual.jump$Sol) # Should be between +-1.96
geweke.diag(bigcarnivore.annual.jump.exp$Sol)

effectiveSize(bigcarnivore.annual.jump$Sol)
effectiveSize(bigcarnivore.annual.jump$VCV)
posterior.mode(bigcarnivore.annual.jump$Sol)
HPDinterval(bigcarnivore.annual.jump$Sol)
posterior.mode(bigcarnivore.annual.jump$VCV)
HPDinterval(bigcarnivore.annual.jump$VCV)

vif.MCMCglmm(bigcarnivore.annual.jump)
vif.MCMCglmm(bigcarnivore.annual.jump.exp)

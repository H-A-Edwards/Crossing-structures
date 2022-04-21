#Script to run GLMM to look at the temporal variation in animal use of underpasses and jumpouts
#We first separate out the day/season/annual counts by structure type and explore the influence
# time of day for the day counts, season for the season counts and year for the annual counts
# We also explore the influence of vehicles and humans 

rm(list = ls())

#-----------------Set working directory--------------------
setwd("/Users/hannahedwards/Documents/Alberta Parks/Crossing-structures/data/")

#----------------Load libraries----------------------------
library(MCMCglmm)

#----------------Load datasets----------------------------

smallungulates.day <- read.csv("Smallungulates hourly daynight.csv")
smallungulates.season <- read.csv("Smallungulates seasonal.csv")
smallungulates.annual <- read.csv("Smallungulates annual.csv")

bigungulates.day <- read.csv("Bigungulates hourly daynight.csv")
bigungulates.season <- read.csv("Bigungulates seasonal.csv")
bigungulates.annual <- read.csv("Bigungulates annual.csv")

smallcarnivores.day <- read.csv("Smallcarnivores hourly daynight.csv")
smallcarnivores.season <- read.csv("Smallcarnivores seasonal.csv")
smallcarnivores.annual <- read.csv("Smallcarnivores annual.csv")

bigcarnivores.day <- read.csv("Bigcarnivores hourly daynight.csv")
bigcarnivores.season <- read.csv("Bigcarnivores seasonal.csv")
bigcarnivores.annual <- read.csv("Bigcarnivores annual.csv")

#Separate out by crossing structure type
smallungulates.day.under<-subset(smallungulates.day, smallungulates.day$Underpass.type != "Jumpout")
smallungulates.day.jump<-subset(smallungulates.day, smallungulates.day$Underpass.type != "Underpass")
smallungulates.season.under<-subset(smallungulates.season, smallungulates.season$Underpass.type != "Jumpout")
smallungulates.season.jump<-subset(smallungulates.season, smallungulates.season$Underpass.type != "Underpass")
smallungulates.annual.under<-subset(smallungulates.annual, smallungulates.annual$Underpass.type != "Jumpout")
smallungulates.annual.jump<-subset(smallungulates.annual, smallungulates.annual$Underpass.type != "Underpass")

bigungulates.day.under<-subset(bigungulates.day, bigungulates.day$Underpass.type != "Jumpout")
bigungulates.day.jump<-subset(bigungulates.day, bigungulates.day$Underpass.type != "Underpass")
bigungulates.season.under<-subset(bigungulates.season, bigungulates.season$Underpass.type != "Jumpout")
bigungulates.season.jump<-subset(bigungulates.season, bigungulates.season$Underpass.type != "Underpass")
bigungulates.annual.under<-subset(bigungulates.annual, bigungulates.annual$Underpass.type != "Jumpout")
bigungulates.annual.jump<-subset(bigungulates.annual, bigungulates.annual$Underpass.type != "Underpass")

smallcarnivores.day.under<-subset(smallcarnivores.day, smallcarnivores.day$Underpass.type != "Jumpout")
smallcarnivores.day.jump<-subset(smallcarnivores.day, smallcarnivores.day$Underpass.type != "Underpass")
smallcarnivores.season.under<-subset(smallcarnivores.season, smallcarnivores.season$Underpass.type != "Jumpout")
smallcarnivores.season.jump<-subset(smallcarnivores.season, smallcarnivores.season$Underpass.type != "Underpass")
smallcarnivores.annual.under<-subset(smallcarnivores.annual, smallcarnivores.annual$Underpass.type != "Jumpout")
smallcarnivores.annual.jump<-subset(smallcarnivores.annual, smallcarnivores.annual$Underpass.type != "Underpass")

#Did not run for big carnivore jumput sample size small use circular plots as evidence
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

# Small daily ungulates

prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))

prioexp<- list(R = list(V = 1, nu=0.002), #residuals prior
               G = list(G1 = list(V = 1,nu= 0.02,alpha.mu=0,alpha.V=1000)))


#Underpass
smallungulates.day.under$Location2<-as.factor(smallcarnivores.day.under$Location2)
smallungulates.day.under$daynight<-as.factor(smallcarnivores.day.under$daynight)

smallungulate.day.under <- MCMCglmm(Total ~
                                      daynight + Location2 +
                                      daynight.traffic+daynight.human,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallungulates.day.under)


smallungulate.day.under.exp <- MCMCglmm(Total ~
                                      daynight + Location2 +
                                      daynight.traffic+daynight.human,
                                    random = ~ average.effort ,
                                    prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallungulates.day.under)

#Summary stats
summary(smallungulate.day.under)
summary(smallungulate.day.under.exp)
plot(smallungulate.day.under)
plot(smallungulate.day.under.exp)

heidel.diag(smallungulate.day.under$VCV)
heidel.diag(smallungulate.day.under$Sol)
autocorr(smallungulate.day.under$Sol)
autocorr(smallungulate.day.under$VCV)
geweke.diag(smallungulate.day.under$Sol) # Should be between +-1.96

effectiveSize(smallungulate.day.under$Sol)
effectiveSize(smallungulate.day.under$VCV)
posterior.mode(smallungulate.day.under$Sol)
HPDinterval(smallungulate.day.under$Sol)
posterior.mode(smallungulate.day.under$VCV)
HPDinterval(smallungulate.day.under$VCV)

#jumpout
smallungulates.day.jump$Location2<-as.factor(smallcarnivores.day.jump$Location2)
smallungulates.day.jump$daynight<-as.factor(smallcarnivores.day.jump$daynight)

smallungulate.day.jump <- MCMCglmm(Total ~
                                      daynight + Location2 +
                                      daynight.traffic+daynight.human,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallungulates.day.jump)


smallungulate.day.jump.exp <- MCMCglmm(Total ~
                                          daynight + Location2 +
                                          daynight.traffic+daynight.human,
                                        random = ~ average.effort ,
                                        prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                        verbose = TRUE,
                                        family = "poisson",
                                        data = smallungulates.day.jump)



summary(smallungulate.day.jump)
summary(smallungulate.day.jump.exp)
plot(smallungulate.day.jump)
plot(smallungulate.day.jump.exp)

heidel.diag(smallungulate.day.jump$VCV)
heidel.diag(smallungulate.day.jump$Sol)
autocorr(smallungulate.day.jump$Sol)
autocorr(smallungulate.day.jump$VCV)
geweke.diag(smallungulate.day.jump$Sol) # Should be between +-1.96

effectiveSize(smallungulate.day.jump$Sol)
effectiveSize(smallungulate.day.jump$VCV)
posterior.mode(smallungulate.day.jump$Sol)
HPDinterval(smallungulate.day.jump$Sol)
posterior.mode(smallungulate.day.jump$VCV)
HPDinterval(smallungulate.day.jump$VCV)

# Small seasonal ungulates
#Underpass
smallungulates.season.under$Location2<-as.factor(smallcarnivores.season.under$Location2)
smallungulates.season.under$Season<-as.factor(smallcarnivores.season.under$Season)


smallungulate.season.under <- MCMCglmm(Total ~
                                      Season + Location2 +
                                      seasonal.traffic+seasonal.human,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallungulates.season.under)


smallungulate.season.under.exp <- MCMCglmm(Total ~
                                          Season + Location2 +
                                            seasonal.traffic+seasonal.human,
                                        random = ~ average.effort ,
                                        prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                        verbose = TRUE,
                                        family = "poisson",
                                        data = smallungulates.season.under)


summary(smallungulate.season.under)
summary(smallungulate.season.under.exp)
plot(smallungulate.season.under)
plot(smallungulate.season.under.exp)

heidel.diag(smallungulate.season.under$VCV)
heidel.diag(smallungulate.season.under$Sol)
autocorr(smallungulate.season.under$Sol)
autocorr(smallungulate.season.under$VCV)
geweke.diag(smallungulate.season.under$Sol) # Should be between +-1.96

effectiveSize(smallungulate.season.under$Sol)
effectiveSize(smallungulate.season.under$VCV)
posterior.mode(smallungulate.season.under$Sol)
HPDinterval(smallungulate.season.under$Sol)
posterior.mode(smallungulate.season.under$VCV)
HPDinterval(smallungulate.season.under$VCV)

#jumpouts
smallungulates.season.jump$Location2<-as.factor(smallcarnivores.season.jump$Location2)
smallungulates.season.jump$Season<-as.factor(smallcarnivores.season.jump$Season)


smallungulate.season.jump <- MCMCglmm(Total ~
                                         Season + Location2 +
                                         seasonal.traffic+seasonal.human,
                                       random = ~ average.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallungulates.season.jump)


smallungulate.season.jump.exp <- MCMCglmm(Total ~
                                             Season + Location2 +
                                             seasonal.traffic+seasonal.human,
                                           random = ~ average.effort ,
                                           prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                           verbose = TRUE,
                                           family = "poisson",
                                           data = smallungulates.season.jump)


summary(smallungulate.season.jump)
summary(smallungulate.season.jump.exp)
plot(smallungulate.season.jump)
plot(smallungulate.season.jump.exp)

heidel.diag(smallungulate.season.jump$VCV)
heidel.diag(smallungulate.season.jump$Sol)
autocorr(smallungulate.season.jump$Sol)
autocorr(smallungulate.season.jump$VCV)
geweke.diag(smallungulate.season.jump$Sol) # Should be between +-1.96

effectiveSize(smallungulate.season.jump$Sol)
effectiveSize(smallungulate.season.jump$VCV)
posterior.mode(smallungulate.season.jump$Sol)
HPDinterval(smallungulate.season.jump$Sol)
posterior.mode(smallungulate.season.jump$VCV)
HPDinterval(smallungulate.season.jump$VCV)

# Small annual ungulates
#Underpass
smallungulates.annual.under$Location2<-as.factor(smallcarnivores.annual.under$Location2)

smallungulate.annual.under <- MCMCglmm(Total ~
                                         Year + Location2 +
                                         annual.traffic+annual.human,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallungulates.annual.under)


smallungulate.annual.under.exp <- MCMCglmm(Total ~
                                              Year + Location2 +
                                             annual.traffic+annual.human,
                                           random = ~ annual.effort ,
                                           prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                           verbose = TRUE,
                                           family = "poisson",
                                           data = smallungulates.annual.under)


summary(smallungulate.annual.under)
summary(smallungulate.annual.under.exp)
plot(smallungulate.annual.under)
plot(smallungulate.annual.under.exp)

heidel.diag(smallungulate.annual.under$VCV)
heidel.diag(smallungulate.annual.under$Sol)
autocorr(smallungulate.annual.under$Sol)
autocorr(smallungulate.annual.under$VCV)
geweke.diag(smallungulate.annual.under$Sol) # Should be between +-1.96

effectiveSize(smallungulate.annual.under$Sol)
effectiveSize(smallungulate.annual.under$VCV)
posterior.mode(smallungulate.annual.under$Sol)
HPDinterval(smallungulate.annual.under$Sol)
posterior.mode(smallungulate.annual.under$VCV)
HPDinterval(smallungulate.annual.under$VCV)

#Jumpouts
smallungulates.annual.jump$Location2<-as.factor(smallcarnivores.annual.jump$Location2)

smallungulate.annual.jump <- MCMCglmm(Total ~
                                         Year + Location2 +
                                         annual.traffic+annual.human,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallungulates.annual.jump)


smallungulate.annual.jump.exp <- MCMCglmm(Total ~
                                             Year + Location2 +
                                             annual.traffic+annual.human,
                                           random = ~ annual.effort ,
                                           prior = prioexp, nitt=1003000, burnin=100000, thin=500,
                                           verbose = TRUE,
                                           family = "poisson",
                                           data = smallungulates.annual.jump)


summary(smallungulate.annual.jump)
summary(smallungulate.annual.jump.exp)
plot(smallungulate.annual.jump)
plot(smallungulate.annual.jump.exp)

heidel.diag(smallungulate.annual.jump$VCV)
heidel.diag(smallungulate.annual.jump$Sol)
autocorr(smallungulate.annual.jump$Sol)
autocorr(smallungulate.annual.jump$VCV)
geweke.diag(smallungulate.annual.jump$Sol) # Should be between +-1.96

effectiveSize(smallungulate.annual.jump$Sol)
effectiveSize(smallungulate.annual.jump$VCV)
posterior.mode(smallungulate.annual.jump$Sol)
HPDinterval(smallungulate.annual.jump$Sol)
posterior.mode(smallungulate.annual.jump$VCV)
HPDinterval(smallungulate.annual.jump$VCV)
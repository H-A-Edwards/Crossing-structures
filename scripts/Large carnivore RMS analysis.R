#This code runs the model for hypothesis one described in Edwards et al.  

#Script to run GLMM to look at the temporal variation in large carnivore use of underpasses and jumpouts.
#We first separate out the season/annual counts (Total) by structure type and explore the influence
#of season for the season counts and year and location for the annual counts. We account for sampling
#effort (annual/average.effort) in the random effect.

rm(list = ls())

#-----------------Set working directory--------------------
setwd("/Users/hannahedwards/Documents/Alberta Parks/Crossing-structures/data/")

#----------------Load libraries----------------------------
library(MCMCglmm)

#----------------Load datasets----------------------------

bigcarnivores.season <- read.csv("Bigcarnivores seasonal.csv")
bigcarnivores.annual <- read.csv("Bigcarnivores annual.csv")

#Separate out by crossing structure type
bigcarnivores.season.under<-subset(bigcarnivores.season, bigcarnivores.season$Underpass.type != "Jumpout")
bigcarnivores.season.jump<-subset(bigcarnivores.season, bigcarnivores.season$Underpass.type != "Underpass")
bigcarnivores.annual.under<-subset(bigcarnivores.annual, bigcarnivores.annual$Underpass.type != "Jumpout")
bigcarnivores.annual.jump<-subset(bigcarnivores.annual, bigcarnivores.annual$Underpass.type != "Underpass")

#Check formatting of variables
bigcarnivores.season.under$Location<-as.factor(bigcarnivores.season.under$Location)
bigcarnivores.season.under$Season<-as.factor(bigcarnivores.season.under$Season)
bigcarnivores.annual.under$Location<-as.factor(bigcarnivores.annual.under$Location)
bigcarnivores.annual.jump$Location<-as.factor(bigcarnivores.annual.jump$Location)

#Set the prior
prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))


#-------------------------------- Large carnivore seasonal model
#Underpass

contrasts(bigcarnivores.season.under$Season)
bigcarnivores.season.under$Season <- relevel(bigcarnivores.season.under$Season, ref=3)
contrasts(bigcarnivores.season.under$Season)

#GLMM
bigcarnivore.season.under <- MCMCglmm(Total ~
                                         Season,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = bigcarnivores.season.under)

summary(bigcarnivore.season.under)
plot(bigcarnivore.season.under)

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

#---------------------------------- Large carnivore annual model
#Underpass
#GLMM
bigcarnivore.annual.under <- MCMCglmm(Total ~
                                         Year + Location,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = bigcarnivores.annual.under)

summary(bigcarnivore.annual.under)
plot(bigcarnivore.annual.under)

heidel.diag(bigcarnivore.annual.under$VCV)
heidel.diag(bigcarnivore.annual.under.exp$VCV)
heidel.diag(bigcarnivore.annual.under$Sol)
heidel.diag(bigcarnivore.annual.under.exp$Sol)
autocorr(bigcarnivore.annual.under$Sol)
autocorr(bigcarnivore.annual.under$VCV)
geweke.diag(bigcarnivore.annual.under$Sol) # Should be between +-1.96

effectiveSize(bigcarnivore.annual.under$Sol)
effectiveSize(bigcarnivore.annual.under$VCV)
posterior.mode(bigcarnivore.annual.under$Sol)
HPDinterval(bigcarnivore.annual.under$Sol)
posterior.mode(bigcarnivore.annual.under$VCV)
HPDinterval(bigcarnivore.annual.under$VCV)

vif.MCMCglmm(bigcarnivore.annual.under)

#Jumpouts
#GLMM
bigcarnivore.annual.jump <- MCMCglmm(Total ~
                                         Year + Location,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = bigcarnivores.annual.jump)


summary(bigcarnivore.annual.jump)
plot(bigcarnivore.annual.jump)

heidel.diag(bigcarnivore.annual.jump$VCV)
heidel.diag(bigcarnivore.annual.jump.exp$VCV)
heidel.diag(bigcarnivore.annual.jump$Sol)
heidel.diag(bigcarnivore.annual.jump.exp$Sol)
autocorr(bigcarnivore.annual.jump$Sol)
autocorr(bigcarnivore.annual.jump$VCV)
geweke.diag(bigcarnivore.annual.jump$Sol) # Should be between +-1.96

effectiveSize(bigcarnivore.annual.jump$Sol)
effectiveSize(bigcarnivore.annual.jump$VCV)
posterior.mode(bigcarnivore.annual.jump$Sol)
HPDinterval(bigcarnivore.annual.jump$Sol)
posterior.mode(bigcarnivore.annual.jump$VCV)
HPDinterval(bigcarnivore.annual.jump$VCV)

vif.MCMCglmm(bigcarnivore.annual.jump)

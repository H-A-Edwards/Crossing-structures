#This code runs the model for hypothesis one described in Edwards et al.  

#Script to run GLMM to look at the temporal variation in small carnivore use of underpasses and jumpouts.
#We first separate out the season/annual counts (Total) by structure type and explore the influence
#of season for the season counts and year and location for the annual counts. We account for sampling
#effort (annual/average.effort) in the random effect.

rm(list = ls())

#-----------------Set working directory--------------------
setwd("/Users/hannahedwards/Documents/Alberta Parks/Crossing-structures/data/")

#----------------Load libraries----------------------------
library(MCMCglmm)

#----------------Load datasets----------------------------

smallcarnivores.season <- read.csv("Smallcarnivores seasonal.csv")
smallcarnivores.annual <- read.csv("Smallcarnivores annual.csv")

#Separate out by crossing structure type
smallcarnivores.season.under<-subset(smallcarnivores.season, smallcarnivores.season$Underpass.type != "Jumpout")
smallcarnivores.season.jump<-subset(smallcarnivores.season, smallcarnivores.season$Underpass.type != "Underpass")
smallcarnivores.annual.under<-subset(smallcarnivores.annual, smallcarnivores.annual$Underpass.type != "Jumpout")
smallcarnivores.annual.jump<-subset(smallcarnivores.annual, smallcarnivores.annual$Underpass.type != "Underpass")

#Setup prior

prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))

#-------------------------------- Small carnivore seasonal model
#Underpass
smallcarnivores.season.under$Location<-as.factor(smallcarnivores.season.under$Location)
smallcarnivores.season.under$Season<-as.factor(smallcarnivores.season.under$Season)

contrasts(smallcarnivores.season.under$Season)
smallcarnivores.season.under$Season <- relevel(smallcarnivores.season.under$Season, ref=3)
contrasts(smallcarnivores.season.under$Season)

#GLMM
smallcarnivore.season.under <- MCMCglmm(Total ~
                                         Season,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallcarnivores.season.under)

summary(smallcarnivore.season.under)
plot(smallcarnivore.season.under)

heidel.diag(smallcarnivore.season.under$VCV)
heidel.diag(smallcarnivore.season.under$Sol)
autocorr(smallcarnivore.season.under$Sol)
autocorr(smallcarnivore.season.under$VCV)
geweke.diag(smallcarnivore.season.under$Sol) # Should be between +-1.96

effectiveSize(smallcarnivore.season.under$Sol)
effectiveSize(smallcarnivore.season.under$VCV)
posterior.mode(smallcarnivore.season.under$Sol)
HPDinterval(smallcarnivore.season.under$Sol)
posterior.mode(smallcarnivore.season.under$VCV)
HPDinterval(smallcarnivore.season.under$VCV)

vif.MCMCglmm(smallcarnivore.season.under)

#Jumpouts
smallcarnivores.season.jump$Location<-as.factor(smallcarnivores.season.jump$Location)
smallcarnivores.season.jump$Season<-as.factor(smallcarnivores.season.jump$Season)

contrasts(smallcarnivores.season.jump$Season)
smallcarnivores.season.jump$Season <- relevel(smallcarnivores.season.jump$Season, ref=3)
contrasts(smallcarnivores.season.jump$Season)

#GLMM
smallcarnivore.season.jump <- MCMCglmm(Total ~
                                         Season + Location,
                                       random = ~ average.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallcarnivores.season.jump)


summary(smallcarnivore.season.jump)
plot(smallcarnivore.season.jump)

heidel.diag(smallcarnivore.season.jump$VCV)
heidel.diag(smallcarnivore.season.jump$Sol)
autocorr(smallcarnivore.season.jump$Sol)
autocorr(smallcarnivore.season.jump$VCV)
geweke.diag(smallcarnivore.season.jump$Sol) # Should be between +-1.96

effectiveSize(smallcarnivore.season.jump$Sol)
effectiveSize(smallcarnivore.season.jump$VCV)
posterior.mode(smallcarnivore.season.jump$Sol)
HPDinterval(smallcarnivore.season.jump$Sol)
posterior.mode(smallcarnivore.season.jump$VCV)
HPDinterval(smallcarnivore.season.jump$VCV)

vif.MCMCglmm(smallcarnivore.season.jump)

#-------------------------------- Small carnivore annual model
#Underpass
smallcarnivores.annual.under$Location<-as.factor(smallcarnivores.annual.under$Location)

#IG prior preferred
smallcarnivore.annual.under <- MCMCglmm(Total ~
                                         Year + Location,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallcarnivores.annual.under)

summary(smallcarnivore.annual.under)
plot(smallcarnivore.annual.under)

heidel.diag(smallcarnivore.annual.under$VCV)
heidel.diag(smallcarnivore.annual.under$Sol)
autocorr(smallcarnivore.annual.under$Sol)
autocorr(smallcarnivore.annual.under$VCV)
geweke.diag(smallcarnivore.annual.under$Sol) # Should be between +-1.96

effectiveSize(smallcarnivore.annual.under$Sol)
effectiveSize(smallcarnivore.annual.under$VCV)
posterior.mode(smallcarnivore.annual.under$Sol)
HPDinterval(smallcarnivore.annual.under$Sol)
posterior.mode(smallcarnivore.annual.under$VCV)
HPDinterval(smallcarnivore.annual.under$VCV)

vif.MCMCglmm(smallcarnivore.annual.under)

#Jumpouts
smallcarnivores.annual.jump$Location<-as.factor(smallcarnivores.annual.jump$Location)

#GLMM
smallcarnivore.annual.jump <- MCMCglmm(Total ~
                                         Year + Location,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallcarnivores.annual.jump)

summary(smallcarnivore.annual.jump)
plot(smallcarnivore.annual.jump)

heidel.diag(smallcarnivore.annual.jump$VCV)
heidel.diag(smallcarnivore.annual.jump$Sol)
autocorr(smallcarnivore.annual.jump$Sol)
autocorr(smallcarnivore.annual.jump$VCV)
geweke.diag(smallcarnivore.annual.jump$Sol) # Should be between +-1.96

effectiveSize(smallcarnivore.annual.jump$Sol)
effectiveSize(smallcarnivore.annual.jump$VCV)
posterior.mode(smallcarnivore.annual.jump$Sol)
HPDinterval(smallcarnivore.annual.jump$Sol)
posterior.mode(smallcarnivore.annual.jump$VCV)
HPDinterval(smallcarnivore.annual.jump$VCV)

vif.MCMCglmm(smallcarnivore.annual.jump)

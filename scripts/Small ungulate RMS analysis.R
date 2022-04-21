#This code runs the model for hypothesis one described in Edwards et al.  

#Script to run GLMM to look at the temporal variation in small ungulate use of underpasses and jumpouts.
#We first separate out the season/annual counts (Total) by structure type and explore the influence
#of season for the season counts and year and location for the annual counts. We account for sampling
#effort (annual/average.effort) in the random effect.

rm(list = ls())

#-----------------Set working directory--------------------
setwd("/Users/hannahedwards/Documents/Alberta Parks/Crossing-structures/data/")

#----------------Load libraries----------------------------
library(MCMCglmm)

#----------------Load datasets----------------------------
smallungulates.season <- read.csv("Smallungulates seasonal.csv")
smallungulates.annual <- read.csv("Smallungulates annual.csv")

#Separate out by crossing structure type
smallungulates.season.under<-subset(smallungulates.season, smallungulates.season$Underpass.type != "Jumpout")
smallungulates.season.jump<-subset(smallungulates.season, smallungulates.season$Underpass.type != "Underpass")
smallungulates.annual.under<-subset(smallungulates.annual, smallungulates.annual$Underpass.type != "Jumpout")
smallungulates.annual.jump<-subset(smallungulates.annual, smallungulates.annual$Underpass.type != "Underpass")

#Setup prior
prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))

#-------------------------------- Small ungulate seasonal model
#Underpass
smallungulates.season.under$Location<-as.factor(smallungulates.season.under$Location)
smallungulates.season.under$Season<-as.factor(smallungulates.season.under$Season)

contrasts(smallungulates.season.under$Season)
smallungulates.season.under$Season <- relevel(smallungulates.season.under$Season, ref=3)
contrasts(smallungulates.season.under$Season)

#GLMM
smallungulate.season.under <- MCMCglmm(Total ~
                                         Season,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = smallungulates.season.under)


summary(smallungulate.season.under)
plot(smallungulate.season.under)

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

vif.MCMCglmm(smallungulate.season.under)

#Jumpouts
smallungulates.season.jump$Location<-as.factor(smallungulates.season.jump$Location)
smallungulates.season.jump$Season<-as.factor(smallungulates.season.jump$Season)

contrasts(smallungulates.season.jump$Season)
smallungulates.season.jump$Season <- relevel(smallungulates.season.jump$Season, ref=3)
contrasts(smallungulates.season.jump$Season)

#GLMM
smallungulate.season.jump <- MCMCglmm(Total ~
                                         Season + Location,
                                       random = ~ average.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallungulates.season.jump)

summary(smallungulate.season.jump)
plot(smallungulate.season.jump)

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

vif.MCMCglmm(smallungulate.season.jump)

#-------------------------------- Small ungulate annual model
#Underpass
smallungulates.annual.under$Location<-as.factor(smallungulates.annual.under$Location)

#GLMM
smallungulate.annual.under <- MCMCglmm(Total ~
                                         Year + Location,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallungulates.annual.under)

summary(smallungulate.annual.under)
plot(smallungulate.annual.under)

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

vif.MCMCglmm(smallungulate.annual.under)

#Jumpouts
smallungulates.annual.jump$Location<-as.factor(smallungulates.annual.jump$Location)
smallungulates.annual.jump$annual.human<-as.factor(smallungulates.annual.jump$annual.human)

#IG prior preferred
smallungulate.annual.jump <- MCMCglmm(Total ~
                                         Year + Location,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = smallungulates.annual.jump)

summary(smallungulate.annual.jump)
plot(smallungulate.annual.jump)

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

vif.MCMCglmm(smallungulate.annual.jump)

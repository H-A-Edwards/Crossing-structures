#This code runs the model for hypothesis one described in Edwards et al.  

#Script to run GLMM to look at the temporal variation in large ungulate use of underpasses and jumpouts.
#We first separate out the season/annual counts (Total) by structure type and explore the influence
#of season for the season counts and year and location for the annual counts. We account for sampling
#effort (annual/average.effort) in the random effect.

rm(list = ls())

#-----------------Set working directory--------------------
setwd("/Users/hannahedwards/Documents/Alberta Parks/Crossing-structures/data/")

#----------------Load libraries----------------------------
library(MCMCglmm)
library(HDInterval)
library(forestplot)

#----------------Load datasets----------------------------
bigungulates.season <- read.csv("Bigungulates seasonal.csv")
bigungulates.annual <- read.csv("Bigungulates annual.csv")

#Separate out by crossing structure type
bigungulates.season.under<-subset(bigungulates.season, bigungulates.season$Underpass.type != "Jumpout")
bigungulates.season.jump<-subset(bigungulates.season, bigungulates.season$Underpass.type != "Underpass")
bigungulates.annual.under<-subset(bigungulates.annual, bigungulates.annual$Underpass.type != "Jumpout")
bigungulates.annual.jump<-subset(bigungulates.annual, bigungulates.annual$Underpass.type != "Underpass")

#Setup prior

prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))


#-------------------------------- Large ungulate seasonal model
#Underpass
bigungulates.season.under$Location<-as.factor(bigungulates.season.under$Location)
bigungulates.season.under$Season<-as.factor(bigungulates.season.under$Season)

contrasts(bigungulates.season.under$Season)
bigungulates.season.under$Season <- relevel(bigungulates.season.under$Season, ref=4)
contrasts(bigungulates.season.under$Season)

#GLMM
bigungulate.season.under <- MCMCglmm(Total ~
                                         Season,
                                    random = ~ average.effort ,
                                    prior = prior, nitt=1003000, burnin=100000, thin=500,
                                    verbose = TRUE,
                                    family = "poisson",
                                    data = bigungulates.season.under)

summary(bigungulate.season.under)
plot(bigungulate.season.under)

heidel.diag(bigungulate.season.under$VCV)
heidel.diag(bigungulate.season.under$Sol)
autocorr(bigungulate.season.under$Sol)
autocorr(bigungulate.season.under$VCV)
geweke.diag(bigungulate.season.under$Sol) # Should be between +-1.96

effectiveSize(bigungulate.season.under$Sol)
effectiveSize(bigungulate.season.under$VCV)
posterior.mode(bigungulate.season.under$Sol)
HPDinterval(bigungulate.season.under$Sol)
posterior.mode(bigungulate.season.under$VCV)
HPDinterval(bigungulate.season.under$VCV)

vif.MCMCglmm(bigungulate.season.under)

#jumpouts
bigungulates.season.jump$Location<-as.factor(bigungulates.season.jump$Location)
bigungulates.season.jump$Season<-as.factor(bigungulates.season.jump$Season)

contrasts(bigungulates.season.jump$Season)
bigungulates.season.jump$Season <- relevel(bigungulates.season.jump$Season, ref=1)
contrasts(bigungulates.season.jump$Season)

#GLMM
bigungulate.season.jump <- MCMCglmm(Total ~
                                         Season + Location,
                                       random = ~ average.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = bigungulates.season.jump)

summary(bigungulate.season.jump)
plot(bigungulate.season.jump)

heidel.diag(bigungulate.season.jump$VCV)
heidel.diag(bigungulate.season.jump$Sol)
autocorr(bigungulate.season.jump$Sol)
autocorr(bigungulate.season.jump$VCV)
geweke.diag(bigungulate.season.jump$Sol) # Should be between +-1.96

effectiveSize(bigungulate.season.jump$Sol)
effectiveSize(bigungulate.season.jump$VCV)
posterior.mode(bigungulate.season.jump$Sol)
HPDinterval(bigungulate.season.jump$Sol)
posterior.mode(bigungulate.season.jump$VCV)
HPDinterval(bigungulate.season.jump$VCV)

vif.MCMCglmm(bigungulate.season.jump)

#--------------------------------- Large ungulate annual model
#Underpass
bigungulates.annual.under$Location<-as.factor(bigungulates.annual.under$Location)

#GLMM
bigungulate.annual.under <- MCMCglmm(Total ~
                                         Year + Location,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = bigungulates.annual.under)

summary(bigungulate.annual.under)
plot(bigungulate.annual.under)

heidel.diag(bigungulate.annual.under$VCV)
heidel.diag(bigungulate.annual.under$Sol)
autocorr(bigungulate.annual.under$Sol)
autocorr(bigungulate.annual.under$VCV)
geweke.diag(bigungulate.annual.under$Sol) # Should be between +-1.96

effectiveSize(bigungulate.annual.under$Sol)
effectiveSize(bigungulate.annual.under$VCV)
posterior.mode(bigungulate.annual.under$Sol)
HPDinterval(bigungulate.annual.under$Sol)
posterior.mode(bigungulate.annual.under$VCV)
HPDinterval(bigungulate.annual.under$VCV)

vif.MCMCglmm(bigungulate.annual.under)

#Jumpouts
bigungulates.annual.jump$Location<-as.factor(bigungulates.annual.jump$Location)

#GLMM
bigungulate.annual.jump <- MCMCglmm(Total ~
                                         Year + Location,
                                       random = ~ annual.effort ,
                                       prior = prior, nitt=1003000, burnin=100000, thin=500,
                                       verbose = TRUE,
                                       family = "poisson",
                                       data = bigungulates.annual.jump)

summary(bigungulate.annual.jump)
plot(bigungulate.annual.jump)

heidel.diag(bigungulate.annual.jump$VCV)
heidel.diag(bigungulate.annual.jump$Sol)
autocorr(bigungulate.annual.jump$Sol)
autocorr(bigungulate.annual.jump$VCV)
geweke.diag(bigungulate.annual.jump$Sol) # Should be between +-1.96

effectiveSize(bigungulate.annual.jump$Sol)
effectiveSize(bigungulate.annual.jump$VCV)
posterior.mode(bigungulate.annual.jump$Sol)
HPDinterval(bigungulate.annual.jump$Sol)
posterior.mode(bigungulate.annual.jump$VCV)
HPDinterval(bigungulate.annual.jump$VCV)

vif.MCMCglmm(bigungulate.annual.jump)

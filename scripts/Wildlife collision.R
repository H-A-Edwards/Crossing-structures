rm(list=ls())

#data
getwd()
setwd("Documents/Alberta parks/Data")

####data set
Collision<-read.csv("Collision_data_1991_2014.csv",header=T)

####Calculate Collision count each year
library(dplyr)
Collision2<-aggregate(Collision.count.x~Year, data=Collision, FUN='sum')
Collision_final<-left_join(Collision,Collision2, by="Year")
library(WriteXLS)
WriteXLS(Collision_final, ExcelFileName="Collision_Final.xlsx")
####Or replace the original file


###Check data structure
summary(Collision$Mitigationstatus)
Collision$Mitigationstatus<-as.factor(Collision$Mitigation.status)
Collision$Species<-as.factor(Collision$Species)
Collision$Year<-as.factor(Collision$Case.year)
Collision$Guild<-as.factor(Collision$Guild)
Collision$Collisioncount<-as.integer(Collision$Collision.count.per.year)
Collision$TimeSC<-as.numeric(Collision$Time.since.Stewart.Creek.installation.cent)
Collision$TimeWVDM<-as.numeric(Collision$Time.since.Wind.Valley.Deadmans.installation.cent)
Collision$Collisionrate<-as.factor(Collision$Collision.rate.divided.by.mitigation.years)
Collision$CollisionID<-as.factor(Collision$Collision.ID)
str(Collision$Collisionrate)
#histogram of collision count per year
hist(Collision$Collisioncount, ylim=c(0, 200))
shapiro.test(Collision$Collisioncount)

library(MCMCglmm)

prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))

prior2<-list(R=list(V=1, nu=0.02), 
            G = list(G1 = list(V = 1, nu = 0.02)))


prioexp<- list(R = list(V = 1, nu=0.02), 
               G = list(G1 = list(V = 1,nu= 0.02,alpha.mu=0,alpha.V=1000)))


model<-MCMCglmm(Collisioncount~Mitigationstatus+Guild,
                random=~Year,
                prior=prioexp, nitt=503000, burnin=40000, thin=250,
                verbose=TRUE, 
                family="poisson", 
                data=Collision)

summary(Collision$TimeSC)
summary(Collision$Year)

save(model, file ="Collision_Expprior.Rdata")
load("Collision_IGprior.Rdata")
load("Collision_Expprior.Rdata")

summary(model)
plot(model)
heidel.diag(model$VCV)
heidel.diag(model$Sol)
autocorr(model$Sol)
autocorr(model$VCV)
geweke.diag(model$Sol)###Should be between +-1.96
effectiveSize(model$Sol)
effectiveSize(model$VCV)
posterior.mode(model$Sol)
HPDinterval(model$Sol)
posterior.mode(model$VCV)
HPDinterval(model$VCV)

####Collision predictors
model<-MCMCglmm(Guild~Timeofday+kmlocation+trafficdensity,
                random=~Year,
                prior=prioexp, nitt=503000, burnin=40000, thin=250,
                verbose=TRUE, 
                family="poisson", 
                data=Collision)


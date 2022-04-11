#This code runs the model for hypothesis one described in Edwards et al.   

# Scripts to run the models in MCMCglmm
# These models address the hypothesis that RMS installation 
# affect the number of annual wildlife-vehicle collision

# After the model is complete we check convergency of the chains, 
# and do basic model diagnostics

#----------------------Load library--------------------

library(MCMCglmm)

#----------------------Load data---------------------

####data set
Collision<-read.csv("Collision_data_1991_2014.csv",header=T)

###Check data structure
Collision$Mitigationstatus<-as.factor(Collision$Mitigation.status)
Collision$Species<-as.factor(Collision$Species)
Collision$Year<-as.factor(Collision$Case.year)
Collision$Guild<-as.factor(Collision$Guild)
Collision$Collisioncount<-as.integer(Collision$Collision.count.per.year)
Collision$Threshold<-as.factor(Collision$Damage.threshold)

#Set the priors
prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))

#Change levels to look at differences in status 
contrasts(Collision$Mitigationstatus)
Collision$Mitigationstatus <- relevel(Collision$Mitigationstatus, ref=2)
contrasts(Collision$Mitigationstatus)

#----------------------Run model---------------------
model<-MCMCglmm(Collisioncount~Mitigationstatus+Guild+Threshold,
                random=~Year,
                prior=prior1, nitt=503000, burnin=40000, thin=250,
                verbose=TRUE, 
                family="poisson", 
                data=Collision)

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

save(model, file ="Collision_IGprior.Rdata")
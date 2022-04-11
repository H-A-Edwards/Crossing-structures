setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/data/")
newdata = read.csv("newdata.csv")

newdata$Year = as.factor(newdata$Year)
newdata$Location = as.factor(newdata$Location)

newdata$Hour.sq = NA
newdata$Month.sq = NA
newdata$Hour.sq = (newdata$Hour)^2
newdata$Month.sq = (newdata$Month)^2


#change variable structures
carnivores_traffic$Location = as.factor(carnivores_traffic$Location)
carnivores_traffic$Species = as.factor(carnivores_traffic$Species)
carnivores_traffic$Total = as.integer(carnivores_traffic$Total)
carnivores_traffic$HourEnding = as.factor(carnivores_traffic$HourEnding)
carnivores_traffic$Year = as.factor(carnivores_traffic$Year)
carnivores_traffic$Month = as.factor(carnivores_traffic$Month)
carnivores_traffic$Day = as.factor(carnivores_traffic$Day)
carnivores_traffic$Hour = as.factor(carnivores_traffic$Hour)
carnivores_traffic$Two.way = as.numeric(carnivores_traffic$Two.way)
carnivores_traffic$Species.grouped = as.factor(carnivores_traffic$Species.grouped)
carnivores_traffic$Location2 = as.factor(carnivores_traffic$Location2)
carnivores_traffic$Underpass.type = as.factor(carnivores_traffic$Underpass.type)
carnivores_traffic$Date.structure.built = as.factor(carnivores_traffic$Date.structure.built)
carnivores_traffic$Structure.age = as.numeric(carnivores_traffic$Structure.age)
carnivores_traffic$Human.total.x = as.numeric(carnivores_traffic$Human.total.x)
carnivores_traffic$Agecentred = as.numeric(carnivores_traffic$Agecentred)
carnivores_traffic$Sampling.effort = as.factor(carnivores_traffic$Sampling.effort)
carnivores_traffic$Hour.sq = as.factor(carnivores_traffic$Hour.sq)
carnivores_traffic$Month.sq = as.factor(carnivores_traffic$Month.sq)
carnivores_traffic$Sampling.effort = as.factor(carnivores_traffic$Sampling.effort)




#################################################################################
#################################################################################
#################################################################################
############################ HR/MO/YR ANALYSIS ##################################
#################################################################################
#################################################################################

ungulates.hourly = read.csv("ungulates hourly.csv")
ungulates.monthly = read.csv("ungulates monthly.csv")
ungulates.annual = read.csv("ungulates annual.csv")


carnivores.hourly = read.csv("carnivores hourly.csv")
carnivores.monthly = read.csv("carnivores monthly.csv")
carnivores.annual = read.csv("carnivores annual.csv")


# Find number of crossings per location
# hourly


# Find number of crossings per location
# hourly
sum(carnivores.hourly$Total[carnivores.hourly$Location2 == "Dead Man's"]) +
  sum(ungulates.hourly$Total[ungulates.hourly$Location2 == "Dead Man's"])

sum(carnivores.hourly$Total[carnivores.hourly$Location2 == "Stewart Creek"]) +
  sum(ungulates.hourly$Total[ungulates.hourly$Location2 == "Stewart Creek"])

sum(carnivores.hourly$Total[carnivores.hourly$Location2 == "Wind Valley"]) +
  sum(ungulates.hourly$Total[ungulates.hourly$Location2 == "Wind Valley"])

sum(carnivores.hourly$Total[carnivores.hourly$Underpass.type == "Jumpout"]) +
  sum(ungulates.hourly$Total[ungulates.hourly$Underpass.type == "Jumpout"])

sum(carnivores.hourly$Total[carnivores.hourly$Underpass.type == "Underpass"]) +
  sum(ungulates.hourly$Total[ungulates.hourly$Underpass.type == "Underpass"])



## Using ungulates.hourly, carnivores.hourly, ungulates.monthly, carnivores.monthly, ungulates.annual, carnivores.anual
# usually aim to store 1,000-2,000 iterations
# have the autocorrelation between successive stored iterations less than 0.1
# fixed effects: interesting in themselves
# random effects: only underlying population is of interest


require(MCMCglmm)

prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))

prioexp<- list(R = list(V = 1, nu=0.002), #residuals prior
               G = list(G1 = list(V = 1,nu= 0.02,alpha.mu=0,alpha.V=1000)))

clean.MCMC <- function(x) {
  sols <- summary(x)$solutions  ## pull out relevant info from model summary
  Gcovs <- summary(x)$Gcovariances
  Rcovs <- summary(x)$Rcovariances
  fixed <- data.frame(row.names(sols), sols, row.names = NULL)  ## convert to dataframes with the row.names as the first col
  random <- data.frame(row.names(Gcovs), Gcovs, row.names = NULL)
  residual <- data.frame(row.names(Rcovs), Rcovs, row.names = NULL)
  names(fixed)[names(fixed) == "row.names.sols."] <- "variable"  ## change the columns names to variable, so they all match
  names(random)[names(random) == "row.names.Gcovs."] <- "variable"
  names(residual)[names(residual) == "row.names.Rcovs."] <- "variable"
  fixed$effect <- "fixed"  ## add ID column for type of effect (fixed, random, residual)
  random$effect <- "random"
  residual$effect <- "residual"
  modelTerms <- as.data.frame(bind_rows(fixed, random, residual))  # merge it all together
}


###### hourly and monthly models with year and sampling effort ######



##################  HOURLY ungulates with year + sampling effort #################

prior <- list(R=list(V=1, nu=0.002), 
              G = list(G1 = list(V = 1, nu = 0.002),
                       G2 = list(V = 1, nu = 0.002)))

prioexp <- list(R = list(V = 1, nu=0.002), #residuals prior
                G = list(G1 = list(V = 1,nu= 0.02,alpha.mu=0,alpha.V=1000), #random effects priors
                         G2 = list(V = 1,nu= 0.02,alpha.mu=0,alpha.V=1000)))

ungulate.hrModel2 = MCMCglmm(Total ~ HourEnding + Hour.sq +
                               Two.way + Location2 + 
                               Underpass.type + Human.total.x +
                               Agecentred,
                             random = ~ Year + Sampling.effort,
                             prior = prior, nitt=203000, burnin=20000, thin=150,
                             verbose = TRUE,
                             family = "poisson",
                             data = ungulates.hourly2)

ungulate.hrModelExp2 = MCMCglmm(Total ~ HourEnding + Hour.sq +
                                  Two.way + Location2 + 
                                  Underpass.type + Human.total.x +
                                  Agecentred,
                                random = ~ Year + Sampling.effort,
                                prior = prioexp, nitt=203000, burnin=20000, thin=150,
                                verbose = TRUE,
                                family = "poisson",
                                data = ungulates.hourly2)

setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/models/ungulates/hourly/")
save(ungulate.hrModel2, file ="ungulate hourly model2.Rdata")
save(ungulate.hrModelExp2, file ="ungulate hourly model - exp2.Rdata")
ung.hrModel2 <- clean.MCMC(ungulate.hrModel2)  # get all the info from summary(modelName)
ung.hrModelExp2 <- clean.MCMC(ungulate.hrModel2)  # get all the info from summary(modelName)
write.csv(ung.hrModel2, "ungulate hourly model 2 - summary.csv")
write.csv(ung.hrModelExp2, "ungulate hourly modelEXP 2 - summary.csv")



summary(ungulate.hrModel2)
summary(ungulate.hrModelExp2)
par(mar=c(1,1,1,1))
plot(ungulate.hrModelExp2)
heidel.diag(ungulate.hrModelExp2$VCV)
heidel.diag(ungulate.hrModelExp2$Sol)
autocorr(ungulate.hrModelExp2$Sol)
autocorr(ungulate.hrModelExp2$VCV)
geweke.diag(ungulate.hrModelExp2$Sol) # Should be between +-1.96
# IG prior model: Hour.sq is outside range
# Exp prior model: Location2Stewart Creek = 2.3
effectiveSize(ungulate.hrModelExp2$Sol)
effectiveSize(ungulate.hrModelExp2$VCV)
posterior.mode(ungulate.hrModelExp2$Sol)
HPDinterval(ungulate.hrModelExp2$Sol)
posterior.mode(ungulate.hrModelExp2$VCV)
HPDinterval(ungulate.hrModelExp2$VCV)




##################  HOURLY carnivores #################

carnivore.hrModel2 = MCMCglmm(Total ~ HourEnding + Hour.sq +
                                Two.way + Location2 + 
                                Underpass.type + Human.total.x +
                                Agecentred,
                              random = ~ Year + Sampling.effort,
                              prior = prior, nitt=203000, burnin=20000, thin=150,
                              verbose = TRUE,
                              family = "poisson",
                              data = carnivores.hourly2)


carnivore.hrModelExp2 = MCMCglmm(Total ~ HourEnding + Hour.sq +
                                   Two.way + Location2 + 
                                   Underpass.type + Human.total.x +
                                   Agecentred,
                                 random = ~ Year + Sampling.effort,
                                 prior = prioexp, nitt=203000, burnin=20000, thin=150,
                                 verbose = TRUE,
                                 family = "poisson",
                                 data = carnivores.hourly2)

setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/models/carnivores/hourly/")
save(carnivore.hrModel2, file ="carnivore hourly model2.Rdata")
save(carnivore.hrModelExp2, file ="carnivore hourly model - exp2.Rdata")
car.hrModel2 <- clean.MCMC(carnivore.hrModel2)  # get all the info from summary(modelName)
car.hrModelExp2 <- clean.MCMC(carnivore.hrModelExp2)  # get all the info from summary(modelName)
write.csv(car.hrModel2, "carnivore hourly model - summary2.csv")
write.csv(car.hrModelExp2, "carnivore hourly modelEXP - summary2.csv")


summary(carnivore.hrModel2)
par(mar=c(1,1,1,1))
plot(carnivore.hrModel2)
heidel.diag(carnivore.hrModel2$VCV)
heidel.diag(carnivore.hrModelExp2$Sol)
autocorr(carnivore.hrModel2$Sol)
autocorr(carnivore.hrModel2$VCV)
geweke.diag(carnivore.hrModelExp2$Sol) # Should be between +-1.96
# IG prior model OK
# exp prior model OK
effectiveSize(carnivore.hrModel2$Sol)
effectiveSize(carnivore.hrModel2$VCV)
posterior.mode(carnivore.hrModel2$Sol)
HPDinterval(carnivore.hrModel2$Sol)
posterior.mode(carnivore.hrModel2$VCV)
HPDinterval(carnivore.hrModel2$VCV)



##################  MONTHLY ungulates ################# 

ungulate.moModel2 = MCMCglmm(Total ~ Month + Month.sq +
                               Two.way + Location2 + 
                               Underpass.type + Human.total.x +
                               Agecentred,
                             random = ~ Year + Sampling.effort,
                             prior = prior, nitt=203000, burnin=20000, thin=150,
                             verbose = TRUE,
                             family = "poisson",
                             data = ungulates.monthly2)


ungulate.moModelExp2 = MCMCglmm(Total ~ Month + Month.sq +
                                  Two.way + Location2 + 
                                  Underpass.type + Human.total.x +
                                  Agecentred,
                                random = ~ Year + Sampling.effort,
                                prior = prioexp, nitt=203000, burnin=20000, thin=150,
                                verbose = TRUE,
                                family = "poisson",
                                data = ungulates.monthly2)


setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/models/ungulates/monthly/")
save(ungulate.moModel2, file ="ungulate monthly model2.Rdata")
save(ungulate.moModelExp2, file ="ungulate monthly model - exp2.Rdata")
ung.moModel2 <- clean.MCMC(ungulate.moModel2)  # get all the info from summary(modelName)
ung.moModelExp2 <- clean.MCMC(ungulate.moModelExp2)  # get all the info from summary(modelName)
write.csv(ung.moModel2, "ungulate monthly model 2 - summary table.csv")
write.csv(ung.moModelExp2, "ungulate monthly modelEXP 2 - summary table.csv")


summary(ungulate.moModel2)
par(mar=c(1,1,1,1))
plot(ungulate.moModel2)
heidel.diag(ungulate.moModelExp2$VCV)
heidel.diag(ungulate.moModelExp2$Sol)
autocorr(ungulate.moModel2$Sol)
autocorr(ungulate.moModel2$VCV)
geweke.diag(ungulate.moModel2$Sol) # Should be between +-1.96
effectiveSize(ungulate.moModel2$Sol)
effectiveSize(ungulate.moModel2$VCV)
posterior.mode(ungulate.moModel2$Sol)
HPDinterval(ungulate.moModel2$Sol)
posterior.mode(ungulate.moModel2$VCV)
HPDinterval(ungulate.moModel2$VCV)



##################  MONTHLY carnivores ################# 

carnivore.moModel2 = MCMCglmm(Total ~ Month + Month.sq +
                                Two.way + Location2 + 
                                Underpass.type + Human.total.x +
                                Agecentred,
                              random = ~ Year + Sampling.effort,
                              prior = prior, nitt=203000, burnin=20000, thin=150,
                              verbose = TRUE,
                              family = "poisson",
                              data = carnivores.monthly2)

carnivore.moModelExp2 = MCMCglmm(Total ~ Month + Month.sq +
                                   Two.way + Location2 + 
                                   Underpass.type + Human.total.x +
                                   Agecentred,
                                 random = ~ Year + Sampling.effort,
                                 prior = prioexp, nitt=203000, burnin=20000, thin=150,
                                 verbose = TRUE,
                                 family = "poisson",
                                 data = carnivores.monthly2)

setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/models/carnivores/monthly/")
save(carnivore.moModel2, file ="carnivore monthly model2.Rdata")
save(carnivore.moModelExp2, file ="carnivore monthly model - exp2.Rdata")
car.moModel2 <- clean.MCMC(carnivore.moModel2)  # get all the info from summary(modelName)
car.moModelExp2 <- clean.MCMC(carnivore.moModelExp2)  # get all the info from summary(modelName)
write.csv(car.moModel2, "carnivore monthly model 2 - summary table.csv")
write.csv(car.moModelExp2, "carnivore monthly modelEXP 2 - summary table.csv")


summary(carnivore.moModel2)
par(mar=c(1,1,1,1))
plot(carnivore.moModel2)
heidel.diag(carnivore.moModelExp2$VCV)
heidel.diag(carnivore.moModelExp2$Sol)
autocorr(carnivore.moModel2$Sol)
autocorr(carnivore.moModel2$VCV)
geweke.diag(carnivore.moModelExp2$Sol) # Should be between +-1.96
## IG: all values within range
## Exp: all values within range
effectiveSize(carnivore.moModel2$Sol)
effectiveSize(carnivore.moModel2$VCV)
posterior.mode(carnivore.moModel2$Sol)
HPDinterval(carnivore.moModel2$Sol)
posterior.mode(carnivore.moModel2$VCV)
HPDinterval(carnivore.moModel2$VCV)






##################  ANNUAL ungulates ################# 
### Change random effect priors here

prior<-list(R=list(V=1, nu=0.002), 
            G = list(G1 = list(V = 1, nu = 0.002)))

prioexp<- list(R = list(V = 1, nu=0.002), #residuals prior
               G = list(G1 = list(V = 1,nu= 0.02,alpha.mu=0,alpha.V=1000),
                        G2 = list(V = 1,nu= 0.02,alpha.mu=0,alpha.V=1000))) #random effects priors


ungulate.yrModel = MCMCglmm(Total ~ Year +
                              Two.way + Location2 + 
                              Underpass.type + Human.total.x,
                            random = ~ Agecentred + Sampling.effort,
                            prior = prior, nitt=203000, burnin=20000, thin=150,
                            verbose = TRUE,
                            family = "poisson",
                            data = ungulates.annual)

ungulate.yrModelExp = MCMCglmm(Total ~ Year +
                              Two.way + Location2 + 
                              Underpass.type + Human.total.x,
                            random = ~ Sampling.effort + Agecentred,
                            prior = prioexp, nitt=203000, burnin=20000, thin=150,
                            verbose = TRUE,
                            family = "poisson",
                            data = ungulates.annual)

setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/models/ungulates/annual/")
#save(ungulate.yrModel, file ="ungulate annual model.Rdata")
save(ungulate.yrModelExp, file ="ungulate annual model - exp.Rdata")
#ung.yrModel <- clean.MCMC(ungulate.yrModel)  # get all the info from summary(modelName)
ung.yrModelExp <- clean.MCMC(ungulate.yrModelExp)  # get all the info from summary(modelName)
#write.csv(ung.yrModel, "ungulate annual model - summary.csv")
write.csv(ung.yrModelExp, "ungulate annual modelEXP - summary.csv")


summary(ungulate.yrModel)
par(mar=c(1,1,1,1))
plot(ungulate.yrModel)
heidel.diag(ungulate.yrModel$VCV)
heidel.diag(ungulate.yrModel$Sol)
autocorr(ungulate.yrModel$Sol)
autocorr(ungulate.yrModelExp$VCV)
geweke.diag(ungulate.yrModelExp$Sol) # Should be between +-1.96 
        ### IG: all within range
        ### Exp: all within range
effectiveSize(ungulate.yrModel$Sol)
effectiveSize(ungulate.yrModel$VCV)
posterior.mode(ungulate.yrModel$Sol)
HPDinterval(ungulate.yrModel$Sol)
posterior.mode(ungulate.yrModel$VCV)
HPDinterval(ungulate.yrModel$VCV)





##################  ANNUAL carnivores ################# 

carnivore.yrModel = MCMCglmm(Total ~ Year +
                               Two.way + Location2 + 
                               Underpass.type + Human.total.x + Agecentred,
                             random = ~ Location + Sampling.effort,
                             prior = prior, nitt=203000, burnin=20000, thin=150,
                             verbose = TRUE,
                             family = "poisson",
                             data = carnivores.annual)

carnivore.yrModelExp = MCMCglmm(Total ~ Year +
                               Two.way + Location2 + 
                               Underpass.type + Human.total.x,
                             random = ~ Sampling.effort + Agecentred,
                             prior = prioexp, nitt=203000, burnin=20000, thin=150,
                             verbose = TRUE,
                             family = "poisson",
                             data = carnivores.annual)

setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/models/carnivores/annual/")
#save(carnivore.yrModel, file ="carnivore annual model.Rdata")
save(carnivore.yrModelExp, file ="carnivore annual model - exp.Rdata")
#car.yrModel <- clean.MCMC(carnivore.yrModel)  # get all the info from summary(modelName)
car.yrModelExp <- clean.MCMC(carnivore.yrModelExp)  # get all the info from summary(modelName)
#write.csv(car.yrModel, "carnivore annual model - summary.csv")
write.csv(car.yrModelExp, "carnivore annual modelEXP - summary.csv")


summary(carnivore.yrModel)
par(mar=c(1,1,1,1))
plot(carnivore.yrModel)
heidel.diag(carnivore.yrModelExp$VCV)
heidel.diag(carnivore.yrModelExp$Sol)
autocorr(carnivore.yrModel$Sol)
autocorr(carnivore.yrModel$VCV)
geweke.diag(carnivore.yrModelExp$Sol) # Should be between +-1.96
        ### IG: Year2020 = -2.3
        ### Exp: all within range
effectiveSize(carnivore.yrModel$Sol)
effectiveSize(carnivore.yrModel$VCV)
posterior.mode(carnivore.yrModel$Sol)
HPDinterval(carnivore.yrModel$Sol)
posterior.mode(carnivore.yrModel$VCV)
HPDinterval(carnivore.yrModel$VCV)






# Re-load model summaries
load("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/models/ungulates/ungulate hourly model.Rdata")
load("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/models/carnivores hourly model.RData")

# setwd for adjusted P-values
setwd("/Users/eleonorelebeuf-taylor/Google Drive/Kananaskis crossing structures/analysis ELT/models/adjusted P-values/")


# Adjust p-values for multiple comparisons using FDR
### HOURLY ungulates: Ungulates2 IG
adjP.ung.hr = p.adjust(ung.hrModel2$pMCMC, method = "fdr", n = length(ung.hrModel2$pMCMC))
adjP.ung.hrDF = data.frame(adjP.ung.hr)
adjP.ung.hrDF$X = 1:nrow(adjP.ung.hrDF)
ung.hrModel2$X = 1:nrow(ung.hrModel2)
adj.ung.hrModel = left_join(ung.hrModel2, adjP.ung.hrDF, by = "X")
write.csv(adj.ung.hrModel, "ungulate hourly summary table_adjusted P.csv")


### HOURLY carnivores: Carnivores2 IG
adjP.car.hr = p.adjust(car.hrModel2$pMCMC, method = "fdr", n = length(car.hrModel2$pMCMC))
adjP.car.hrDF = data.frame(adjP.car.hr)
adjP.car.hrDF$X = 1:nrow(adjP.car.hrDF)
car.hrModel2$X = 1:nrow(car.hrModel2)
adj.car.hrModel = left_join(car.hrModel2, adjP.car.hrDF, by = "X")
write.csv(adj.car.hrModel, "carnivore hourly summary table_adjusted P.csv")



### MONTHLY ungulates Ungulates2 Expanded
adjP.ung.mo = p.adjust(ung.moModelExp2$pMCMC, method = "fdr", n = length(ung.moModelExp2$pMCMC))
adjP.ung.moDF = data.frame(adjP.ung.mo)
adjP.ung.moDF$X = 1:nrow(adjP.ung.moDF)
ung.moModelExp2$X = 1:nrow(ung.moModelExp2)
adj.ung.moModel = left_join(ung.moModelExp2, adjP.ung.moDF, by = "X")
write.csv(adj.ung.moModel, "ungulate monthly model table_adjusted P.csv")


### MONTHLY carnivores: Carnivores2 Expanded
adjP.car.mo = p.adjust(car.moModelExp2$pMCMC, method = "fdr", n = length(car.moModelExp2$pMCMC))
adjP.car.moDF = data.frame(adjP.car.mo)
adjP.car.moDF$X = 1:nrow(adjP.car.moDF)
car.moModelExp2$X = 1:nrow(car.moModelExp2)
adj.car.moModel = left_join(car.moModelExp2, adjP.car.moDF, by = "X")
write.csv(adj.car.moModel, "carnivore monthly model table_adjusted P.csv")



### ANNUAL ungulates: Expanded
adjP.ung.yr = p.adjust(ung.yrModelExp$pMCMC, method = "fdr", n = length(ung.yrModelExp$pMCMC))
adjP.ung.yrDF = data.frame(adjP.ung.yr)
adjP.ung.yrDF$X = 1:nrow(adjP.ung.yrDF)
ung.yrModelExp$X = 1:nrow(ung.yrModelExp)
adj.ung.yrModel = left_join(ung.yrModelExp, adjP.ung.yrDF, by = "X")
write.csv(adj.ung.yrModel, "ungulate annual model table_adjusted P.csv")


### ANNUAL carnivores: Expanded
adjP.car.yr = p.adjust(car.yrModelExp$pMCMC, method = "fdr", n = length(car.yrModelExp$pMCMC))
adjP.car.yrDF = data.frame(adjP.car.yr)
adjP.car.yrDF$X = 1:nrow(adjP.car.yrDF)
car.yrModelExp$X = 1:nrow(car.yrModelExp)
adj.car.yrModel = left_join(car.yrModelExp, adjP.car.yrDF, by = "X")
write.csv(adj.car.yrModel, "carnivore annual model table_adjusted P.csv")




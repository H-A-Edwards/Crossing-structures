library(lmtest)

#simulate a bunch of autocorrelated time series 
b0 <- 12       #mean 
rho <- 0.5     #auto-correlation
sigma <- 3     #sampling SD
sims <- 100    #number of data sets 
times <- 10000 #how many samples per series   
junk <- matrix(NA,nrow = times, ncol = sims) #store data 
junk[1,] <- b0 #initialize time series 
sig <- rep(NA,sims) #store Durbin-Watson significance values 
for(s in 1:sims){
  for(t in 2:times){
    junk[t,s] <- b0 + junk[t-1,s]*rho + rnorm(1,0,sigma) #generate time series
  }
  mod <- lm(junk[,s] ~ 1) #fit a model of the mean 
  sig[s] <- dwtest(mod)$p.value #look at significance of D-W test 
} 
sd <- sqrt(apply(junk,2,var)) #calculate sd of the data (if rho = 0, this should be ~ sigma) 
mean(sd) #the mean SD  
mean(sig) #what is mean significance value 
hist(sig) #plot significance values 

#now thin the data, and try again 
thin.val <- 10 #how much to thin (e.g., thin.val = 10 takes every 10th value)
thin <- matrix(NA,nrow=times/thin.val,ncol=sims)
sig.thin <- rep(NA,sims)
for(s in 1:sims){
  thin[,s] <- junk[seq(from = 10, to = times, by = thin.val),s]
  mod <- lm(thin[,s] ~ 1)
  sig.thin[s] <- dwtest(mod)$p.value
}
sd <- sqrt(apply(thin,2,var))
mean(sd)
mean(sig.thin) #mean significance value 
hist(sig.thin) #plot significance values

acf(junk[,1], plot = TRUE)
acf(thin[,1], plot = TRUE)


#Read in the egg data 
data <- read.csv("P4_Y16no1.csv")
#Create temp mean and var across the three sensors
data$temp_mean_3_sensors <- apply(cbind(data$Temp1., data$Temp2., data$Temp3.), 1, mean)
data$temp_var_3_sensors <- var(data$temp_mean_3_sensors)
temp <- data$temp_mean_3_sensors 

#build a model of the mean 
mod <- lm(temp ~ 1)
dwtest(mod) #DW test (tests the residuals)
#also do the Box-Pierce an Ljung-Box tests which test based on the data
Box.test(temp, lag = 1, type = c("Box-Pierce"), fitdf = 0)
Box.test(temp, lag = 1, type = c("Ljung-Box"), fitdf = 0)
acf(temp) #plot acf
var(temp)
mean(temp)

thin.val <- 400 #now thin and keep each (thin.val)th value 
thin <- temp[seq(from = 1, to = length(temp), by = thin.val)]
mod.thin <- lm(thin ~ 1)
dwtest(mod.thin) #DW test 
Box.test(thin, lag = 1, type = c("Box-Pierce"), fitdf = 0)#Box-Pierce
Box.test(thin, lag = 1, type = c("Ljung-Box"), fitdf = 0)#Ljung-Box
acf(thin) #plot acf 
var(thin)
mean(thin)


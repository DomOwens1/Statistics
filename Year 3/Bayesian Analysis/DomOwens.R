radon <- read.table('DataSheet6/radon.txt', sep='')
set.seed(333)
library(MASS)
library(rjags)
library(coda)
load.module('glm')

#2. Estimate your model. In particular:
#a) (15 marks) Implement your model in JAGS.

#write model to radon.bug
cat('
    model{  
    for(j in 1:M){
    theta[j] ~ dnorm(alpha*uu[j], sigma2^(-2))
    }
    for(i in 1:n){
    x[i] ~ dnorm( (theta[county.index[i]]) + beta*B[i], sigma^(-2))
    }
    
    alpha ~ dnorm(0,0.0001)
    beta ~ dnorm(0,0.001)
    sigma ~ dunif(0,100)
    sigma2 ~ dunif(0,10000)
    }', file='radon.bug')

#specify determined parts
radon_data<-list(n=nrow(radon), M=max(radon$county.index), x=radon$log.radon, county.index = radon$county.index,
                 B=radon$floor, uu = unique(radon$uu))
#specify prior points
radon_inits <- list(alpha=0,beta=0,sigma=1,sigma2=1)

#construct model in jags
radon_mu<-jags.model('radon.bug',inits = radon_inits,data=radon_data)
#verify we are sampling correct quantities, and using correct samplers
list.samplers(radon_mu)


#b) (15 marks) Simulate a trajectory of length T and justify your choice for T
#set burn-in length
Burn <- 2000
#set trajectory length
Tr <- 10000
#update chain by burn-in length
update(radon_mu, n.iter = Burn)
#sample from distribution with correct quantities
sample<-coda.samples(radon_mu, variable.names = c("alpha","theta","beta","sigma","sigma2"), n.iter=Tr)

#plot autocorrelations for parameters to verify convergence
autocorr.plot(sample[,1:7], 100)
#These have rapidly-declining autocorrelation, indicating the chain has converged for all parameters

traceplot(sample[,1:7])
#These appear as white noise, centred on the parameter estimate, indicating the chain has converged to a stationary distribution
summary(sample)$statistics
#All the standard deviations are small, supporting convergence 




#3. Provide an answer to the above problem P1. In particular,
#a) (5 marks) For each county in the dataset, compute the average log-radon
#level of a house with a basement. Make a plot with these estimates on the
#y-axis and the number of observations per county on the x-axis. Add in the
#plot an horizontal line that represents the average log-radon level of a house
#with a basement for Minnesota.

#select houses with basements
radon_bm <- radon[radon$floor==1,]
#find average and counts by county 
county_means <- rep(0, 84)
county_count <- rep(0, 84)
for(i in 1:84){
  county_means[i] <- mean(radon_bm$log.radon[radon_bm$county.index == i])
  county_count[i] <- nrow(radon_bm[radon_bm$county.index == i,])
}

#plot means against counts
plot(county_count,county_means)
#find overall mean and overlay as line
state_mean <- mean(radon_bm$log.radon)
abline(a=state_mean, b=0)

#b) (15 marks) Using your model, predict for each county in the dataset the
#log-radon level of a house with a basement. Explain how these predictions
#have been computed from your model and make a plot with these estimates
#on the y-axis and the number of observations per county on the x-axis. Add
#in the plot an horizontal line that represents the average log-radon level of a
#house with a basement for Minnesota.

#select estimates for parameters as a new vector
pars <- summary(sample)$statistics[,1]
#predict log radon for each county, assuming house has a basement, using model equation
preds <- rep(0, 84)
for(i in 1:84){
  preds[i] <-  pars[1]*pars[i+4] + pars[2]
}

#plot predictions against counts by county
plot(county_count, preds)
abline(a=state_mean, b=0)

#c) (5 marks) Compare the `naive' estimates obtained in part 3.a) with the model
#based estimates obtained in part 3.b). Comment your results.

#plot estimates against each other
plot(county_means, preds)

#find mean square error of both estimates
mean((county_means-preds)^2)
##0.144199

#find average log radon for houses with a basement in state
state_mean
##1.328477

#find average predicted log radon for houses with a basement in state
(sum(preds*county_count))/nrow(radon_bm)
##1.257926




#4. (15 marks) Provide an answer to the above problem P2, that is use your model
#to predict the log-radon level of a house with no basement and of a house with a
#basement located in Cook, given that the soil uranium measurement for this county
#is equal to -0.504995982525783. Explain how your prediction has been computed
#from your model.

u_cook <- -0.504995982525783
#Predict the log-radon level of a house with no basement in the county of Cook
pars[1]*u_cook 
##-0.3191636 

#Predict the log-radon level of a house with a basement in the county of Cook
pars[1]*u_cook + pars[2]
##0.8216265
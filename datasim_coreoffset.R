#TM's project
#data simulation to figure out modeling approach for offset tree cores

#setup work environment
library(ggplot2)
library(dplyr)
library(tidyr)
require(lubridate)
require(rjags)


##########
rm(list = ls())


###########
#data simulation
###########
y <- 1:100
#weather data
w <- rnorm(100, mean = 0, sd = 1)

#growth data for one tree
g <- rep(NA, 100)
for(i in 1:100){
  g[i] <- w[i] * 0.5 + rnorm(n = 1, mean =0, sd = 0.5)
}

wg <- data.frame(w, g, y)


ggplot(wg, aes(x = y, y = g, color = w)) + geom_point(size = 5) + 
  scale_color_gradient2(low = "blue", mid = "white", high = "red")

ggplot(wg, aes(x = w, y = g)) + geom_point() + geom_smooth(method = "lm")

#growth of many trees
g <- matrix(NA, nrow = 100, ncol = 30)

for(i in 1:30){
  for(j in 1:100){
    g[j,i] <- w[j] * 1 + rnorm(n = 1, mean =0, sd = 0.5)
  }
}

wg <- data.frame(g, w, y)
wgy <- gather(wg, "tree","growth",1:30) #convert to long form for visualization
wgy$tree <- as.numeric(wgy$tree)
names(wgy)
head(wgy)

ggplot(wgy, aes(x = y, y = growth, color = as.factor(tree))) + geom_point()
ggplot(wgy, aes(x = w, y = growth, color = as.factor(tree))) +geom_point() + geom_smooth(method = "lm", se = FALSE)

#offset individual trees
for(i in 1:10){wgy$growth[wgy$tree == i] <- lead(wgy$growth[wgy$tree == i])}
ggplot(wgy, aes(x = w, y = growth, color = as.factor(tree))) +geom_point() + geom_smooth(method = "lm", se = FALSE)

#convert back to wide for the model
wgy2 <- subset(wgy, select = - c(w))
g2 <- spread(wgy2, tree, growth)

#create a model that can detect which trees are offset

sink("model.txt")
cat("   
    model{   
    
    for(t in 1:30){
    for(y in 3:97){
      growth[y,t] ~ dnorm(mu[y,t], tau)
      mu[y,t] <- alpha * w[y + offset[t]]
    }
    }
    
    #priors
    tau <- pow(sigma, -2)
    sigma ~ dunif(0,10)
    alpha ~ dnorm(0, 0.001)

    for(i in 1:30){

offset[i] ~ dbern(0.5)}
    }
    ",fill=TRUE)
sink() 


jags <- jags.model('model.txt',
                   data = list(                     
                     'growth' = g2,  #g2 has the offset data 
                     'w' = w),
                   n.chains = 3,
                   n.adapt = 100)

mcmc_samples <- coda.samples(jags, variable.names=c("sigma","alpha", "offset"),  n.iter= 500)

#plot(mcmc_samples)
resultsall <- summary(mcmc_samples) 
results <- data.frame(resultsall$statistics,resultsall$quantiles) 

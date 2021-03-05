###let's do an individual learning sim from first principals using EWA model.
##we can start with a continuous simulation, and add agents and heterogeneity later

######Functions

######create a softmax function to simply code
Softmax <- function(x){
  exp(x)/sum(exp(x))
}

#color palate
col.pal <- c("#1B9E77", "#D95F02")
# green (low payoff) and red (high payoff)

#simulate data set conditions
dsim <- data.frame( timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
timesteps <- 100 # number of timesteps (t), in snail example, every interaction with a snail is a new timestep
phi <- 0.1 # low is more reliance on past memory and high is reliance on recent experience (0-1)
lambda <- 0.2 # sensitivity to differences in attraction, low is insensitive (0 is no attention to differences in attraction score, equal chance to pick each behavior), higher is more sensitive (0-infinity)
techmeans <- c(8,12) # pay offs of behaviors 1 and 2 
techvar <- c(1,1) # variance in pay offs of behavior across time steps
#visualize how hard to learn
plot(density(rnorm( 10000, mean=techmeans[1] , sd=techvar[1] ) ) ,col=col.pal[1] , xlim=c(0,20)) 
lines(density(rnorm( 10000, mean=techmeans[2] , sd=techvar[2] ) ) ,col=col.pal[2]) 

AC <- matrix(0,nrow=timesteps,ncol=2) 	

##simulated data for one discreet individual

for (t in 1:timesteps){
    prtech_i <-  Softmax(lambda*AC[t,]) #calculate probability of performing a behavior at this timestep
    tech <- sample( 1:2 , size=1 , prob=prtech_i) # sample a behavior with prtech_i
    payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff
    
    dsim[t,] <- c(  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , AC[t,1] , AC[t,2] ,  prtech_i[1] , prtech_i[2] )
    
    # update attractions for next timestep t + 1, don't do on final round
     if(t<timesteps){ 
      for (k in 1:2){
        AC[t+1,k] <- (1-phi)*AC[t,k] + phi*obs_payoffs_t[k]
      }
    }
  }

#below plots sim
plot(dsim$Pr1~dsim$timestep , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1.1) ) 
points(dsim$Pr2~dsim$timestep,col=col.pal[2],pch=19 )
points(rep(1.05,timesteps) ~ dsim$timestep,col=col.pal[dsim$tech],pch=19 )
abline(h=1)

####### mean dynamics of model i, no variance, just a numerical solution
dsim2 <- data.frame( timestep=0 , A1=0 , A2=0 , Pr1=0 , Pr2=0)

for (t in 1:timesteps){
  prtech_i <-  Softmax(lambda*AC[t,]) #calculate probability of performing a behavior at this timestep
  obs_payoffs_t <- techmeans #initialize observed payoffs vector

  dsim2[t,] <- c(  t , AC[t,1] , AC[t,2] ,  prtech_i[1] , prtech_i[2] )
  
  # update attractions for next timestep t + 1, don't do on final round
  if(t<timesteps){ 
    for (k in 1:2){
      AC[t+1,k] <- (1-phi)*AC[t,k] + phi*obs_payoffs_t[k]
    }
  }
}

#below plots sims
plot(dsim2$Pr1~dsim2$timestep , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1) ) 
points(dsim2$Pr2~dsim2$timestep,col=col.pal[2],pch=19 )


###lets write a simple function to plot dynamics on average

sim_reinf_learn <- function(phi,lambda){
  techmeans <- c(8,12)
  timesteps <- 100
  dsim2 <- data.frame( timestep=0 , A1=0 , A2=0 , Pr1=0 , Pr2=0)
  
  for (t in 1:timesteps){
    prtech_i <-  Softmax(lambda*AC[t,]) #calculate probability of performing a behavior at this timestep
    obs_payoffs_t <- techmeans #initialize observed payoffs vector
    dsim2[t,] <- c(  t , AC[t,1] , AC[t,2] ,  prtech_i[1] , prtech_i[2] )
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AC[t+1,k] <- (1-phi)*AC[t,k] + phi*obs_payoffs_t[k]
      }
    }
  }
  #below plots sims
  plot(dsim2$Pr1~dsim2$timestep , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1) ) 
  points(dsim2$Pr2~dsim2$timestep,col=col.pal[2],pch=19 )
  
}

##run some functions to explore average dynamics across simulation space
sim_reinf_learn(phi=0.5,lambda=0.1)
sim_reinf_learn(phi=0.9,lambda=0.1)
sim_reinf_learn(phi=0.01,lambda=0.3)
sim_reinf_learn(phi=0.1,lambda=0.3)
sim_reinf_learn(phi=0.5,lambda=0.3)
sim_reinf_learn(phi=0.1,lambda=1)
sim_reinf_learn(phi=0.9,lambda=1)

######### From down here Zoë's experimental space ############
# general set-up (including looping over individuals)

######create a softmax function to simply code
Softmax <- function(x){
  exp(x)/sum(exp(x))
}
# ask brendan why exponentional (you never get object with zero attraction)
# alexander says the exponential means that when calculating probtech the higher the lambda the bigger the difference between the two behaviors


#color palate
col.pal <- c("#1B9E77", "#D95F02")
# green (low payoff) and red (high payoff)

# Things that don't vary
individuals <- 20 # number of individuals 
timesteps <- 100 # number of timesteps (t), in snail example, every interaction with a snail is a new timestep

####### Loop over individuals, individuals all the same ######
# other parameters
phi <- 0.1 # low is more reliance on past memory and high is reliance on recent experience (0-1)
lambda <- 0.2 # sensitivity to differences in attraction, low is insensitive (0 is no attention to differences in attraction score, equal chance to pick each behavior), higher is more sensitive (0-infinity)
techmeans <- c(8,12) # pay offs of behaviors 1 and 2 
techvar <- c(1,1) # variance in pay offs of behavior across time steps

##simulated data looping over individuals
AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) ) # empty array for storing payoffs
dsim <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, A1=0 , A2=0 , Pr1=0 , Pr2=0) # data frame for storing simulation results
therow <- 1

# run loop
for (t in 1:timesteps){
  for (i in 1:individuals) {
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
    tech <- sample( 1:2 , size=1 , prob=prtech_i) # sample a behavior with prtech_i
    payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff
  
    dsim[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2] )
    therow <- therow + 1
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AR[t+1,k,i] <- (1-phi)*AR[t,k,i] + phi*obs_payoffs_t[k]
      }
    }
  }
}

#below plots sim
for (i in 1:individuals) {
  plot(dsim$Pr1[dsim$individual == i] ~dsim$timestep[dsim$individual == i] , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1.1) ) 
  points(dsim$Pr2[dsim$individual == i]~dsim$timestep[dsim$individual == i],col=col.pal[2],pch=19 )
  points(rep(1.05,timesteps) ~ dsim$timestep[dsim$individual == i],col=col.pal[dsim$tech[dsim$individual == i]],pch=19 )
  abline(h=1)
} 

###### Varying phi per individual ########
# other parameters
# Varying phi
logit <- function(p){
  (log(p/(1-p)))
}

logistic <- function(x){
  (1/(1+exp(-x)))
}

phi.sim <- logit(0.25) #population mean phi based on previous simulations with vervets/capuchins/etc (see payoff bias paper Brendan)
phi.sim_i <- rnorm(individuals, mean = 0, sd = 0.7) # can set standard deviation
phi_id <- round(logistic(phi.sim +phi.sim_i), digits=2)

##simulated data looping over individuals
AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) )
dsim <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
therow <- 1

# run loop
for (t in 1:timesteps){
  for (i in 1:individuals) {
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
    tech <- sample( 1:2 , size=1 , prob=prtech_i) # sample a behavior with prtech_i
    payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff
    
    dsim[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2])
    therow <- therow + 1
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AR[t+1,k,i] <- (1-phi_id[i])*AR[t,k,i] + phi_id[i]*obs_payoffs_t[k]
      }
    }
  }
}

#plotting sims
for (i in 1:individuals) {
  plot(dsim$Pr1[dsim$individual == i] ~dsim$timestep[dsim$individual == i] , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1.1) ) 
  points(dsim$Pr2[dsim$individual == i]~dsim$timestep[dsim$individual == i],col=col.pal[2],pch=19 )
  points(rep(1.05,timesteps) ~ dsim$timestep[dsim$individual == i],col=col.pal[dsim$tech[dsim$individual == i]],pch= 19)
  abline(h=1)
  title(main = paste("id =", i, ", lambda =", lambda, ", phi =", round(phi_id[i],2)))
} 

#### Payoffs of behaviors switching halfway through ####
# introducing change in payoff
techmeans_bas <- c(8,12) # pay offs of behaviors 1 and 2 baseline
techmeans_exp <- c(12,8) # pay offs switched (experiment)

##simulated data looping over individuals
AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) )
dsim <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
therow <- 1

for (t in 1:timesteps){
  for (i in 1:individuals) {
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
    tech <- sample( 1:2 , size=1 , prob=prtech_i) # sample a behavior with prtech_i
    techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
    payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff
    
    dsim[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2] )
    therow <- therow + 1
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AR[t+1,k,i] <- (1-phi_id[i])*AR[t,k,i] + phi_id[i]*obs_payoffs_t[k]
      }
    }
  }
}

##plot sims
for (i in 1:individuals) {
  plot(dsim$Pr1[dsim$individual == i] ~dsim$timestep[dsim$individual == i] , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1.1) ) 
  points(dsim$Pr2[dsim$individual == i]~dsim$timestep[dsim$individual == i],col=col.pal[2],pch=19 )
  points(rep(1.05,timesteps) ~ dsim$timestep[dsim$individual == i],col=col.pal[dsim$tech[dsim$individual == i]],pch= 19)
  abline(h=1)
  abline(v = timesteps/2, lty = 2)
  title(main = paste("id =", i, ", lambda =", lambda, ", phi =", round(phi_id[i],2)))
} 

##### Using efficiency rather than absolute payoffs ####
# use efficiency rather than absolute payoffs (the techmeans from before). Find this more intuitive, so chance of opening snail, when payoff of snail flesh is the same
techprsucceed_bas <- c(0.50, 0.95)
techprsucceed_exp <- c(0.50, 0.01)
lambda = 1 #needs to be high to see variation

##simulated data looping over individuals
AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) )
dsim <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, A1=0 , A2=0 , Pr1=0 , Pr2=0, succeed = 0)
therow <- 1

for (t in 1:timesteps){
  for (i in 1:individuals) {
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
    tech <- sample( 1:2 , size=1 , prob=prtech_i) # sample a behavior with prtech_i
    succeed <- if (t > timesteps/2) {sample(rbinom(1,1,techprsucceed_exp[tech]))} else {sample(rbinom(1,1,techprsucceed_bas[tech]))} #introduce sample whether it was successful
    payoff <- succeed # right now the payoff is 1 or 0, so succeed yes or no. Can still change this (to also account for time needed to get payoff for ex)
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff
    dsim[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2], succeed )
    therow <- therow + 1
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AR[t+1,k,i] <- (1-phi_id[i])*AR[t,k,i] + phi_id[i]*obs_payoffs_t[k]
      }
    }
  }
}

##plot sims
# add points showing whether succeeded or not (empty circle is not succeed, filled is succeed)
pch.pal <- c(1,19)

for (i in 1:individuals) {
  plot(dsim$Pr1[dsim$individual == i] ~dsim$timestep[dsim$individual == i] , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1.1) ) 
  points(dsim$Pr2[dsim$individual == i]~dsim$timestep[dsim$individual == i],col=col.pal[2],pch=19 )
  points(rep(1.05,timesteps) ~ dsim$timestep[dsim$individual == i],col=col.pal[dsim$tech[dsim$individual == i]],pch= pch.pal[1 + dsim$succeed[dsim$individual == i]])
  abline(h=1)
  abline(v = timesteps/2, lty = 2)
  title(main = paste("id =", i, ", lambda =", lambda, ", phi =", round(phi_id[i],2)))
} 

## To make PDF of output
pdf("freq_sims_zg.pdf", width = 9, height = 11)
par(mfrow=c(5,2)) #sets number of rows and columns per page, could also change margins
par(cex = 0.5)

#plot code here

dev.off()


##### Function to show deterministic simulation (average) #####
AC <- matrix(0,nrow=timesteps,ncol=2) 	

sim_reinf_learn <- function(phi,lambda){
  techprsucceed_bas <- c(0.50, 0.95)
  techprsucceed_exp <- c(0.50, 0.01)
  timesteps <- 100
  dsim2 <- data.frame( timestep=0 , A1=0 , A2=0 , Pr1=0 , Pr2=0)
  
  for (t in 1:timesteps){
    prtech_i <-  Softmax(lambda*AC[t,]) #calculate probability of performing a behavior at this timestep
    obs_payoffs_t <- if (t > timesteps/2) {techprsucceed_exp} else {techprsucceed_bas} #initialize observed payoffs vector
    dsim2[t,] <- c(  t , AC[t,1] , AC[t,2] ,  prtech_i[1] , prtech_i[2] )
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AC[t+1,k] <- (1-phi)*AC[t,k] + phi*obs_payoffs_t[k]
      }
    }
  }
  #below plots sims
  plot(dsim2$Pr1~dsim2$timestep , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1) ) 
  points(dsim2$Pr2~dsim2$timestep,col=col.pal[2],pch=19 )
  abline(v = timesteps/2, lty = 2)
  title(main = paste("lambda =", lambda, ", phi =", phi))
}

##run some functions to explore average dynamics across simulation space
sim_reinf_learn(phi=0.05,lambda=1)
sim_reinf_learn(phi=0.05,lambda=5)
sim_reinf_learn(phi=0.05,lambda=10)
sim_reinf_learn(phi=0.1,lambda=10)
sim_reinf_learn(phi=0.2,lambda=10)
sim_reinf_learn(phi=0.5,lambda=10)

##### Making the nice plot for presentations of average effect at different phi's and lambdas #######

techprsucceed_bas <- c(0.50, 0.95)
techprsucceed_exp <- c(0.50, 0.01)
timesteps <- 100
phi_v <- c(0.05, 0.1, 0.2, 0.5) #vector with phi values
lambda_v <- c(1,5,10)
#add lambda loop on outside

AR <- array(0 , dim=c( nrow=timesteps , 2 , length(phi_v), length(lambda_v)) ) 
dsim2 <- data.frame( timestep=0 , A1=0 , A2=0 , Pr1=0 , Pr2=0, phi =0, lambda =0)
therow <- 1

for (l in 1:length(lambda_v)) { 
  for (p in 1:length(phi_v)) {
    for (t in 1:timesteps){
      prtech_i <-  Softmax(lambda_v[l]*AR[t,,p,l]) #calculate probability of performing a behavior at this timestep
      obs_payoffs_t <- if (t > timesteps/2) {techprsucceed_exp} else {techprsucceed_bas} #initialize observed payoffs vector
      dsim2[therow,] <- c(  t , AR[t,1,p,l] , AR[t,2,p,l] ,  prtech_i[1] , prtech_i[2], phi_v[p], lambda_v[l])
      therow <- therow + 1
      # update attractions for next timestep t + 1, don't do on final round
      if(t<timesteps){ 
        for (k in 1:2){
          AR[t+1,k,p,l] <- (1-phi_v[p])*AR[t,k,p,l] + phi_v[p]*obs_payoffs_t[k]
        }
      }
    }
  }
}
# make variable for probability of high payoff behavior
for (i in 1:nrow(dsim2)) {
  dsim2$Pr_highpay[i] <- if (dsim2$timestep[i] > timesteps/2) {dsim2$Pr1[i]} else {dsim2$Pr2[i]}
}

# make variable for tool use or non tool use is high payoff. Circle is tool use, triangle is pounding
for (i in 1:nrow(dsim2)) {
  dsim2$point_tu[i] <- if (dsim2$timestep[i] < timesteps/2) {19} else {17}
}

# plot for presentation
library(RColorBrewer)
col.pal.phi <- brewer.pal(4, "Set2") #can give palette from r color brewer or do linetypes or both

# save as pdf
pdf("pres_sims_zg.pdf", width = 9, height = 11)
par(mfrow=c(3,1)) #sets number of rows and columns per page, could also change margins
par( mar=c(4,5,2,2))
par(cex = 0.8)

for (l in 1:length(lambda_v)) {
  plot(dsim2$Pr1~dsim2$timestep , col="white" , pch=19 , xlab="timestep" , ylab="prob choose high payoff behavior", ylim=c(0,1.3) ) 
  abline(v = timesteps/2, lty = 2)
  
  for (p in 1:length(phi_v)) {
    points(dsim2$Pr_highpay[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]]~dsim2$timestep[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]],col=col.pal.phi[p],
           pch= dsim2$point_tu[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]])
}
title(main = paste("lambda =", lambda_v[l]))
legend("top", as.character(phi_v), pch = 19, col = col.pal.phi, title = "phi", horiz=TRUE, bg = "white")
legend("topright", c("Tool use", "Pounding"), pch = c(19,17), col = "black")
}

dev.off()

#### Exploring tool use vs non-tool use groups ####
## tool users 

techprsucceed_bas <- c(0.50, 0.95)
techprsucceed_exp <- c(0.50, 0.01)
timesteps <- 100
phi_v <- c(0.05, 0.1, 0.2, 0.5) #vector with phi values
lambda_v <- c(1,5,10)
#add lambda loop on outside

AR <- array(0 , dim=c( nrow=timesteps , 2 , length(phi_v), length(lambda_v)) )
AR[1,1,,] <- 6.8 # attraction score first behavior (pounding)
AR[1,2,,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use
dsim2 <- data.frame( timestep=0 , A1=0 , A2=0 , Pr1=0 , Pr2=0, phi =0, lambda =0)
therow <- 1

for (l in 1:length(lambda_v)) { 
  for (p in 1:length(phi_v)) {
    for (t in 1:timesteps){
      prtech_i <-  Softmax(lambda_v[l]*AR[t,,p,l]) #calculate probability of performing a behavior at this timestep
      obs_payoffs_t <- if (t > timesteps/2) {techprsucceed_exp} else {techprsucceed_bas} #initialize observed payoffs vector
      dsim2[therow,] <- c(  t , AR[t,1,p,l] , AR[t,2,p,l] ,  prtech_i[1] , prtech_i[2], phi_v[p], lambda_v[l])
      therow <- therow + 1
      # update attractions for next timestep t + 1, don't do on final round
      if(t<timesteps){ 
        for (k in 1:2){
          AR[t+1,k,p,l] <- (1-phi_v[p])*AR[t,k,p,l] + phi_v[p]*obs_payoffs_t[k]
        }
      }
    }
  }
}
# make variable for probability of high payoff behavior
for (i in 1:nrow(dsim2)) {
  dsim2$Pr_highpay[i] <- if (dsim2$timestep[i] > timesteps/2) {dsim2$Pr1[i]} else {dsim2$Pr2[i]}
}

# make variable for tool use or non tool use is high payoff. Circle is tool use, triangle is pounding
for (i in 1:nrow(dsim2)) {
  dsim2$point_tu[i] <- if (dsim2$timestep[i] < timesteps/2) {19} else {17}
}

# plot for presentation
for (l in 1:length(lambda_v)) {
  plot(dsim2$Pr1~dsim2$timestep , col="white" , pch=19 , xlab="timestep" , ylab="prob choose high payoff behavior", ylim=c(0,1.3) ) 
  abline(v = timesteps/2, lty = 2)
  
  for (p in 1:length(phi_v)) {
    points(dsim2$Pr_highpay[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]]~dsim2$timestep[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]],col=col.pal.phi[p],
           pch= dsim2$point_tu[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]])
  }
  title(main = paste("lambda =", lambda_v[l]))
  legend("top", as.character(phi_v), pch = 19, col = col.pal.phi, title = "phi", horiz=TRUE, bg = "white")
  legend("topright", c("Tool use", "Pounding"), pch = c(19,17), col = "black")
}

# looking only at tool use not at behavior with highest payoff
for (l in 1:length(lambda_v)) {
  plot(dsim2$Pr1~dsim2$timestep , col="white" , pch=19 , xlab="timestep" , ylab="prob choose tool use", ylim=c(0,1.3) ) 
  abline(v = timesteps/2, lty = 2)
  
  for (p in 1:length(phi_v)) {
    points(dsim2$Pr2[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]]~dsim2$timestep[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]],col=col.pal.phi[p], pch = 19)
  }
  title(main = paste("lambda =", lambda_v[l]))
  legend("top", as.character(phi_v), pch = 19, col = col.pal.phi, title = "phi", horiz=TRUE, bg = "white")
  legend("topright", c("Tool use", "Pounding"), pch = c(19,17), col = "black")
}

## non tool users
techprsucceed_bas <- c(0.50, 0.95)
techprsucceed_exp <- c(0.50, 0.01)
timesteps <- 100
phi_v <- c(0.01, 0.05, 0.1, 0.2) #vector with phi values
lambda_v <- c(1,5,10)

AR <- array(0 , dim=c( nrow=timesteps , 2 , length(phi_v), length(lambda_v)) )
AR[1,1,,] <- 700 # attraction score first behavior (pounding)
AR[1,2,,] <- 0 # attraction score second behavior (tool use) will get translated to probabilities of nearly 1 pounding and about 0 tool use
dsim2 <- data.frame( timestep=0 , A1=0 , A2=0 , Pr1=0 , Pr2=0, phi =0, lambda =0)
therow <- 1

for (l in 1:length(lambda_v)) { 
  for (p in 1:length(phi_v)) {
    for (t in 1:timesteps){
      prtech_i <-  Softmax(lambda_v[l]*AR[t,,p,l]) #calculate probability of performing a behavior at this timestep
      obs_payoffs_t <- if (t > timesteps/2) {techprsucceed_exp} else {techprsucceed_bas} #initialize observed payoffs vector
      dsim2[therow,] <- c(  t , AR[t,1,p,l] , AR[t,2,p,l] ,  prtech_i[1] , prtech_i[2], phi_v[p], lambda_v[l])
      therow <- therow + 1
      # update attractions for next timestep t + 1, don't do on final round
      if(t<timesteps){ 
        for (k in 1:2){
          AR[t+1,k,p,l] <- (1-phi_v[p])*AR[t,k,p,l] + phi_v[p]*obs_payoffs_t[k]
        }
      }
    }
  }
}
# make variable for probability of high payoff behavior
for (i in 1:nrow(dsim2)) {
  dsim2$Pr_highpay[i] <- if (dsim2$timestep[i] > timesteps/2) {dsim2$Pr1[i]} else {dsim2$Pr2[i]}
}

# make variable for tool use or non tool use is high payoff. Circle is tool use, triangle is pounding
for (i in 1:nrow(dsim2)) {
  dsim2$point_tu[i] <- if (dsim2$timestep[i] < timesteps/2) {19} else {17}
}

# plot
for (l in 1:length(lambda_v)) {
  plot(dsim2$Pr1~dsim2$timestep , col="white" , pch=19 , xlab="timestep" , ylab="prob choose high payoff behavior", ylim=c(0,1.3) ) 
  abline(v = timesteps/2, lty = 2)
  
  for (p in 1:length(phi_v)) {
    points(dsim2$Pr_highpay[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]]~dsim2$timestep[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]],col=col.pal.phi[p],
           pch= dsim2$point_tu[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]])
  }
  title(main = paste("lambda =", lambda_v[l]))
  #legend("top", as.character(phi_v), pch = 19, col = col.pal.phi, title = "phi", horiz=TRUE, bg = "white")
  #legend("topright", c("Tool use", "Pounding"), pch = c(19,17), col = "black")
}

for (l in 1:length(lambda_v)) {
  plot(dsim2$Pr1~dsim2$timestep , col="white" , pch=19 , xlab="timestep" , ylab="prob choose high payoff behavior", ylim=c(0,1.3) ) 
  abline(v = timesteps/2, lty = 2)
  
  for (p in 1:length(phi_v)) {
    points(dsim2$Pr1[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]]~dsim2$timestep[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]],col=col.pal.phi[p],
           pch = 17)
  }
  title(main = paste("lambda =", lambda_v[l]))
  legend("top", as.character(phi_v), pch = 19, col = col.pal.phi, title = "phi", horiz=TRUE, bg = "white")
  legend("topright", c("Tool use", "Pounding"), pch = c(19,17), col = "black")
}

#### do this above stochastically for both tool use and non-tool use ### 

# using efficiency rather than absolute payoffs 
# tool users 

techprsucceed_bas <- c(0.50, 0.95) 
techprsucceed_exp <- c(0.50, 0.01) 
lambda = 5 #needs to be high to see variation 

#simulated data looping over individuals 
AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) ) 
dsim <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, A1=0 , A2=0 , Pr1=0 , Pr2=0, succeed = 0) 
therow <- 1 

AR[1,1,] <- 6.8 # attraction score first behavior (pounding) 
AR[1,2,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use 


for (t in 1:timesteps){ 
  for (i in 1:individuals) { 
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep 
    tech <- sample( 1:2 , size=1 , prob=prtech_i) # sample a behavior with prtech_i 
    succeed <- if (t > timesteps/2) {sample(rbinom(1,1,techprsucceed_exp[tech]))} else {sample(rbinom(1,1,techprsucceed_bas[tech]))} #introduce sample whether it was successful 
    payoff <- succeed # right now the payoff is 1 or 0, so succeed yes or no. Can still change this (to also account for time needed to get payoff for ex) 
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector 
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff 
    dsim[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2], succeed ) 
    therow <- therow + 1 
    # update attractions for next timestep t + 1, don't do on final round 
    if(t<timesteps){  
      for (k in 1:2){ 
        AR[t+1,k,i] <- (1-phi_id[i])*AR[t,k,i] + phi_id[i]*obs_payoffs_t[k] 
      } 
    } 
  } 
} 

##plot sims 
# add points showing whether succeeded or not (empty circle is not succeed, filled is succeed) 
pch.pal <- c(1,19) 

for (i in 1:individuals) { 
  plot(dsim$Pr1[dsim$individual == i] ~dsim$timestep[dsim$individual == i] , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1.1) )  
  points(dsim$Pr2[dsim$individual == i]~dsim$timestep[dsim$individual == i],col=col.pal[2],pch=19 ) 
  points(rep(1.05,timesteps) ~ dsim$timestep[dsim$individual == i],col=col.pal[dsim$tech[dsim$individual == i]],pch= pch.pal[1 + dsim$succeed[dsim$individual == i]]) 
  abline(h=1) 
  abline(v = timesteps/2, lty = 2) 
  title(main = paste("id =", i, ", lambda =", lambda, ", phi =", round(phi_id[i],2))) 
}  

# non tool users 

techprsucceed_bas <- c(0.50, 0.95) 
techprsucceed_exp <- c(0.50, 0.01) 
lambda = 1 #needs to be high to see variation 

#simulated data looping over individuals 
AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) ) 
dsim <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, A1=0 , A2=0 , Pr1=0 , Pr2=0, succeed = 0) 
therow <- 1 

AR[1,1,] <- 700 # attraction score first behavior (pounding) 
AR[1,2,] <- 0 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use 


for (t in 1:timesteps){ 
  for (i in 1:individuals) { 
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep 
    tech <- sample( 1:2 , size=1 , prob=prtech_i) # sample a behavior with prtech_i 
    succeed <- if (t > timesteps/2) {sample(rbinom(1,1,techprsucceed_exp[tech]))} else {sample(rbinom(1,1,techprsucceed_bas[tech]))} #introduce sample whether it was successful 
    payoff <- succeed # right now the payoff is 1 or 0, so succeed yes or no. Can still change this (to also account for time needed to get payoff for ex) 
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector 
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff 
    dsim[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2], succeed ) 
    therow <- therow + 1 
    # update attractions for next timestep t + 1, don't do on final round 
    if(t<timesteps){  
      for (k in 1:2){ 
        AR[t+1,k,i] <- (1-phi_id[i])*AR[t,k,i] + phi_id[i]*obs_payoffs_t[k] 
      } 
    } 
  } 
} 

##plot sims 
# add points showing whether succeeded or not (empty circle is not succeed, filled is succeed) 
pch.pal <- c(1,19) 

for (i in 1:individuals) { 
  plot(dsim$Pr1[dsim$individual == i] ~dsim$timestep[dsim$individual == i] , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1.1) )  
  points(dsim$Pr2[dsim$individual == i]~dsim$timestep[dsim$individual == i],col=col.pal[2],pch=19 ) 
  points(rep(1.05,timesteps) ~ dsim$timestep[dsim$individual == i],col=col.pal[dsim$tech[dsim$individual == i]],pch= pch.pal[1 + dsim$succeed[dsim$individual == i]]) 
  abline(h=1) 
  abline(v = timesteps/2, lty = 2) 
  title(main = paste("id =", i, ", lambda =", lambda, ", phi =", round(phi_id[i],2))) 
}  


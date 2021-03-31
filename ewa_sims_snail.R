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

=======
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
#pdf("freq_sims_zg.pdf", width = 9, height = 11)
#par(mfrow=c(5,2)) #sets number of rows and columns per page, could also change margins
#par(cex = 0.5)

#plot code here

#dev.off()


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
phi_v <- c(0.05, 0.1, 0.5) #vector with phi values, for presentation took out 0.2
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
col.pal.phi <- brewer.pal(3, "Greens") #can give palette from r color brewer or do linetypes or both. Set2 is nice for distinct colors
col.pal.phi2 <- c("#A1D99B", "#74C476", "#41AB5D")

# save as pdf
pdf("pres_sims_zg.pdf", width = 9, height = 11)
par(mfrow=c(3,1)) #sets number of rows and columns per page, could also change margins
par( mar=c(4,5,2,2))
par(cex = 0.8)

for (l in 1:length(lambda_v)) {
  plot(dsim2$Pr1~dsim2$timestep , col="white" , pch=19 , xlab="timestep" , ylab="prob choose high payoff behavior", ylim=c(0,1.3) ) 
  abline(v = timesteps/2, lty = 2)
  
  for (p in 1:length(phi_v)) {
    points(dsim2$Pr_highpay[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]]~dsim2$timestep[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]],col=col.pal.phi2[p],
           pch= dsim2$point_tu[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]])
}
title(main = paste("lambda =", lambda_v[l]))
legend("top", as.character(phi_v), pch = 19, col = col.pal.phi2, title = "phi", horiz=TRUE, bg = "white")
legend("topright", c("Tool use", "Pounding"), pch = c(19,17), col = "black")
}

dev.off()


# one phi with varying lambdas
for (p in 1:length(phi_v)) {
  plot(dsim2$Pr1~dsim2$timestep , col="white" , pch=19 , xlab="timestep" , ylab="prob choose high payoff behavior", ylim=c(0,1.3) ) 
  abline(v = timesteps/2, lty = 2)
  
  for (l in 1:length(lambda_v)) {
    points(dsim2$Pr_highpay[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]]~dsim2$timestep[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]],col=col.pal.phi2[l],
           pch= dsim2$point_tu[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[p]])
  }
  title(main = paste("phi =", phi_v[p]))
  legend("top", as.character(lambda_v), pch = 19, col = col.pal.phi2, title = "lambda", horiz=TRUE, bg = "white")
  legend("topright", c("Tool use", "Pounding"), pch = c(19,17), col = "black")
}

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
  legend("top", as.character(phi_v), pch = 19, col = col.pal.phi, title = "phi", horiz=TRUE, bg = "white")
  legend("topright", c("Tool use", "Pounding"), pch = c(19,17), col = "black")
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

#### do this above stochastically for both tool use and non-tool use ####


# using efficiency rather than absolute payoffs
# tool users

techprsucceed_bas <- c(0.50, 0.95)
techprsucceed_exp <- c(0.50, 0.01)
lambda = 1 #needs to be high to see variation

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

########## Plot conservatism of tool use behavior for proposal ########
# so basically above is the warming up/exploring and now we're getting closer to the real thing

# some basic things we need

######create a softmax function to simply code
Softmax <- function(x){
  exp(x)/sum(exp(x))
}

logit <- function(p){
  (log(p/(1-p)))
}

logistic <- function(x){
  (1/(1+exp(-x)))
}


#color palet
col.pal <- c("#1B9E77", "#D95F02")
# green (low payoff) and red (high payoff)

# Things that don't vary
individuals <- 100 # number of individuals 
timesteps <- 100 # number of timesteps (t), in snail example, every interaction with a snail is a new timestep

#setting varying phi per individual for stochastic model
phi.sim <- logit(0.15) #population mean phi based on previous simulations with vervets/capuchins/etc (see payoff bias paper Brendan) (0.15)
phi.sim_i <- rnorm(individuals, mean = 0, sd = 0.7) # can set standard deviation
phi_id <- round(logistic(phi.sim +phi.sim_i), digits=2)

### Individual learning only #####
## Stochastic
## model with sampling cost, don't think I will use this for now

techprsucceed_bas <-c(0.50, 0.95) # probability of each technique to succeed
techprsucceed_exp <- c(0.50, 0.01) # probability of each technique to succeed
lambda = 1 # needs to be high to see variation
samplecost <- -0.0001

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
    payoff <- samplecost + succeed # right now the payoff is 1 or 0, so succeed yes or no. Can still change this (to also account for time needed to get payoff for ex)
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
# Still see the drifting problem a bit, because attraction score of not sampled behavior always changes (function of phi)

### Other approach, dont use success/non success but techmeans. This I do use now!
techmeans_bas <- c(6,12) # pay offs of behaviors 1 and 2 baseline
techmeans_exp <- c(6,0.1) # pay offs changed (experiment)
techvar <- c(1,1) # variance in pay offs of behavior across time steps
lambda = 0.2 

AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) )
dsim <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, A1=0 , A2=0 , Pr1=0 , Pr2=0, succeed = 0)
therow <- 1

AR[1,1,] <- 6.8 # attraction score first behavior (pounding)
AR[1,2,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use


for (t in 1:timesteps){
  for (i in 1:individuals) {
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
    tech <- sample( 1:2 , size=1 , prob=prtech_i) # sample a behavior with prtech_i
    techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
    payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
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

for (i in 1:individuals) {
  plot(dsim$Pr1[dsim$individual == i] ~dsim$timestep[dsim$individual == i] , col=col.pal[1] , pch=19 , xlab="timestep" , ylab="prob choose behavior", ylim=c(0,1.1) ) 
  points(dsim$Pr2[dsim$individual == i]~dsim$timestep[dsim$individual == i],col=col.pal[2],pch=19 )
  points(rep(1.05,timesteps) ~ dsim$timestep[dsim$individual == i],col=col.pal[dsim$tech[dsim$individual == i]],pch= 16)
  abline(h=1)
  abline(v = timesteps/2, lty = 2)
  title(main = paste("id =", i, ", lambda =", lambda, ", phi =", round(phi_id[i],2)))
} 

## Deterministic plot of individual learning
# this is one for proposal
phi_v <-c(0.01, 0.1) #pick combinations of phi and lambda that illustrate the point
lambda_v <- c(0.3,0.6)
techmeans_bas <- c(6,12) # pay offs of behaviors 1 and 2 baseline
techmeans_exp <- c(6,0.1) # pay offs switched (experiment)

AR <- array(0 , dim=c( nrow=timesteps , 2 , length(phi_v), length(lambda_v)) ) 
AR[1,1,,] <- 6.8 # attraction score first behavior (pounding)
AR[1,2,,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use

dsim2 <- data.frame( timestep=0 , A1=0 , A2=0 , Pr1=0 , Pr2=0, phi =0, lambda =0)
therow <- 1

for (l in 1:length(lambda_v)) { 
  for (p in 1:length(phi_v)) {
    for (t in 1:timesteps){
      prtech_i <-  Softmax(lambda_v[l]*AR[t,,p,l]) #calculate probability of performing a behavior at this timestep
      obs_payoffs_t <- if (t > timesteps/2) {techmeans_exp} else {techmeans_bas} #initialize observed payoffs vector
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

## plot for proposal
col.pal2 <- c("#A1D99B", "#41AB5D")

# now it plots first phi with first lambda
plot(dsim2$Pr1~dsim2$timestep , col="white" , pch=19 , xlab="Timestep" , ylab="Probability of selecting tool use", ylim=c(0,1.3) ) 
abline(v = timesteps/2, lty = 2)

for (l in 1:length(lambda_v)) {
  
  points(dsim2$Pr2[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[l]]~dsim2$timestep[dsim2$lambda == lambda_v[l] & dsim2$phi == phi_v[l]],col=col.pal2[l], pch = 16)
  
  
}

title(main = "Individual Learning")
legend("topright", c("phi = 0.01, lambda = 0.3", "phi = 0.1,   lambda = 0.6"), pch = 19, col = col.pal2, horiz=FALSE, bg = "white")


##### Frequency-dependent learning #####

# we have two new parameters, gamma (weight of social information) and fc (strength of frequency dependent learning)
# gamma
gamma.sim <- logit(0.14)                   #weight given to social info par on log-odds scale, average received from Brendan's payoff bias paper (0.14)
gamma.sim_i <- rnorm( individuals , mean=0 , sd=0.8) #weight given to social info offsets per i
gamma.sim_id <- round( logistic(gamma.sim + gamma.sim_i), digits=2) 		##simulated gammas for all n individuals

# frequency dependence 
fc.sim <- log(2.2)				          #frequency dep par on log scale, should be around 2.2
fc.sim_i <- rnorm( individuals , mean=0 , sd=0.5)     #frequency dependent offsets per i
fc.sim_id <- round(exp(fc.sim + fc.sim_i), digits=2)  						##simulated strength of frequency dependent learning for all n individuals

## stochastic model with techmeans
techmeans_bas <- c(6,12) # pay offs of behaviors 1 and 2 baseline
techmeans_exp <- c(6,1) # pay offs switched (experiment)
lambda = 0.4 

#simulated data looping over individuals
dsim_s <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, s1 = 0, s2 =0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
therow <- 1

AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) )
AR[1,1,] <- 6.8 # attraction score first behavior (pounding)
AR[1,2,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use

S1 <- S2 <- rep(0, individuals+1) # number of individuals choosing each technology in previous timestep
s_temp <- rep(0,2)

# S1[1] <- 0.3 # starting number of individuals choosing pounding, seeding this only works if you take out the (it t >=1) condition in the loop
# S2[1] <- 0.7 # starting number of individuals choosing tool use

for (t in 1:timesteps){
  for (i in 1:individuals) {
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
    prtech_su <- c(S1[t], S2[t])
    
    # frequency dependent social learning
    if (t >= 1) { 
      if(sum(prtech_su) > 0) { 
        
        #compute frequency cue
        for( j in 1:2){  s_temp[j] <- prtech_su[j]^fc.sim_id[i]}
        
        prtech_s <- s_temp/sum(s_temp)
        prtech <- (1- gamma.sim_id[i])*prtech_i + gamma.sim_id[i]*prtech_s
        
      } else { 
          prtech <- prtech_i
      }
    } else {
      prtech <- prtech_i
    }
      #choose tech
    tech <- sample( 1:2 , size=1 , prob=prtech) # sample a behavior with prtech_i
    techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
    payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff
    
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AR[t+1,k,i] <- (1-phi_id[i])*AR[t,k,i] + phi_id[i]*obs_payoffs_t[k]
      }
    }
    dsim_s[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , S1[t], S2[t], AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2])
    therow <- therow + 1
  }
  #i
  S1[t+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$timestep==t] )
  S2[t+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$timestep==t] )
  
  }

o <- order( dsim_s$i ) #not sure if this is necessary but doesn't harm
dsim <- dsim_s[o,]

plot(s1/individuals ~ timestep, data=dsim[dsim$timestep>1,], col=col.pal[1] , ylim=c(0,1.1) , xlim=c(2,timesteps+1), pch=19 , xlab="Time" , ylab="Proportion of Individuals Choosing Option" )
points(s2/individuals ~ timestep, data=dsim[dsim$timestep>1,] , col=col.pal[2], pch=19)
legend("topleft", cex=0.85 , as.character(techmeans), pch=19 ,col=col.pal, horiz=TRUE , bty="n", title="Payoffs")
title(main = paste("Population Mean: lambda=",lambda ,", gamma=",round(logistic(gamma.sim), digits=2),", phi=",round(logistic(phi.sim),digits=2),", f=", round( exp(fc.sim), digits=2 ) ) , line = 0.5, outer = FALSE)

# individual plot
for(i in 1:individuals){
  plot(Pr1 ~ (timestep-1), data=dsim[dsim$individual==i & dsim$timestep>1,] , col=col.pal[1] , ylim=c(0,1.2) , pch=19 , xlab="Time" , ylab="Proportion of Individuals Choosing Option" , xlim=c(2,timesteps) )
  points(Pr2 ~ (timestep-1), data=dsim[dsim$individual==i & dsim$timestep>1,] , col=col.pal[2], pch=19)
  title(main = paste("id=",i ,", lambda=",lambda ,", gamma=",gamma.sim_id[i],", phi=",phi_id[i],", f=", round(fc.sim_id[i],digits=2 ) ) , line = -1.2, outer = FALSE)
  legend("top", inset=.05, cex=1 , as.character(techmeans), pch=19 ,col=col.pal, horiz=TRUE , bty="n")
} 
  
### Deterministic plot for proposal

# fc = 1 and gamma = 0.14
fc_v <- c(1, 5)
gamma_v <- c(0.14, 1)
phi <- 0.15
lambda = 0.4


# fc = 1 and gamma = 0.14
# set up data frame
dsim_s <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, s1 = 0, s2 =0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
therow <- 1

AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals) )
AR[1,1,] <- 6.8 # attraction score first behavior (pounding)
AR[1,2,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use

S1 <- S2 <- rep(0, individuals+1) # number of individuals choosing each technology in previous timestep
s_temp <- rep(0,2)

for (t in 1:timesteps){
  for (i in 1:individuals) {
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
    prtech_su <- c(S1[t], S2[t])
    
    # frequency dependent social learning
    if (t >= 1) { 
      if(sum(prtech_su) > 0) { 
        
        #compute frequency cue
        for( j in 1:2){  s_temp[j] <- prtech_su[j]^fc_v[1]}
        
        prtech_s <- s_temp/sum(s_temp)
        prtech <- (1- gamma_v[1])*prtech_i + gamma_v[1]*prtech_s
        
      } else { 
        prtech <- prtech_i
      }
    } else {
      prtech <- prtech_i
    }
    #choose tech
    tech <- sample( 1:2 , size=1 , prob=prtech) # sample a behavior with prtech_i
    techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
    payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff
    
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AR[t+1,k,i] <- (1-phi)*AR[t,k,i] + phi*obs_payoffs_t[k]
      }
    }
    dsim_s[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , S1[t], S2[t], AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2])
    therow <- therow + 1
  }
  #i
  S1[t+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$timestep==t] )
  S2[t+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$timestep==t] )
  
}

o <- order( dsim_s$i ) #not sure if this is necessary but doesn't harm
dsim <- dsim_s[o,]

# aggregate using dplyr
require(dplyr)

meanfreq <- dsim %>% 
  group_by(timestep) %>%
  summarize(average_tu = mean(Pr2))


# fc = 1 and gamma = 0.5
# set up data frame
dsim_s2 <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, s1 = 0, s2 =0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
therow <- 1

AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals) )
AR[1,1,] <- 6.8 # attraction score first behavior (pounding)
AR[1,2,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use

S1 <- S2 <- rep(0, individuals+1) # number of individuals choosing each technology in previous timestep
s_temp <- rep(0,2)

for (t in 1:timesteps){
  for (i in 1:individuals) {
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
    prtech_su <- c(S1[t], S2[t])
    
    # frequency dependent social learning
    if (t >= 1) { 
      if(sum(prtech_su) > 0) { 
        
        #compute frequency cue
        for( j in 1:2){  s_temp[j] <- prtech_su[j]^fc_v[1]}
        
        prtech_s <- s_temp/sum(s_temp)
        prtech <- (1- gamma_v[2])*prtech_i + gamma_v[2]*prtech_s
        
      } else { 
        prtech <- prtech_i
      }
    } else {
      prtech <- prtech_i
    }
    #choose tech
    tech <- sample( 1:2 , size=1 , prob=prtech) # sample a behavior with prtech_i
    techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
    payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff
    
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AR[t+1,k,i] <- (1-phi)*AR[t,k,i] + phi*obs_payoffs_t[k]
      }
    }
    dsim_s2[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , S1[t], S2[t], AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2])
    therow <- therow + 1
  }
  #i
  S1[t+1] <- length( dsim_s2$tech[dsim_s2$tech==1 & dsim_s2$timestep==t] )
  S2[t+1] <- length( dsim_s2$tech[dsim_s2$tech==2 & dsim_s2$timestep==t] )
  
}

o <- order( dsim_s2$i ) #not sure if this is necessary but doesn't harm
dsim2 <- dsim_s2[o,]

# aggregate using dplyr
meanfreq2 <- dsim2 %>% 
  group_by(timestep) %>%
  summarize(average_tu = mean(Pr2))


# fc = 2 and gamma = 0.14
# set up data frame
dsim_s <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, s1 = 0, s2 =0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
therow <- 1

AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals) )
AR[1,1,] <- 6.8 # attraction score first behavior (pounding)
AR[1,2,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use

S1 <- S2 <- rep(0, individuals+1) # number of individuals choosing each technology in previous timestep
s_temp <- rep(0,2)

for (t in 1:timesteps){
  for (i in 1:individuals) {
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
    prtech_su <- c(S1[t], S2[t])
    
    # frequency dependent social learning
    if (t >= 1) { 
      if(sum(prtech_su) > 0) { 
        
        #compute frequency cue
        for( j in 1:2){  s_temp[j] <- prtech_su[j]^fc_v[2]}
        
        prtech_s <- s_temp/sum(s_temp)
        prtech <- (1- gamma_v[1])*prtech_i + gamma_v[1]*prtech_s
        
      } else { 
        prtech <- prtech_i
      }
    } else {
      prtech <- prtech_i
    }
    #choose tech
    tech <- sample( 1:2 , size=1 , prob=prtech) # sample a behavior with prtech_i
    techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
    payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff
    
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AR[t+1,k,i] <- (1-phi)*AR[t,k,i] + phi*obs_payoffs_t[k]
      }
    }
    dsim_s[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , S1[t], S2[t], AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2])
    therow <- therow + 1
  }
  #i
  S1[t+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$timestep==t] )
  S2[t+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$timestep==t] )
  
}

o <- order( dsim_s$i ) #not sure if this is necessary but doesn't harm
dsim3 <- dsim_s[o,]

# aggregate using dplyr
require(dplyr)

meanfreq3 <- dsim3 %>% 
  group_by(timestep) %>%
  summarize(average_tu = mean(Pr2))


plot(s1/individuals ~ timestep, data=dsim3[dsim3$timestep>1,], col=col.pal[1] , ylim=c(0,1.1) , xlim=c(2,timesteps+1), pch=19 , xlab="Time" , ylab="Proportion of Individuals Choosing Option" )
points(s2/individuals ~ timestep, data=dsim3[dsim3$timestep>1,] , col=col.pal[2], pch=19)
legend("topleft", cex=0.85 , as.character(techmeans), pch=19 ,col=col.pal, horiz=TRUE , bty="n", title="Payoffs")
title(main = paste("Population Mean: lambda=",lambda ,", gamma=",round(logistic(gamma.sim), digits=2),", phi=",round(logistic(phi.sim),digits=2),", f=", round( exp(fc.sim), digits=2 ) ) , line = 0.5, outer = FALSE)


# fc = 2 and gamma = 0.5
# set up data frame
dsim_s4 <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, s1 = 0, s2 =0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
therow <- 1

AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals) )
AR[1,1,] <- 6.8 # attraction score first behavior (pounding)
AR[1,2,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use

S1 <- S2 <- rep(0, individuals+1) # number of individuals choosing each technology in previous timestep
s_temp <- rep(0,2)

for (t in 1:timesteps){
  for (i in 1:individuals) {
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
    prtech_su <- c(S1[t], S2[t])
    
    # frequency dependent social learning
    if (t >= 1) { 
      if(sum(prtech_su) > 0) { 
        
        #compute frequency cue
        for( j in 1:2){  s_temp[j] <- prtech_su[j]^fc_v[2]}
        
        prtech_s <- s_temp/sum(s_temp)
        prtech <- (1- gamma_v[2])*prtech_i + gamma_v[2]*prtech_s
        
      } else { 
        prtech <- prtech_i
      }
    } else {
      prtech <- prtech_i
    }
    #choose tech
    tech <- sample( 1:2 , size=1 , prob=prtech) # sample a behavior with prtech_i
    techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
    payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff
    
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AR[t+1,k,i] <- (1-phi)*AR[t,k,i] + phi*obs_payoffs_t[k]
      }
    }
    dsim_s4[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , S1[t], S2[t], AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2])
    therow <- therow + 1
  }
  #i
  S1[t+1] <- length( dsim_s4$tech[dsim_s4$tech==1 & dsim_s4$timestep==t] )
  S2[t+1] <- length( dsim_s4$tech[dsim_s4$tech==2 & dsim_s4$timestep==t] )
  
}

o <- order( dsim_s4$i ) #not sure if this is necessary but doesn't harm
dsim4 <- dsim_s4[o,]

# aggregate using dplyr
meanfreq4 <- dsim4 %>% 
  group_by(timestep) %>%
  summarize(average_tu = mean(Pr2))

# plot for proposal
# shapes for f values
point_f <- c(17,19)

plot(meanfreq$average_tu~meanfreq$timestep , col="white" , pch=19 , xlab="timestep" , ylab="prob choose high payoff behavior", ylim=c(0,1.3) ) 
abline(v = timesteps/2, lty = 2)
points(meanfreq$average_tu ~meanfreq$timestep,col=col.pal.phi[1], pch = point_f[1])
points(meanfreq2$average_tu ~meanfreq2$timestep,col=col.pal.phi[2], pch = point_f[1])
points(meanfreq3$average_tu ~meanfreq3$timestep,col=col.pal.phi[1], pch = point_f[2])
points(meanfreq4$average_tu ~meanfreq4$timestep,col=col.pal.phi[2], pch = point_f[2])


### USING LOOPS, DOESN'T WORK
#simulated data looping over individuals
dsim_s <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, s1 = 0, s2 =0, A1=0 , A2=0 , Pr1=0 , Pr2=0, gamma_v = 0, fc_v = 0)
therow <- 1

AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals , length(fc_v), length(gamma_v)) )
AR[1,1,,,] <- 6.8 # attraction score first behavior (pounding)
AR[1,2,,,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use

S1 <- S2 <- rep(0, individuals+1) # number of individuals choosing each technology in previous timestep
s_temp <- rep(0,2)

# S1[1] <- 0.3 # starting number of individuals choosing pounding, seeding this only works if you take out the (it t >=1) condition in the loop
# S2[1] <- 0.7 # starting number of individuals choosing tool use
for (f in 1:length(fc_v)) { 
  print(paste("running fc", f))
  for (g in 1:length(gamma_v)) { 
    for (t in 1:timesteps){
      for (i in 1:individuals) {
        prtech_i <-  Softmax(lambda*AR[t,,i,g,f]) #calculate probability of performing a behavior at this timestep
        prtech_su <- c(S1[t], S2[t])
        
        # frequency dependent social learning
        if (t >= 1) { 
          if(sum(prtech_su) > 0) { 
            
            #compute frequency cue
            for( j in 1:2){  s_temp[j] <- prtech_su[j]^fc_v[f]}
            
            prtech_s <- s_temp/sum(s_temp)
            prtech <- (1- gamma_v[g])*prtech_i + gamma_v[g]*prtech_s
            
          } else { 
            prtech <- prtech_i
          }
        } else {
          prtech <- prtech_i
        }
        #choose tech
        tech <- sample( 1:2 , size=1 , prob=prtech) # sample a behavior with prtech_i
        techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
        payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
        obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
        obs_payoffs_t[tech] <- payoff #populate with each observed payoff
        
        # update attractions for next timestep t + 1, don't do on final round
        if(t<timesteps){ 
          for (k in 1:2){
            AR[t+1,k,i,,] <- (1-phi)*AR[t,k,i,,] + phi*obs_payoffs_t[k]
          }
        }
        dsim_s[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , S1[t], S2[t], AR[t,1,i,g,f] , AR[t,2,i,g,f] ,  prtech_i[1] , prtech_i[2], gamma_v[g], fc_v[f])
        therow <- therow + 1
      }
      #i
      S1[t+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$timestep==t & dsim_s$gamma_v == gamma_v[g] & dsim_s$fc_v == fc_v[f]] )
      S2[t+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$timestep==t & dsim_s$gamma_v == gamma_v[g] & dsim_s$fc_v == fc_v[f]] )
      
    }
  } 
} 

o <- order( dsim_s$i ) #not sure if this is necessary but doesn't harm
dsim <- dsim_s[o,]

# aggregate to average per timestep
meanfreq <- aggregate(list(Pr2 = dsim$Pr2), by = list(timestep = dsim$timestep, gamma_v = dsim$gamma_v, fc_v = dsim$fc_v),  mean)
# aggregate using dplyr
require(dplyr)

meanfreq <- dsim %>% 
  group_by(timestep, gamma_v, fc_v) %>%
  summarize(average_tu = mean(Pr2))

# plot for proposal
# shapes for f values
point_f <- c(17,19)

plot(meanfreq$average_tu~meanfreq$timestep , col="white" , pch=19 , xlab="timestep" , ylab="prob choose high payoff behavior", ylim=c(0,1.3) ) 
abline(v = timesteps/2, lty = 2)
points(meanfreq$average_tu[meanfreq$fc_v == fc_v[1] & meanfreq$gamma_v == gamma_v[1]]~meanfreq$timestep[meanfreq$fc_v == fc_v[1] & meanfreq$gamma_v == gamma_v[1]],col=col.pal.phi[1], pch = point_f[1])
points(meanfreq$average_tu[meanfreq$fc_v == fc_v[1] & meanfreq$gamma_v == gamma_v[2]]~meanfreq$timestep[meanfreq$fc_v == fc_v[1] & meanfreq$gamma_v == gamma_v[2]],col=col.pal.phi[2], pch = point_f[1])
points(meanfreq$average_tu[meanfreq$fc_v == fc_v[1] & meanfreq$gamma_v == gamma_v[1]]~meanfreq$timestep[meanfreq$fc_v == fc_v[2] & meanfreq$gamma_v == gamma_v[1]],col=col.pal.phi[1], pch = point_f[2])
points(meanfreq$average_tu[meanfreq$fc_v == fc_v[1] & meanfreq$gamma_v == gamma_v[2]]~meanfreq$timestep[meanfreq$fc_v == fc_v[2] & meanfreq$gamma_v == gamma_v[2]],col=col.pal.phi[2], pch = point_f[2])

# using loop, didn't work
for (f in 1:length(fc_v)) {
  plot(meanfreq$Pr2~meanfreq$timestep , col="white" , pch=19 , xlab="timestep" , ylab="prob choose high payoff behavior", ylim=c(0,1.3) ) 
  abline(v = timesteps/2, lty = 2)
  
  for (g in 1:length(gamma_v)) {
    points(meanfreq$Pr2[meanfreq$fc_v == fc_v[f] & meanfreq$gamma_v == gamma_v[g]]~meanfreq$timestep[meanfreq$fc_v == fc_v[f] & meanfreq$gamma_v == gamma_v[g]],col=col.pal.phi[p],
           pch= point_f)
  }
  title(main = paste("lambda =", lambda_v[l]))
  legend("top", as.character(phi_v), pch = 19, col = col.pal.phi, title = "phi", horiz=TRUE, bg = "white")
  legend("topright", c("Tool use", "Pounding"), pch = c(19,17), col = "black")
}


# set f at 2 (no standard deviation) so population of all conformist individuals
# could also do four lines (f = 1 and f = 2)
# set phi at 0.15 for everyone
# gamma 0.14 and 0.5
# can have line types for f values and colors for gamma

# the above approach did not work very well, but I want to keep it as I can still learn a lot from it




#### GO HERE FOR WORKING CODE FOR PLOTS PROPOSAL #####
# code adapted by Brendan to run cleaner (with functions) and including tech vars for the techmeans bas and techmeans exp
library(RColorBrewer)

######create a softmax function to simply code
Softmax <- function(x){
  exp(x)/sum(exp(x))
}

logit <- function(p){
  (log(p/(1-p)))
}

logistic <- function(x){
  (1/(1+exp(-x)))
}

##got rid of individual variation for ease of rhetorical approach

# AIMS
#1. show IL can limit spread of adaptive behavior, this is highlighted when memory is important (i.e. phi is low)
# we will fix some value of lambda, as it is likely not important
#2. show positive frequency dependence and an over reliance of social learning  limits spread of adaptive behavior
#unbiased might be informative but not necessary. lets seem lambda fixed, gamma vary at 0,0.2, 0.5 , 1)
# we can ignore loops for now, and perhaps write a function to save space
#color palette
col.pal <- c("#1B9E77", "#D95F02")
# green (low payoff) and red (high payoff)
timesteps <- 100
individuals <- 200 # put at 1000 for smooth plots (but takes a while)

SimFreqDepEWA <- function(fc.sim , gamma.sim , phi.sim){
  individuals <- 200 # number of individuals 
  timesteps <- 100 # number of timesteps (t), in snail example, every interaction with a snail is a new timestep
  
  #setting varying phi per individual for stochastic model
  phi.sim_i <- rnorm(individuals, mean = 0, sd = 0) # can set standard deviation
  phi_id <- logistic(phi.sim +phi.sim_i)
  
  ##### Frequency-dependent learning #####
  # we have two new parameters, gamma (weight of social information) and fc (strength of frequency dependent learning)
  # gamma
  gamma.sim_i <- rnorm( individuals , mean=0 , sd=0) #weight given to social info offsets per i
  gamma.sim_id <- round( logistic(gamma.sim + gamma.sim_i), digits=2) 		##simulated gammas for all n individuals
  
  # frequency dependence 
  fc.sim_i <- rnorm( individuals , mean=0 , sd=0)     #frequency dependent offsets per i
  fc.sim_id <- round(exp(fc.sim + fc.sim_i), digits=2)  						##simulated strength of frequency dependent learning for all n individuals
  
  ## stochastic model with techmeans
  techmeans_bas <- c(6,12) # pay offs of behaviors 1 and 2 baseline
  techvar_bas <- c(2,3) # pay offs of behaviors 1 and 2 baseline
  
  techmeans_exp <- c(6,1) # pay offs switched (experiment)
  techvar_exp <- c(2,1) # pay offs of behaviors 1 and 2 baseline
  
  lambda = 1 
  
  #simulated data looping over individuals
  dsim_s <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, s1 = 0, s2 =0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
  therow <- 1
  
  AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) )
  AR[1,1,] <- 6.8 # attraction score first behavior (pounding)
  AR[1,2,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use
  
  S1 <- S2 <- rep(0, individuals+1) # number of individuals choosing each technology in previous timestep
  s_temp <- rep(0,2)
  
  for (t in 1:timesteps){
    for (i in 1:individuals) {
      prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
      prtech_su <- c(S1[t], S2[t])
      
      # frequency dependent social learning
      if (t >= 1) { 
        if(sum(prtech_su) > 0) { 
          
          #compute frequency cue
          for( j in 1:2){  s_temp[j] <- prtech_su[j]^fc.sim_id[i]}
          
          prtech_s <- s_temp/sum(s_temp)
          prtech <- (1- gamma.sim_id[i])*prtech_i + gamma.sim_id[i]*prtech_s
          
        } else { 
          prtech <- prtech_i
        }
      } else {
        prtech <- prtech_i
      }
      #choose tech
      tech <- sample( 1:2 , size=1 , prob=prtech) # sample a behavior with prtech_i
      techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
      techvar <- if(t > timesteps/2) {techvar_exp} else {techvar_bas}
      payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
      obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
      obs_payoffs_t[tech] <- payoff #populate with each observed payoff
      
      # update attractions for next timestep t + 1, don't do on final round
      if(t<timesteps){ 
        for (k in 1:2){
          AR[t+1,k,i] <- (1-phi_id[i])*AR[t,k,i] + phi_id[i]*obs_payoffs_t[k]
        }
      }
      dsim_s[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , S1[t], S2[t], AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2])
      therow <- therow + 1
    }
    #i
    S1[t+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$timestep==t] )
    S2[t+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$timestep==t] )
    
  }
  
  o <- order( dsim_s$i ) #not sure if this is necessary but doesn't harm
  dsim <-  dsim_s[o,] 
  return(dsim)
}

###lets do simulation, only for tool using populations, use function for brevity
# this stores function simulated dataframe, we can plot later if we call it different things

# Plot 1: individual learning
col_pal1 <- brewer.pal(4, "Paired")

d_IL_lowphi<- SimFreqDepEWA(fc.sim=log(0.4) , gamma.sim=logit(0) , phi.sim=logit(0.05) ) # no social info, just IL, low phi
d_IL_midphi<- SimFreqDepEWA(fc.sim=log(0.4) , gamma.sim=logit(0) , phi.sim=logit(0.2) ) # no social info, just IL, higher phi

plot(s2/individuals ~ timestep, data=d_IL_lowphi[d_IL_lowphi$timestep>1,], col=col_pal1[1] , ylim=c(0,1.1) , xlim=c(2,timesteps+1), pch=19 , xlab="Time" , ylab="Proportion of Individuals Usimg Tools" ) #low phi, tool behavior
points(s2/individuals ~ timestep, data=d_IL_midphi[d_IL_midphi$timestep>1,] , col=col_pal1[2], pch=19)
abline(v=50 , lty=2)
legend("topright", cex=0.85 , c("0.05" , "0.20"), pch=19 ,col=col_pal1, horiz=TRUE , bty="y", title="phi")
title(main = "Individual Learning" , line = 0.5, outer = FALSE)

###lets do  social learning case, just conformity
d_conf_SL_zerogamma <- SimFreqDepEWA(fc.sim=log(2) , gamma.sim=logit(0) , phi.sim=logit(0.15) ) # no social info, just IL
d_conf_SL_lowgamma <- SimFreqDepEWA(fc.sim=log(2) , gamma.sim=logit(0.25) , phi.sim=logit(0.15) ) # no social info, just IL
d_conf_SL_highgamma <- SimFreqDepEWA(fc.sim=log(2) , gamma.sim=logit(0.75) , phi.sim=logit(0.15) ) # no social info, just IL
d_conf_SL_onegamma <- SimFreqDepEWA(fc.sim=log(2) , gamma.sim=logit(1) , phi.sim=logit(0.15) ) # no social info, just IL

col_pal2 <- brewer.pal(4, 'BuGn')

plot(s2/individuals ~ timestep, data=d_conf_SL_zerogamma[d_conf_SL_zerogamma$timestep>1,], col=col_pal2[1] , ylim=c(0,1.1) , xlim=c(2,timesteps+1), pch=19 , xlab="Time" , ylab="Proportion of Individuals Usimg Tools" ) #low phi, tool behavior
points(s2/individuals ~ timestep, data=d_conf_SL_lowgamma[d_conf_SL_lowgamma$timestep>1,] , col=col_pal2[2], pch=19)
points(s2/individuals ~ timestep, data=d_conf_SL_highgamma[d_conf_SL_highgamma$timestep>1,] , col=col_pal2[3], pch=19)
points(s2/individuals ~ timestep, data=d_conf_SL_onegamma[d_conf_SL_onegamma$timestep>1,] , col=col_pal2[4], pch=19)
abline(v=50 , lty=2)

legend("topright", cex=0.85 , c("0" , "0.25" , "0.75" , "1"), pch=19 ,col=col_pal2, horiz=TRUE , bty="y", title="gamma")
title(main = "Frequency Dependent Learning I" , line = 0.5, outer = FALSE)

###lets vary fc, and keep gamma at 0.8

###lets do  social learning case, just conformity
d_SL_1 <- SimFreqDepEWA(fc.sim=log(2.5) , gamma.sim=logit(0.8) , phi.sim=logit(0.15) ) # no social info, just IL
d_SL_2 <- SimFreqDepEWA(fc.sim=log(1.5) , gamma.sim=logit(0.8) , phi.sim=logit(0.15) ) # no social info, just IL
d_SL_3 <- SimFreqDepEWA(fc.sim=log(1) , gamma.sim=logit(0.8) , phi.sim=logit(0.15) ) # no social info, just IL
d_SL_4 <- SimFreqDepEWA(fc.sim=log(0.5) , gamma.sim=logit(0.8) , phi.sim=logit(0.15) ) # no social info, just IL

col_pal2 <- brewer.pal(6, 'RdYlGn') #new color palatte

plot(s2/individuals ~ timestep, data=d_SL_1 [d_SL_1 $timestep>1,], col=col_pal2[1] , ylim=c(0,1.1) , xlim=c(2,timesteps+1), pch=19 , xlab="Time" , ylab="Proportion of Individuals Usimg Tools" ) #low phi, tool behavior
points(s2/individuals ~ timestep, data=d_SL_2[d_SL_2$timestep>1,] , col=col_pal2[2], pch=19)
points(s2/individuals ~ timestep, data=d_SL_3[d_SL_3$timestep>1,] , col=col_pal2[3], pch=19)
# points(s2/individuals ~ timestep, data=d_SL_4[d_SL_4$timestep>1,] , col=col_pal2[4], pch=19)
abline(v=50 , lty=2)
legend("topright", cex=0.85 , c("2.5" , "1.5" , "1" ), pch=19 ,col=col_pal2, horiz=TRUE , bty="y", title="Strength f")
title(main = "Frequency Dependent Learning II" , line = 0.5, outer = FALSE)

#### Payoff biased social learning #####
individuals <- 200
timesteps <- 100

# new parameter: beta.p, which is contribution of payoff bias cues
beta.p <- 1.1
beta.p_i <- rnorm(individuals, mean = 0 , sd = 0.2)
beta.p_id <- beta.p + beta.p_i

# also still need gamma

# techmeans (with variance now)
techmeans_bas <- c(6,12) # pay offs of behaviors 1 and 2 baseline
techvar_bas <- c(2,3) # pay offs of behaviors 1 and 2 baseline

techmeans_exp <- c(6,1) # pay offs switched (experiment)
techvar_exp <- c(2,1) # pay offs of behaviors 1 and 2 baseline # pay offs changed (experiment)
techvar <- c(1,1) # variance in pay offs of behavior across time steps
lambda = 1 
# need to fix f to 100?

#simulated data looping over individuals
dsim_s <- data.frame(individual = 0, timestep=0 , tech=0 , payoff_i1=0 , payoff_i2=0, s1 = 0, s2 =0, ps1 = 0, ps2 = 0, A1=0 , A2=0 , Pr1=0 , Pr2=0)
therow <- 1

AR <- array(0 , dim=c( nrow=timesteps , 2 , individuals ) )
AR[1,1,] <- 6.8 # attraction score first behavior (pounding)
AR[1,2,] <- 9 # attraction score second behavior (tool use) will get translated to probabilities of 0.1 pounding and 0.9 tool use

S1 <- S2 <- rep(0, individuals+1) # number of individuals choosing each technology in previous timestep
PS1 <- PS2 <- rep(0,nbouts+1) # empty vector for mean observed in previous rounds
s_temp <- rep(0,2)

# S1[1] <- 0.3 # starting number of individuals choosing pounding, seeding this only works if you take out the (it t >=1) condition in the loop
# S2[1] <- 0.7 # starting number of individuals choosing tool use

for (t in 1:timesteps){
  for (i in 1:individuals) {
    prtech_i <-  Softmax(lambda*AR[t,,i]) #calculate probability of performing a behavior at this timestep
    prtech_sp <- c(PS1[t],PS2[t])
    prtech_su <- c(S1[t], S2[t])
    
    # frequency dependent social learning
    if (t >= 1) { 
      if(sum(prtech_su) > 0) { 
        
        #compute frequency cue
        for( j in 1:2){  s_temp[j] <- prtech_su[j]^fc.sim_id[i]}
        
        prtech_s <- s_temp/sum(s_temp)
        prtech <- (1- gamma.sim_id[i])*prtech_i + gamma.sim_id[i]*prtech_s
        
      } else { 
        prtech <- prtech_i
      }
    } else {
      prtech <- prtech_i
    }
    #choose tech
    tech <- sample( 1:2 , size=1 , prob=prtech) # sample a behavior with prtech_i
    techmeans <- if(t > timesteps/2) {techmeans_exp} else {techmeans_bas}
    payoff <- rnorm( 1 , mean=techmeans[tech] , sd=techvar[tech] ) #draw a behavior of tech=k with specified mean and SD, realized choice
    obs_payoffs_t <- rep(0,2) #initialize observed payoffs vector
    obs_payoffs_t[tech] <- payoff #populate with each observed payoff
    
    # update attractions for next timestep t + 1, don't do on final round
    if(t<timesteps){ 
      for (k in 1:2){
        AR[t+1,k,i] <- (1-phi_id[i])*AR[t,k,i] + phi_id[i]*obs_payoffs_t[k]
      }
    }
    dsim_s[therow,] <- c(i,  t , tech , obs_payoffs_t[1] , obs_payoffs_t[2] , S1[t], S2[t], AR[t,1,i] , AR[t,2,i] ,  prtech_i[1] , prtech_i[2])
    therow <- therow + 1
  }
  #i
  S1[t+1] <- length( dsim_s$tech[dsim_s$tech==1 & dsim_s$timestep==t] )
  S2[t+1] <- length( dsim_s$tech[dsim_s$tech==2 & dsim_s$timestep==t] )
  
}

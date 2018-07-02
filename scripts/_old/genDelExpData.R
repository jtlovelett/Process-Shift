library(tidyverse)
# generate some fake data with known parameters, see if we can recover them with below
participants = 1:32
items = 1:10
trials = 1:50

fakeData = expand.grid(participant=participants, item=items, trial=trials) %>%
  arrange(participant, item, trial)

# I guess maybe start with just fitting global parameters?
# maybe offsets per participant and item but no interaction? 

# take some parameters from caption to figure 1:
# M1:
# α = 1, β = 2, r = 0.2, t0 = 0.3, and σ = 0.1
# global params
a = 1
b = 2
r = .2
t0 = .3
sigma = .1

# randomly assign some offsets centered on means

# assemble components into fakeData column for predicted log(RT)

genDelayedExp = function(a,b,n,r,t,N,sigma){
  tau = exp(r*t)
  a + b* ((tau+1)/(tau+exp(r*N))) + rnorm(0, sigma)
}


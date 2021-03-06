library(tidyverse)

# load generating functions
source('generator_sourceCode.R')

#### Parameters ####

# data level
nsubs <- 5
nitems <- 6
ntrials <- 50

# model level
f <- 'ps' # which function to use as a generator? (de or ps)
sigma <- .2 # overall sigma: sd(y-y_hat)

# Model parameters
# ::: Note ::: 
#' Most of these have defaults set within the function definition
#' However, the wisdom of those particular starting values is suspect

#### P(retreival) ~ trial
pRet_A = .25; pRet_A_sigma = .05
pRet_B = ntrials/4; pRet_B_sigma = 3 # dunno, weird initializations, sorry.

#### DE EQUATION:  RT = A + B*exp(-r*N) + error ~ N(0,sigma) 
## See Heathcote p. 15 for priors
A_de <- 1 ; A_de_sigma <- .05 # overall mean; sd of A (alpha) components A_ ~ N(A, A_sigma)
B_de <- 2; B_de_sigma <- .3 # beta |  B_ ~ N(B, B_sigma)
r_de <- .2; r_de_sigma <- .025 # rate | r ~ N(r, r_sigma)
tau_de <- 3; tau_de_sigma <- 3 # delay parameter ; this behaves weirdly with truncation -- lots of values == trunc_min... ok?

#### PS EQUATION:  RT = A + B*N^(-r) + error ~ N(0,sigma) or mu + error
mu_ps = 2 ; mu_ps_sigma = .5 # setting the same as beta, which i think makes sense. 
A_ps <- 1 ; A_ps_sigma <- 1 # overall mean; sd of A (alpha) components A_ ~ N(A, A_sigma)
B_ps <- 2; B_ps_sigma <- 2 # beta |  B_ ~ N(B, B_sigma)
r_ps <- .7; r_ps_sigma <- .5 # rate | r ~ N(r, r_sigma)

# model parameter means (taken from Evans et al. 2018 fig 1)
# Figure 1. Fits to two simulated data sets (black circles), generated by the
# exponential function (left panel; parameters: alpha = 1, beta = 2, r = 0.2, t0** = 0.3, and sigma = 0.1)
# and the power function (right panel; parameters: alpha = 1, beta = 2, r = 0.7, t0** = 0.3, and sigma = 0.1).
# **t0 <- .3 # i.e. fastest possible response-contigent RT, in seconds. Ignore this for now.



if(f == 'de'){
  RTs <- generate_RTs(f = 'de',
                         nsubs=nsubs, nitems=nitems, ntrials=ntrials,
                         alpha=A_de, beta=B_de, rate=r_de, sigma=sigma,
                         a_sig=A_de_sigma, b_sig=B_de_sigma, r_sig=r_de_sigma,
                         tau=tau_de, tau_sig=tau_de_sigma,
                         pRet_A = pRet_A, pRet_B = pRet_B,
                         pRet_A_sig = pRet_A_sigma, pRet_B_sig = pRet_B_sigma)
}else if(f == 'ps'){

  RTs <- generate_RTs(f = 'ps',
                         nsubs=nsubs, nitems=nitems, ntrials=ntrials,
                         alpha=A_ps, beta=B_ps, rate=r_ps, sigma=sigma,
                         a_sig=A_ps_sigma, b_sig=B_ps_sigma, r_sig=r_ps_sigma,
                         mu = mu_ps, mu_sig = mu_ps_sigma,
                         pRet_A = pRet_A, pRet_B = pRet_B,
                         pRet_A_sig = pRet_A_sigma, pRet_B_sig = pRet_B_sigma)
} else{
  RTs <- -1 # again, handle this better...
}

drawPlot(RTs, f)

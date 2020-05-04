library(tidyverse)

# generate the full subject*item effect matrix for a single parameter
gen_param <- function(param_name, param_mu, # just for reporting back, not used computationally
                      nsubs, nitems,
                      showWork=F, # do we want to output the matrices leading to the final parameters?
                      sd = param_mu*.1, # sd of random effects; sub*item's sd is /2 (good idea?)
                      mu_bias=0) # should ranefs be biased in either direction (weird)?
{
  sub_offsets <- rnorm(nsubs,mu_bias,sd)
  item_offsets <- rnorm(nitems,mu_bias,sd)
  out <- list(param_mu, # parameter mean
              sub_offsets,
              matrix(rep(sub_offsets, times = nitems),nrow = nsubs), # subject offsets as sub*item matrix
              item_offsets,
              matrix(rep(item_offsets, each = nsubs),nrow = nsubs), # item offsets as sub*item matrix
              matrix(rnorm(nsubs*nitems,mu_bias,sd/2),nrow = nsubs) # subj*item offsets
              
  )
  names(out) <- c(param_name, #1
                  paste0(param_name,'_sub'), #2
                  paste0(param_name,'_sub_mat'), #3
                  paste0(param_name,'_item'), #4
                  paste0(param_name,'_item_mat'),#5
                  paste0(param_name,'_subItem'))#6
  out[[paste0(param_name,'_PAREMETER')]] <- out[[1]] + out[[3]] + out[[5]] + out[[6]] #7
  if(showWork){
    out
  } else{
    out[[7]]
  }
}

# take a matrix of predicted RTs by subject and item for a given trial, convert it to a df
RTmat2DF <- function(RT_mat, choice_mat, trial){
  subs <- 1:nrow(RT_mat)
  items <- 1:ncol(RT_mat)
  as_tibble(expand.grid(item = items, sub = subs)) %>%
    mutate(trial = trial)%>%
    rowwise() %>%
    mutate(isRetrieval = as.logical(choice_mat[sub,item]),
           RT_hat = RT_mat[sub,item]) %>%
    select(sub, item, trial, isRetrieval, RT_hat)
}


# actually generate the data
generate_RTs <- function(f, #'de' or 'ps'
                         nsubs, nitems, ntrials,
                         alpha, beta, rate, sigma, # overall sigma
                         a_sig=alpha*.1, b_sig=beta*.1, r_sig=rate*.1,
                         tau = 0, tau_sig = 0, # delay arguments; default to no delay
                         mu = beta, mu_sig = b_sig, # ps_algorithm arguments
                         pRet_A = .25, pRet_B = ntrials/4,# tricky to set default to .25 of the way...
                         pRet_A_sig =.05, pRet_B_sig = 3, # again, weird defaults...
                         trunc_min = 10e-6){ # min values for should-be positive values
  params <- list(
    alpha = gen_param('alpha',alpha, nsubs, nitems, sd = a_sig, showWork=F), # changing showWork changes structure of output (bad)
    beta = gen_param('beta', beta, nsubs, nitems, sd = b_sig, showWork=F),
    rate = gen_param('rate', rate, nsubs, nitems, sd = r_sig, showWork=F),
    pRet_A = gen_param('pRet_A', pRet_A, nsubs, nitems, sd = pRet_A_sig, showWork=F),
    pRet_B = gen_param('pRetB', pRet_B, nsubs, nitems, sd = pRet_B_sig, showWork=F))
  if(f == 'ps'){
    params$mu <- gen_param('mu', mu, nsubs, nitems, sd = mu_sig, showWork=F)
  } else if(f == 'de'){
    params$tau <- gen_param('tau', tau, nsubs, nitems, sd = tau_sig, showWork=F)
  }
  
  trials <- 1:ntrials
  
  # get p(retrieval on each trial)
  pRet_by_trial <- lapply(trials, function(N){
    # logistic function modeling p(retrieval)
    1/(1 + exp(-pmax_mat(params$pRet_A, trunc_min)*(N-pmax_mat(params$pRet_B,trunc_min))))})

  # sample whether or not actually retrieval
  isRet_by_trial <- lapply(pRet_by_trial, function(m){
    apply(m, c(1,2), function(p) rbinom(1,1,p))})
  
  # per trial, get 2 matrices, 1 per component of PS function, that give the number of 
  # trials, up to and including the current one, for which the alg or ret strategy was used
  # first, initialize a ntrials long list of pairs of nsubs*nitems matrices, to strore the counts
  # (since we're breaking with R dogma and writing a for loop, this helps speed things up)
  initStratUseMats <- list(
    nRetTrials = isRet_by_trial[[1]], # start with the counts for the first trial
    nAlgTrials = 1-isRet_by_trial[[1]]
  )
  cumulStratUse_by_trial <- rep(list(initStratUseMats),times=ntrials)
  for(t in 2:ntrials){
    # increment the count of retrieval trials by the isRet matrix
    # since it represents, for each subject*item, whether retrieval was used for this trial
    cumulStratUse_by_trial[[t]]$nRetTrials <- 
      cumulStratUse_by_trial[[t-1]]$nRetTrials + isRet_by_trial[[t]]
    # increment the count of algorithm trials by 1 - isRet
    cumulStratUse_by_trial[[t]]$nAlgTrials <- 
      cumulStratUse_by_trial[[t-1]]$nAlgTrials + (1-isRet_by_trial[[t]]) # swap 0s and 1s to get isAlg
  }

  ## create a list, indexed by trial, of subject * item matrices of predicted RTs (pre-noise)
  if (f == 'de'){ # DELAYED EXPONENTIAL
    RT_hat_by_trial <- lapply(trials, function(N){
      # EQUATION:  RT = A + B*exp(-r*N) + error~ N(0,sigma)
      # Delayed EQUATION:  RT = A + B* ((T+1)/(T+exp(r*N))) + error~ N(0,sigma)
      A <- pmax_mat(params$alpha,trunc_min)# truncate the params at (close to) 0 
      B <- pmax_mat(params$beta,trunc_min)
      t <- pmax_mat(params$tau,trunc_min) 
      R <- pmax_mat(params$rate, trunc_min)
      
      A + B * ((t+1)/(t+exp(R*N)))}) # Delayed exponential RT equation
    
  } else if (f == 'ps'){ # PROCESS SHIFT
    # compute retrieval component for all trials
    RT_hat_RET_by_trial <- lapply(trials, function(N){
      # In here figure out how to deal with RT_ret ~ ntrials(OF THAT STRATEGY)
      # EQUATION:  RT = A + B*N^(-r) + error~ N(0,sigma)
      A <- pmax_mat(params$alpha, trunc_min) # truncate the params at (close to) 0
      B <- pmax_mat(params$beta, trunc_min)
      R <- pmax_mat(params$rate, trunc_min)
      N <- pmax_mat(cumulStratUse_by_trial[[N]]$nRetTrials, trunc_min) 
      # ^ the above pmax is to avoid N=0 which causes NaN output. 
      # ^ we should never be saving values where N=0, since if theres been no retrieval
      # ^ trials, then this trial is not retrieval, and so whatever RT_hat_RET we compute
      # ^ is multiplied by 0, which kills it (as long as its finite, so no raising 0 to powers)
      # (so pmaxing here should have no effect except to make the whole thing computable)
      A + B * N^(-R)}) # process shift retrieval component power function


    RT_hat_ALG_by_trial <- lapply(trials, function(N){
      N <- cumulStratUse_by_trial[[N]]$nAlgTrials # not used now, but will be if we eventually want to model two component power laws instead of a constant and a power law
      params$mu}) # process shift algorithm component constant function

    RT_hat_by_trial <- lapply(trials, function(N){
      # compute the weighted sum of the two strategies' RTs (0,1 weights)
      #i.e. choose between them on the basis of samples from P(retrieval) ~ Trial
      isRet_by_trial[[N]]*RT_hat_RET_by_trial[[N]] +
        (1-isRet_by_trial[[N]])*RT_hat_ALG_by_trial[[N]]
    })
    
  } else {
    RT_hat_by_trial <- NULL # throw a better error here
  }
  # convert all those matrices to data frames and staple them together
  bind_rows(lapply(1:length(RT_hat_by_trial), function(i) {
    RTmat2DF(RT_hat_by_trial[[i]], isRet_by_trial[[i]], i)})) %>%
    # add some noise (based on sigma, set globally above)
    mutate(error = rnorm(n(),0,sigma),
           RT = RT_hat + error,
           fn = f)
}

# take the generated data and plot it
drawPlot <- function(rt, fn){
  if (anyNA(rt)){
    stop('invalid function specified. Set f = \'de\' or \'ps\'\n
         Alternatively, NAs were introduced in generation somehow.')
  }
  rt %>% 
    ggplot(aes(x = trial, y = RT, color = isRetrieval)) + 
    geom_point() + 
    facet_grid(sub~item) + 
    scale_y_continuous('RT (sec)')+
    ggtitle(paste0('Generated RTs for the ', fn, ' function\nby subject (rows) and item (cols)'))+
    theme_bw() +
    theme(
      legend.position = 'bottom'
    )
}

# return positional/parallel max between a matrix and scalar x
pmax_mat <- function(mat, x){
  mat[mat<x] = x
  mat
}

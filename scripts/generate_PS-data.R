library(dplyr)
library(ggplot2)
#### Parameters ####
#  general
trials <- 1:100
# algorithm
alg_mu <- 1000
alg_sd <- 50
# retrieval
ret_mu <- function(trial, Alpha, Beta, rate){
  Alpha + Beta*trial^(-rate)
}
#ret_sd <- alg_sd
## p_retrieval
logistic <- function(x, A, B){
  1/(1 + exp(-A*(x-B)))
}

# generator
gen_ProcShift_RTs <- function(trials,
                              alg_mu, alg_sd, 
                              ret_Alpha = 0, ret_Beta = alg_mu, ret_rate = 1, ret_sd = alg_sd,
                              p_ret_A = .125, p_ret_B = floor(max(trials/4)),
                              plot = FALSE){
  
  p_ret <- logistic(trials, p_ret_A, p_ret_B)
  PS_dat <- tibble(trial = trials,
                       p_ret = p_ret,
                       strategy = ifelse(rbinom(length(trials),1,p_ret),
                                         'ret',
                                         'alg')) %>%
    group_by(strategy) %>%
    mutate(trial_of_strategy = row_number()) %>%
    ungroup() %>%
  ### IS THIS WHAT'S MISSING ELSEWHERE? (i.e. trial should be counted within each strategy not accross)
    rowwise() %>%
    mutate(rt = case_when(strategy == 'alg' ~ rnorm(1,alg_mu, alg_sd),
                          strategy == 'ret' ~ rnorm(1,
                                                    ret_mu(trial_of_strategy, ret_Alpha, ret_Beta, ret_rate),
                                                    ret_sd)))
  if(plot){
    ggplot(PS_dat, aes(x = trial, y = rt, color = strategy))+
      geom_point()+
      theme_linedraw()
  } else {
    PS_dat %>%
      transmute(su = 1,
                b1 = trial,
                item = 1,
                strat = ifelse(strategy == 'alg', 1, 0))
  }
}


#gen_ProcShift_RTs(trials, alg_mu, alg_sd, ret_rate=.2, p_ret_A =.1, ret_Alpha = 250, plot =T)


## ----setup---------------------------------------------------------------
library(tidyverse)
library(forcats)
knitr::opts_chunk$set(echo = F, message = F, warning = F)
rm(list=ls())

path='~/Documents/UCSD/Research/Process Shift/'
dat = read.csv(paste0(path,'data/proc_shif_data.csv')) %>%
  transmute(subject = su,
            trial = bl,
            first.correct.trial.item = first,
            trials.since.1st.correct = bl_r,
            reversions = ifelse(itype == 1, F,T),
            item,
            strategy = ifelse(strat==1, 'algorithm','retrieval'),
            is.alg = ifelse(strategy == 'algorithm', 1, 0),
            is.ret = ifelse(strategy == 'retrieval', 1 , 0),
            RT)

library(rstan)
stan.data = list(y = log(dat$RT), 
                 strategy = as.integer(dat$strategy == 'retrieval')+1,
                 isAlg = dat$is.alg,
                 isRet = dat$is.ret,
                 item = as.numeric(as.factor(dat$item)),
                 subject = as.numeric(as.factor(dat$subject)),
                 trial = dat$trial,
                 N = nrow(dat),
                 nc = length(unique(dat$strategy)),
                 ni = length(unique(dat$item)),
                 ns = length(unique(dat$subject)),
                 nt = length(unique(dat$trial)),
                 # Find a reasonable location for beta:
                 # 85th %ile of subject*item first correct retrieval trial (ok?)
                 betaBase = dat %>% 
                   group_by(subject, item) %>%
                   summarize(first.correct = mean(first.correct.trial.item)) %>%
                   ungroup() %>%
                   summarize(beta.loc = quantile(first.correct,.85)) %>%
                   .$beta.loc
)
# use overall RT mean, sigma to set priors for algorithm trials
meanRT = mean(log(dat$RT)) # = 7.6
sdRT = sd(log(dat$RT)) # = .9

# set beta priors based on mean, sd for retrieval trials 
meanRT.beta = mean(log(dat$RT[dat$strategy == 'retrieval'])) # = 7.3
sdRT.beta = sd(log(dat$RT[dat$strategy == 'retrieval'])) # = 7

## ----stan - process shift------------------------------------------------
fit <- stan(
  file = paste0(path, '/scripts/procShiftModel.stan'),
  #model_code = mod,  # Stan program
  data = stan.data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 3000,            # total number of iterations per chain
  cores = 4,              # number of cores (using 2 just for the vignette)
  refresh = 250          # show progress every 'refresh' iterations
)

samples <- extract(fit) %>% as.data.frame()


## ----stan - delayed exponential------------------------------------------
fit.delExp <- stan(
  file = paste0(path, '/scripts/delayedExp.stan'),
  #model_code = mod,  # Stan program
  data = stan.data,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 20,          # number of warmup iterations per chain
  iter = 1000,            # total number of iterations per chain
  cores = 4,              # number of cores
  refresh = 25          # show progress every 'refresh' iterations
)

samples.delExp <- extract(fit.delExp) %>% as.data.frame()
 c(mean(log10(dat$RT)), sd(log10(dat$RT)))
 samples %>% summarise_all(mean)
# samples %>% ggplot(aes(x=1:nrow(samples), y=mu))+geom_point()

# diagnostics:
# - trace plot
# - shape
# - 

## ----diagnostics---------------------------------------------------------
params = get_sampler_params(fit) # apparently this is useful for something...

# simulate some data
participants = 1:40

# sim.dat = 



## ----predict-------------------------------------------------------------
#coefs = summary(fit)$summary[,1] # pull out just the mean of each coef
coefs = summary(fit.delExp)$summary[,1] # pull out just the mean of each coef


# is there a built-in way of doing this in stan...? would be useful... 
pred.dat = dat %>%
  mutate_at(c('subject','item'), function(x) as.numeric(as.factor(x))) %>%
  mutate(strategy = as.integer(strategy == 'retrieval')+1,
         pred.logRT = NA)

for(row in 1:nrow(pred.dat)){ 

  # Mu = coefs['Alg_M'] +
  #   coefs[paste0('Alg_M_es[',pred.dat[row,'subject'],']')] +
  #   coefs[paste0('Alg_M_ei[',pred.dat[row,'item'],']')] +
  #   coefs[paste0('Alg_M_esi[',
  #                  pred.dat[row,'subject'],',',
  #                  pred.dat[row,'item'],']')]
  # 
  # Beta = coefs['Ret_B'] +
  #   coefs[paste0('Ret_B_es[',pred.dat[row,'subject'],']')] +
  #   coefs[paste0('Ret_B_ei[',pred.dat[row,'item'],']')] +
  #   coefs[paste0('Ret_B_esi[',
  #                  pred.dat[row,'subject'],',',
  #                  pred.dat[row,'item'],']')]
  # 
  # Tau = coefs['Ret_T'] +
  #   coefs[paste0('Ret_T_es[',pred.dat[row,'subject'],']')] +
  #   coefs[paste0('Ret_T_ei[',pred.dat[row,'item'],']')] +
  #   coefs[paste0('Ret_T_esi[',
  #                  pred.dat[row,'subject'],',',
  #                  pred.dat[row,'item'],']')]
  # 
  # pred.dat[row,'pred.logRT.assembled'] =
  #   pred.dat[row,'is.alg']*Mu +
  #   pred.dat[row,'is.ret'] * (Beta + Tau*log(17) - exp(Tau)*log(pred.dat[row,'trial']))
  # 
  # pred.dat[row, 'Mu'] = Mu 
  # pred.dat[row, 'Beta'] = Beta
  # pred.dat[row, 'Tau'] = Tau
  # pred.dat[row, 'log.Trial'] = log(pred.dat[row, 'trial'])
  
  pred.dat[row, 'pred.logRT'] = coefs[paste0('y_hat[',row,']')]
  
  pred.dat[row,'pred.RT'] = exp(pred.dat[row,'pred.logRT'])
}

# pred.dat = pred.dat %>% mutate(comp.pred.RT = exp(Beta + exp(Tau)*log(17) -exp(Tau)*log(trial)))

# AGGREGATE PLOTS OF PRED (RED) VS. ACTUAL (BLACK)
 # pred.dat %>% 
 #   filter(isRet == T)
 #   group_by(trial, item) %>% 
 #   summarize(mPredRT = mean(comp.pred.RT), mRT = mean(RT)) %>% 
 #   ggplot(aes(x = trial))+ 
 #   geom_point(aes(y = mRT), color = 'black') + 
 #   geom_point(aes(y = mPredRT), color = 'red')+ 
 #   facet_wrap(~item)

## ----individual data-----------------------------------------------------

sub_plot = pred.dat %>%
  mutate(strategy = as.factor(strategy)) %>%
  ggplot(aes(x = trial, color = strategy, group=strategy))+
  geom_point(aes(y=RT))+
  geom_line(aes(y=pred.RT), color = 'black')+
  #geom_vline(aes(xintercept=first.correct.trial.item))+
  facet_grid(subject~item)

# sub_plot %>% ggsave(filename='subj.plot.predictions_2.pdf',path=paste0(path,'plots/'), width = 25, height = 40, device= 'pdf')

sub_plot %>% ggsave(filename='subj.plot.preds_delExp.pdf',path=paste0(path,'plots/'), width = 25, height = 40, device= 'pdf')



library(tidyverse)
library(forcats)
library(rstan)
knitr::opts_chunk$set(echo = F, message = F, warning = F)
rm(list=ls())

# generate some fake data with known parameters, see if we can recover them with below

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

fit <- stan(
  file = paste0(path, '/scripts/delayedExp.stan'),
  #model_code = mod,  # Stan program
  data = stan.data,    # named list of data
  chains = 2,             # number of Markov chains
  warmup = 500,          # number of warmup iterations per chain
  iter = 2000,            # total number of iterations per chain
  cores = 2,              # number of cores
  refresh = 100          # show progress every 'refresh' iterations
)

samples <- extract(fit) %>% as.data.frame()

params = get_sampler_params(fit) # apparently this is useful for something...

coefs = summary(fit)$summary[,1] # pull out just the mean of each coef

# is there a built-in way of doing this in stan...? would be useful... 
pred.dat = dat %>%
  mutate_at(c('subject','item'), function(x) as.numeric(as.factor(x))) %>%
  mutate(strategy = as.integer(strategy == 'retrieval')+1,
         pred.logRT = NA)

for(row in 1:nrow(pred.dat)){ 
  pred.dat[row, 'pred.logRT'] = coefs[paste0('y_hat[',row,']')]
  pred.dat[row,'pred.RT'] = exp(pred.dat[row,'pred.logRT'])
}

sub_item_plot = pred.dat %>%
  mutate(strategy = as.factor(strategy)) %>%
  ggplot(aes(x = trial))+
  geom_point(aes(y=log(RT), color = strategy))+
  geom_line(aes(y=pred.logRT), color = 'black')+
  #geom_vline(aes(xintercept=first.correct.trial.item))+
  facet_grid(subject~item)

sub_item_plot_rawRT = pred.dat %>%
  mutate(strategy = as.factor(strategy)) %>%
  ggplot(aes(x = trial))+
  geom_point(aes(y=RT, color = strategy))+
  geom_line(aes(y=pred.RT), color = 'black')+
  #geom_vline(aes(xintercept=first.correct.trial.item))+
  facet_grid(subject~item)

t = pred.dat %>%
  filter(subject ==1, item==8)

sub_item_plot %>% ggsave(filename='subj.plot.preds_delExp.pdf',path=paste0(path,'plots/'), width = 25, height = 40, device= 'pdf')

sub_item_plot_rawRT %>% ggsave(filename='subj.plot.preds_rawRT_delExp.pdf',path=paste0(path,'plots/'), width = 25, height = 40, device= 'pdf')

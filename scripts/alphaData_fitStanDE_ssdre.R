library(dplyr)
library(forcats)
library(ggplot2)
library(rstan)
library(tidyr)
library(readr)
knitr::opts_chunk$set(echo = F, message = F, warning = F)
#rm(list=ls())

dat = read_csv('../data/alphaData_raw.csv') %>%
  filter(acc == 1) %>%
  select(subject = su,
         trial = bl,
         item = item,
         strategy = strat,
         RT = rt) %>%
  mutate(strategy = fct_recode(as.factor(strategy),
                               unprobed = '0',
                               algorithm = '1',
                               retrieval = '2',
                               forgot = '3'),
         is.alg = ifelse(strategy == 'algorithm', 1, 0),
         is.ret = ifelse(strategy == 'retrieval', 1 , 0),
         is.unknown = ifelse(strategy == 'unprobed', 1 , 0)) %>%
  filter(strategy != 'forgot') %>%
  mutate(subject = as.numeric(as.factor(subject)),
         item = as.numeric(as.factor(item)))

source('./mk_sparse_sub-item_matrix.R')
si.lookup = siMat(dat)

stan.data = list(y = log(dat$RT), 
                 strategy = as.integer(dat$strategy),# == 'retrieval')+1,
                 isAlg = dat$is.alg,
                 isRet = dat$is.ret,
                 isUnpr = dat$is.unknown,
                 item = as.numeric(as.factor(dat$item)),
                 subject = as.numeric(as.factor(dat$subject)),
                 trial = dat$trial,
                 N = nrow(dat),
                 nc = length(unique(dat$strategy)),
                 ni = length(unique(dat$item)),
                 ns = length(unique(dat$subject)),
                 nsi = nrow(dat %>% count(subject, item)),
                 nt = length(unique(dat$trial)),
                 # Find a reasonable location for beta:
                 # 85th %ile of subject*item first correct retrieval trial (ok?)
                 # betaBase = dat %>% 
                 #   group_by(subject, item) %>%
                 #   summarize(first.correct = mean(first.correct.trial.item)) %>%
                 #   ungroup() %>%
                 #   summarize(beta.loc = quantile(first.correct,.85)) %>%
                 #   .$beta.loc,
                 betaBase = 17, # I think that's the result of the above...
                 si_lookup = si.lookup
)
# For execution on a local, multicore CPU with excess RAM we recommend calling
options(mc.cores = parallel::detectCores())
# To avoid recompilation of unchanged Stan programs, we recommend calling
rstan_options(auto_write = TRUE)


fit.de <- stan(
  file = './delayedExp.stan', # can use same model with this data -- no need for "alpha_"
  #model_code = mod,  # Stan program
  data = stan.data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1500,          # number of warmup iterations per chain
  iter = 3500,            # total number of iterations per chain
  cores = 4,              # number of cores
  refresh = 250,          # show progress every 'refresh' iterations
  control = list(adapt_delta = 0.9,  max_treedepth = 12)

)

#samples <- extract(fit) %>% as.data.frame()

#params = get_sampler_params(fit) # apparently this is useful for something...

#coefs.ps = summary(fit.ps)$summary[,1] # pull out just the mean of each coef
coefs.de = summary(fit.de)$summary[,1] # pull out just the mean of each coef


pred.dat.de = dat %>%
  mutate_at(c('subject','item'), function(x) as.numeric(as.factor(x))) %>%
  mutate(strategy = as.integer(strategy == 'retrieval')+1,
         logRT = log(RT),
         #pred.logRT.ps = NA,
         pred.logRT.de = NA)

for(row in 1:nrow(pred.dat.de)){ 
  pred.dat.de[row, 'pred.logRT.de'] = coefs.de[paste0('y_hat[',row,']')]
  pred.dat.de[row,'pred.RT.de'] = exp(pred.dat.de[row,'pred.logRT.de'])
}

# sub.item.plot = pred.dat.de %>%
#   mutate(strategy = as.factor(strategy)) %>%
#   ggplot(aes(x = trial, color = strategy, group=strategy))+
#   geom_point(aes(y=logRT))+
#   geom_line(aes(y=pred.logRT.delExp), color = 'green')+
#   geom_line(aes(y=pred.logRT.ps), color = 'blue')+
#   #geom_vline(aes(xintercept=first.correct.trial.item))+
#   facet_grid(subject~item)


# sub.item.plot %>% ggsave(filename='mostRecentSubj.plot.preds_ps&de.pdf',path='../plots/', width = 25, height = 40, device= 'pdf')

save(list = c('pred.dat.de'), file = '../data/mostRecentPredDat_DE_alpha.rdata')

save.image(paste0('../large_data/stanoutput_de_alpha_',base::date(),'.rdata'))



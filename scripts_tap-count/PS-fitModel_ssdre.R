
library(dplyr)
#library(forcats)
library(ggplot2)
library(rstan)
knitr::opts_chunk$set(echo = F, message = F, warning = F)
rm(list=ls())

dat = read.csv('../data/proc_shif_data.csv') %>%
  transmute(subject = su,
            trial = bl,
            first.correct.trial.item = first,
            trials.since.1st.correct = bl_r,
            reversions = ifelse(itype == 1, F,T),
            item,
            strategy = ifelse(strat==1, 'algorithm','retrieval'),
            is.alg = ifelse(strategy == 'algorithm', 1, 0),
            is.ret = ifelse(strategy == 'retrieval', 1 , 0),
            RT) %>%
  mutate(subject = as.numeric(as.factor(subject)),
         item = as.numeric(as.factor(item)))

source('../scripts_general/mk_sparse_sub-item_matrix.R')
si.lookup = siMat(dat)

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
                 nsi = nrow(dat %>% count(subject, item)),
                 nt = length(unique(dat$trial)),
                 # Find a reasonable location for beta:
                 # 85th %ile of subject*item first correct retrieval trial (ok?)
                 betaBase = dat %>% 
                   group_by(subject, item) %>%
                   summarize(first.correct = mean(first.correct.trial.item)) %>%
                   ungroup() %>%
                   summarize(beta.loc = quantile(first.correct,.85)) %>%
                   .$beta.loc,
                 si_lookup = si.lookup
)
# For execution on a local, multicore CPU with excess RAM we recommend calling
# options(mc.cores = parallel::detectCores())
# To avoid recompilation of unchanged Stan programs, we recommend calling
rstan_options(auto_write = TRUE)

fit.ps <- stan(
  file = './PS-modelSpec.stan',
  #model_code = mod,  # Stan program
  data = stan.data,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 1000,          # number of warmup iterations per chain
  iter = 3000,            # total number of iterations per chain
  cores = 4,              # number of cores
  refresh = 250,          # show progress every 'refresh' iterations
  control = list(adapt_delta = 0.99,  max_treedepth = 15)
)


# fit.de <- stan(
#   file = './delayedExp.stan',
#   #model_code = mod,  # Stan program
#   data = stan.data,    # named list of data
#   chains = 4,             # number of Markov chains
#   warmup = 1000,          # number of warmup iterations per chain
#   iter = 3000,            # total number of iterations per chain
#   cores = 4,              # number of cores
#   refresh = 250,          # show progress every 'refresh' iterations
#   control = list(adapt_delta = 0.99,  max_treedepth = 15)
# 
# )

#params = get_sampler_params(fit) # apparently this is useful for something...

coefs.ps = summary(fit.ps)$summary[,1] # pull out just the mean of each coef


pred.dat.ps = dat %>%
  mutate_at(c('subject','item'), function(x) as.numeric(as.factor(x))) %>%
  mutate(strategy = as.integer(strategy == 'retrieval')+1,
         logRT = log(RT),
         pred.logRT.ps = NA)

for(row in 1:nrow(pred.dat.ps)){ 
  pred.dat.ps[row, 'pred.logRT.ps'] = coefs.ps[paste0('y_hat[',row,']')]
  pred.dat.ps[row,'pred.RT.ps'] = exp(pred.dat.ps[row,'pred.logRT.ps'])
}

samples.ps <- extract(fit.ps, pars = c('Mu','Tau','Beta','y_hat','logLik','lp__')) %>% as.data.frame()

# Save the relevant stuff
save(list = c('fit.ps',
              'pred.dat.ps',
              'samples.ps'),
     file = '../output/PS-fitData.rdata')

# launch the report generator
rmarkdown::render('PS-modelReport.Rmd', output_file = '../output/PS-ModelReport.html')

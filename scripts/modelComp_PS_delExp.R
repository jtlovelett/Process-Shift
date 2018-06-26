# compare the hbm fits of process shift and delay4ed exponetial models

library(tidyverse)
library(rstan)
path = '~/Documents/UCSD/Research/Process Shift/data/'

load(paste0(path, 'data_from_delExp_fit.rdata'))
pred.dat=pred.dat %>%
  rename(pred.RT.delExp = pred.RT,
         pred.logRT.delExp = pred.logRT)
#fit.delExp = fit
#rm(list=ls()[!ls() %in% c('pred.dat','fit.delExp', 'path')])

load(paste0(path, 'stan_fit_1.RData'))
fit.ps = fit
#rm(fit)


coefs.ps = summary(fit.ps)$summary[,1]

for(row in 1:nrow(pred.dat)){ 
  pred.dat[row, 'pred.logRT.ps'] = coefs.ps[paste0('y_hat[',row,']')]
  
  pred.dat[row,'pred.RT.ps'] = exp(pred.dat[row,'pred.logRT.ps'])
}

pred.dat %>% 
  mutate(logRT = log(RT)) %>%
  summarize(ps.r2 = cor(logRT, pred.logRT.ps)^2,
            delExp.r2 = cor(logRT, pred.logRT.delExp)^2)
save(pred.dat, file = paste0(path,'model.predictions.ps&delExp.Rdata'))


sub.plot = pred.dat %>%
  mutate(logRT = log(RT),
         strategy = as.factor(strategy)) %>%
  ggplot(aes(x = trial, color = strategy, group=strategy))+
  geom_point(aes(y=logRT))+
  geom_line(aes(y=pred.logRT.delExp), color = 'green')+
  geom_line(aes(y=pred.logRT.ps), color = 'blue')+
  #geom_vline(aes(xintercept=first.correct.trial.item))+
  facet_grid(subject~item)

sub.plot %>%  ggsave(filename='subj.plot.preds_delExp_ps.pdf',path=paste0(path,'../plots/'), width = 25, height = 40, device= 'pdf')


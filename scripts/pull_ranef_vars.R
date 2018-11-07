setwd('~/Documents/UCSD/Research/Process-Shift/large_data/')
f = 'stanoutput_de_Tue Jul 10 19:54:11 2018.rdata'

load(f)

samps = lapply(fit.de@sim$samples, function(chain) sapply(chain, mean))

effects = c('A_esi','B_esi','T_esi','R_esi')
names(effects) = effects

ranefs_per_effect = lapply(effects, function(effect) 
  lapply(samps, function(chain) 
    chain[grepl(effect, names(chain))]))

ranef_vars_per_chain = knitr::kable(data.frame(chain = 1:4, 
                                  lapply(ranefs_per_effect, function(chain)
                                    sapply(chain, var))))

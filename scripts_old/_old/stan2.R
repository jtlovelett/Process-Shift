# 1. bayesian inference.
# specifying a forward model.
# 
# 2. MAP search vs posterior
# 3. sampling, MCMC sampling
# 4. stan
library(tidyverse)

logprob_to_prob = function(log.prob){
  log.prob = log.prob - max(log.prob)
  prob = exp(log.prob)
  prob / sum(prob)
}

df <- data.frame(x=rbinom(10, 1, 0.2))

log.likelihood = function(p){
  N = length(df$x)
  k = sum(df$x)
  log(p)*k + log(1-p)*(N-k)
}

log.prior = function(p){
  dbeta(p, 3,3, log=T)
}

nlog.posterior = function(p){
  -log.likelihood(p)-log.prior(p)
}

library(stats4)

map.fit <- mle(nlog.posterior, start=list(p=0.5))
summary(map.fit)


p = seq(0.001, 0.999, by=0.001)

results <- data.frame(p) %>%
  mutate(log.likelihood = log.likelihood(p),
         log.prior = log.prior(p),
         log.posterior = log.likelihood + log.prior)

sum(p*dbeta(p, 7,3))/sum(dbeta(p,7,3))
mean(rbeta(10000, 7, 3))
results %>% 
  ggplot(aes(x=p, y=logprob_to_prob(log.posterior)))+
  geom_line()

# posterior mean:
results %>% 
  summarize(mean.p = weighted.mean(x=p, w=logprob_to_prob(log.posterior)),
            sd.p = sqrt(weighted.mean(x=(p-mean.p)^2, w=logprob_to_prob(log.posterior))))

# IN STAN
library(rstan)

stan.code = '
data {
int<lower=0> N; // number of data points
int x[N]; // observed X
}
parameters {
real<lower=0, upper=1> p; 
}
model {
p ~ beta(3,3);
x ~ bernoulli(p);
}
'
df$x[3] = 3
data.for.stan <- list(
  N = nrow(df),
  x = df$x
)

fit <- stan(
  # file = stan.file,   
  model_code = stan.code,  # Stan program
  data = data.for.stan,    # named list of data
  chains = 1,             # number of Markov chains
  warmup = 50,          # number of warmup iterations per chain
  iter = 100,            # total number of iterations per chain
  cores = 1,              # number of cores (using 2 just for the vignette)
  refresh = 100          # show progress every 'refresh' iterations
)

fit

samples <- extract(fit) %>% as.data.frame()

ggplot(samples, aes(x=p))+
  geom_histogram()
plot(fit@sim$samples[[1]]$p)
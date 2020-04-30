# 1. bayesian inference.
# 2. MAP search vs posterior
# 3. sampling
# 4. stan

library(rstan)
df <- data.frame(x=c(rnorm(10, 5, 4),rnorm(10, 3, 4),rnorm(10, 2, 4)),
                 g = c(rep(1,10), rep(2,10), rep(3,10)))

stan.code = '
data {
int<lower=0> N; // number of data points
real x[N]; // observed X
int g[N];
int K; // number of groups
}
parameters {
real mu[K]; 
real<lower=0> sigma[K];
real<lower=0> tau;
real gmu;
}
model {
sigma ~ exponential(2);
tau ~ exponential(2);
mu ~ normal(gmu, tau);
x ~ normal(mu[g], sigma[g]);
}
'
# stan.file = 'model.stan'
# fp <- file(stan.file)
# writeLines(stan.code, fp)
# close(fp)

data.for.stan <- list(
  N = nrow(df),
  x = df$x,
  g = df$g,
  K = length(unique(df$g))
)
xxxxxx
fit <- stan(
  # file = stan.file,   
  model_code = stan.code,  # Stan program
  data = data.for.stan,    # named list of data
  chains = 4,             # number of Markov chains
  warmup = 100,          # number of warmup iterations per chain
  iter = 10000,            # total number of iterations per chain
  cores = 1,              # number of cores (using 2 just for the vignette)
  refresh = 100          # show progress every 'refresh' iterations
)
fit
samples <- extract(fit) %>% as.data.frame()

ggplot(samples, aes(x=mu.1, y=mu.2, color=lp__))+
  geom_point(size=0.1)


ggplot(samples, aes(x=sigma))+
  geom_histogram()


data {
  int<lower=0> N; // num observations
  int<lower=0> ni; // num items
  int<lower=0> ns; // num subjects
  int<lower=0> nt; // num trials
  int<lower=0> nc; // num strategies
  real<lower=0> y[N]; // log RT 
  int<lower=0> strategy[N]; // strategy for a given obs
  int<lower=0> item[N]; // item for a given obs
  int<lower=0> subject[N]; // subject for a given obs
  int<lower=0> trial[N]; // trial for a given obs
  int<lower=0> isAlg[N]; // whether strat==alg for a given obs
  int<lower=0> isRet[N]; // whether strat==ret for a given obs
}

parameters {
  real Alg_M; // overall mean for Algorithm
  real Alg_M_es[ns]; // offset per subject for Alg mu
  real Alg_M_ei[ni]; // offset per item for Alg mu
  real Alg_M_esi[ns,ni]; // offset per item for Alg mu
  real<lower=0> sigma_Alg_M_ei; // priors on Algorithm components ...
  real<lower=0> sigma_Alg_M_es; 
  real<lower=0> sigma_Alg_M_esi; 

  real Ret_B; // overall Beta for Retrieval
  real Ret_B_es[ns]; // offset per subject for Ret beta
  real Ret_B_ei[ni]; // offset per item for Ret beta
  real Ret_B_esi[ns,ni]; // offset per item for Ret beta
  real<lower=0> sigma_Ret_B_ei; // priors on Retrieval Beta components ...
  real<lower=0> sigma_Ret_B_es; 
  real<lower=0> sigma_Ret_B_esi; 

  real Ret_T; // overall Tau for Retrieval (rate)
  real Ret_T_es[ns]; // offset per subject for Ret Tau
  real Ret_T_ei[ni]; // offset per item for Ret Tau
  real Ret_T_esi[ns,ni]; // offset per item for Ret Tau
  real<lower=0> sigma_Ret_T_ei; // priors on Retrieval Tau components ...
  real<lower=0> sigma_Ret_T_es; 
  real<lower=0> sigma_Ret_T_esi; 

  // add asymptote parameter? 

  // add sigma variability 

  real<lower=0> sigma; // expand this to something similar to the above (variable sigma)
}

transformed parameters {  
  real Mu[N];
  real Tau[N];
  real Beta[N];
  real y_hat[N];
  for(i in 1:N){
    Mu[i] = Alg_M + Alg_M_es[subject[i]]+Alg_M_ei[item[i]]+Alg_M_esi[subject[i], item[i]];
    Beta[i] = Ret_B + Ret_B_es[subject[i]]+Ret_B_ei[item[i]]+Ret_B_esi[subject[i], item[i]];
    Tau[i] = Ret_T + Ret_T_es[subject[i]]+Ret_T_ei[item[i]]+Ret_T_esi[subject[i], item[i]];
    y_hat[i] =  isAlg[i]*( Mu[i] ) + isRet[i]*( Beta[i] +exp(Tau[i])*log(17) - exp(Tau[i])*log(trial[i]));
  }
 }

model {
  sigma ~ exponential(0.01);

  Alg_M ~ normal(7.6, 0.9); // set as overall mean and sd of data
  Ret_B ~ normal(7.3, 1); // set as mean, sd for retrieval trials
  Ret_T ~ normal(-1, 2); // no idea if this is reasonable ... 

  sigma_Alg_M_es ~ exponential(0.01);
  sigma_Alg_M_ei ~ exponential(0.01);
  sigma_Alg_M_esi ~ exponential(0.01);

  sigma_Ret_B_es ~ exponential(0.01);
  sigma_Ret_B_ei ~ exponential(0.01);
  sigma_Ret_B_esi ~ exponential(0.01);

  sigma_Ret_T_es ~ exponential(0.01);
  sigma_Ret_T_ei ~ exponential(0.01);
  sigma_Ret_T_esi ~ exponential(0.01);
  
  Alg_M_es ~ normal(0, sigma_Alg_M_es);
  Alg_M_ei ~ normal(0, sigma_Alg_M_ei);
  for(i in 1:ns) {
    for(j in 1:ni)
      Alg_M_esi[i,j] ~ normal(0, sigma_Alg_M_esi);
  }

  Ret_B_es ~ normal(0, sigma_Ret_B_es);
  Ret_B_ei ~ normal(0, sigma_Ret_B_ei);
  for(i in 1:ns) {
    for(j in 1:ni)
      Ret_B_esi[i,j] ~ normal(0, sigma_Ret_B_esi);
  }

  Ret_T_es ~ normal(0, sigma_Ret_T_es);
  Ret_T_ei ~ normal(0, sigma_Ret_T_ei);
  for(i in 1:ns) {
    for(j in 1:ni)
      Ret_T_esi[i,j] ~ normal(0, sigma_Ret_T_esi);
  }

  y ~ normal(y_hat, sigma);
}

generated quantities {
  vector[N] logLik; 
  for(n in 1:N){
    logLik[n] = normal_lpdf(y[n] | y_hat[n], sigma);
  }
}
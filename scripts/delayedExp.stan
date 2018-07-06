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
}

parameters {
  real A; 
  real A_es[ns]; 
  real A_ei[ni]; 
  real A_esi[ns,ni]; 
  real<lower=0> sigma_A_ei;
  real<lower=0> sigma_A_es; 
  real<lower=0> sigma_A_esi; 

  real B;
  real B_es[ns]; 
  real B_ei[ni]; 
  real B_esi[ns,ni]; 
  real<lower=0> sigma_B_ei; 
  real<lower=0> sigma_B_es; 
  real<lower=0> sigma_B_esi; 

  real T; 
  real T_es[ns]; 
  real T_ei[ni]; 
  real T_esi[ns,ni]; 
  real<lower=0> sigma_T_ei; 
  real<lower=0> sigma_T_es; 
  real<lower=0> sigma_T_esi; 

  real R; 
  real R_es[ns]; 
  real R_ei[ni]; 
  real R_esi[ns,ni]; 
  real<lower=0> sigma_R_ei; 
  real<lower=0> sigma_R_es; 
  real<lower=0> sigma_R_esi; 

  // add sigma variability 

  real<lower=0> sigma;
}

transformed parameters {  
  real log_Alpha[N];
  real log_Beta[N];
  real Tau[N];
  real Rate[N];
  real y_hat[N];
  for(i in 1:N){ 
    log_Alpha[i] = A + A_es[subject[i]] + A_ei[item[i]] + A_esi[subject[i], item[i]];
    log_Beta[i] = B + B_es[subject[i]] + B_ei[item[i]] + B_esi[subject[i], item[i]];
    Rate[i] = R + R_es[subject[i]] + R_ei[item[i]] + R_esi[subject[i], item[i]];
    Tau[i] = T + T_es[subject[i]] + T_ei[item[i]] + T_esi[subject[i], item[i]];
    Tau[i] = exp(Tau[i]*Rate[i]);
    // print(log_Alpha[i]);
    // print(log_Beta[i]);
    // print(Tau[i]);
    // print(Rate[i]);
    // print(exp(log_Alpha[i]) + exp(log_Beta[i]) * (Tau[i]+1) / (Tau[i] + trial[i]^Rate[i]));
    // print(log(exp(log_Alpha[i]) + exp(log_Beta[i]) * (Tau[i]+1) / (Tau[i] + trial[i]^Rate[i])));
    // print("");
    y_hat[i] =  log(exp(log_Alpha[i]) + exp(log_Beta[i]) * (Tau[i]+1) / (Tau[i] + trial[i]^Rate[i]));
  }
}

model {
  sigma ~ exponential(.1);

  A ~ normal(0, 5); // set as overall mean and sd of data
  B ~ normal(8, 5); // set as mean, sd for retrieval trials
  T ~ normal(3, 2); // was centered on 0
  R ~ normal(-1, 2);

  sigma_A_es ~ exponential(1); 
  sigma_A_ei ~ exponential(1);
  sigma_A_esi ~ exponential(1);

  sigma_B_es ~ exponential(1);
  sigma_B_ei ~ exponential(1);
  sigma_B_esi ~ exponential(1);

  sigma_R_es ~ exponential(1);
  sigma_R_ei ~ exponential(1);
  sigma_R_esi ~ exponential(1);

  sigma_T_es ~ exponential(1);
  sigma_T_ei ~ exponential(1);
  sigma_T_esi ~ exponential(1);
  
  A_es ~ normal(0, sigma_A_es);
  A_ei ~ normal(0, sigma_A_ei);
  for(i in 1:ns) {
    for(j in 1:ni)
      A_esi[i,j] ~ normal(0, sigma_A_esi);
  }

  B_es ~ normal(0, sigma_B_es);
  B_ei ~ normal(0, sigma_B_ei);
  for(i in 1:ns) {
    for(j in 1:ni)
      B_esi[i,j] ~ normal(0, sigma_B_esi);
  }

  R_es ~ normal(0, sigma_R_es);
  R_ei ~ normal(0, sigma_R_ei);
  for(i in 1:ns) {
    for(j in 1:ni)
      R_esi[i,j] ~ normal(0, sigma_R_esi);
  }

  T_es ~ normal(0, sigma_T_es);
  T_ei ~ normal(0, sigma_T_ei);
  for(i in 1:ns) {
    for(j in 1:ni)
      T_esi[i,j] ~ normal(0, sigma_T_esi);
  }

  y ~ normal(y_hat, sigma); # y_hat coming back nan ... why? (location parameter)
}
generated quantities {
  vector[N] logLik; 
  for(n in 1:N){
    logLik[n] = normal_lpdf(y[n] | y_hat[n], sigma);
  }
}

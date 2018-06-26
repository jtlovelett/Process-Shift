data {
  int<lower=0> N; // num observations
  int<lower=0> ni; // num items
  int<lower=0> ns; // num subjects
  int<lower=0> nt; // num trials
  int<lower=0> nc; // num strategies
  real<lower=0> y[N]; // log10 RT 
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

  real y_hat[N];

  real<lower=0> sigma;
}

transformed parameters {  
  real Alpha[N];
  real Beta[N];
  real Tau[N];
  real Rate[N];
  
  for(i in 1:N){
    Alpha[i] = A;// + A_es[subject[i]] + A_ei[item[i]] + A_esi[subject[i], item[i]];
    Beta[i] = B;// + B_es[subject[i]] + B_ei[item[i]] + B_esi[subject[i], item[i]];
    Rate[i] = R;// + R_es[subject[i]] + R_ei[item[i]] + R_esi[subject[i], item[i]];
    Tau[i] = T;// + T_es[subject[i]] + T_ei[item[i]] + T_esi[subject[i], item[i]]));
    y_hat[i] =  2*log(Alpha[i] + 2*(Rate[i] * Tau[i]) - log(Beta[i]));
      //Alpha[i] + Beta[i] * (Tau[i]+1) / (Tau[i] + trial[i]^Rate[i]);
  }
 }

model {
  sigma ~ exponential(0.01);

  Alpha ~ normal(7.6, 0.9); // set as overall mean and sd of data
  Beta ~ normal(7.6, 0.9); // set as mean, sd for retrieval trials
  Tau ~ exponential(.005); 
  Rate ~ normal(-1, 2);

  sigma_A_es ~ exponential(0.01);
  sigma_A_ei ~ exponential(0.01);
  sigma_A_esi ~ exponential(0.01);

  sigma_B_es ~ exponential(0.01);
  sigma_B_ei ~ exponential(0.01);
  sigma_B_esi ~ exponential(0.01);

  sigma_R_es ~ exponential(0.01);
  sigma_R_ei ~ exponential(0.01);
  sigma_R_esi ~ exponential(0.01);

  sigma_T_es ~ exponential(0.01);
  sigma_T_ei ~ exponential(0.01);
  sigma_T_esi ~ exponential(0.01);
  
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

  y_hat ~ nornaml(log(Alpha + Beta * ( (Tau+1) / (Tau+exp(r*N)) ) ))

  y ~ normal(y_hat, sigma);
}
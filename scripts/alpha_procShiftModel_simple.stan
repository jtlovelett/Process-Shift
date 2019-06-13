data {
  int<lower=0> N; // num observations
  int<lower=0> ni; // num items
  int<lower=0> ns; // num subjects
  int<lower=0> nsi; // num subject-item combos
  int<lower=0> si_lookup[ns, ni]; // lookup table for sparsely coded subject-item offsets
  int<lower=0> nt; // num trials
  int<lower=0> nc; // num strategies
  real<lower=0> y[N]; // log RT 
  int<lower=0> strategy[N]; // strategy for a given obs
  int<lower=0> item[N]; // item for a given obs
  int<lower=0> subject[N]; // subject for a given obs
  int<lower=0> trial[N]; // trial for a given obs
  int<lower=0> isAlg[N]; // whether strat==alg for a given obs
  int<lower=0> isRet[N]; // whether strat==ret for a given obs
  int<lower=0> isUnpr[N]; // whether strat was unprobed
}

parameters {
  real Alg_M; // overall mean for Algorithm
  // real Alg_M_es[ns]; // offset per subject for Alg mu
  // real Alg_M_ei[ni]; // offset per item for Alg mu
  // real Alg_M_esi[nsi]; // offset per subject-item for Alg mu (spasely coded)
  // real<lower=0> sigma_Alg_M_ei; // priors on Algorithm components ...
  // real<lower=0> sigma_Alg_M_es; 
  // real<lower=0> sigma_Alg_M_esi; 

  real Ret_B; // overall Beta for Retrieval
  // real Ret_B_es[ns]; // offset per subject for Ret beta
  // real Ret_B_ei[ni]; // offset per item for Ret beta
  // real Ret_B_esi[nsi]; 
  // real<lower=0> sigma_Ret_B_ei; // priors on Retrieval Beta components ...
  // real<lower=0> sigma_Ret_B_es; 
  // real<lower=0> sigma_Ret_B_esi; 

  real Ret_T; // overall Tau for Retrieval (rate)
  // real Ret_T_es[ns]; // offset per subject for Ret Tau
  // real Ret_T_ei[ni]; // offset per item for Ret Tau
  // real Ret_T_esi[nsi]; 
  // real<lower=0> sigma_Ret_T_ei; // priors on Retrieval Tau components ...
  // real<lower=0> sigma_Ret_T_es; 
  // real<lower=0> sigma_Ret_T_esi; 

  // add asymptote parameter? 

  // add sigma variability 

  real<lower=0> sigma; // expand this to something similar to the above (variable sigma)


  real p_Ret_alpha; // parameter 1 of logisitc function predicting p
  // real p_Ret_alpha_ei[ni];
  // real p_Ret_alpha_es[ns];
  // real p_Ret_alpha_esi[nsi];
  // real<lower=0> sigma_p_Ret_alpha_ei;
  // real<lower=0> sigma_p_Ret_alpha_es;
  // real<lower=0> sigma_p_Ret_alpha_esi;

  real p_Ret_gamma; // parameter 2 of logisitc function predicting p
  // real p_Ret_gamma_ei[ni];
  // real p_Ret_gamma_es[ns];
  // real p_Ret_gamma_esi[nsi];
  // real<lower=0> sigma_p_Ret_gamma_ei;
  // real<lower=0> sigma_p_Ret_gamma_es;
  // real<lower=0> sigma_p_Ret_gamma_esi;
}

transformed parameters {  
  real Mu[N];
  real Tau[N];
  real Beta[N];
  real Alpha[N];
  real Gamma[N];
  real p_Ret[N];
  real y_hat_Alg[N];
  real y_hat_Ret[N];
  for(i in 1:N){
    Mu[i] = Alg_M //+ Alg_M_es[subject[i]]+Alg_M_ei[item[i]]+Alg_M_esi[si_lookup[subject[i], item[i]]];
    Beta[i] = Ret_B //+ Ret_B_es[subject[i]]+Ret_B_ei[item[i]]+Ret_B_esi[si_lookup[subject[i], item[i]]];
    Tau[i] = Ret_T //+ Ret_T_es[subject[i]]+Ret_T_ei[item[i]]+Ret_T_esi[si_lookup[subject[i], item[i]]];

    Alpha[i] = p_Ret_alpha //+ p_Ret_alpha_es[subject[i]] + p_Ret_alpha_ei[item[i]] + p_Ret_alpha_esi[si_lookup[subject[i], item[i]]];
    Gamma[i] = p_Ret_gamma //+ p_Ret_gamma_es[subject[i]] + p_Ret_gamma_ei[item[i]] + p_Ret_gamma_esi[si_lookup[subject[i], item[i]]];

    p_Ret[i] = 1 / (1 + exp(-(Alpha[i]+Gamma[i]*log(trial[i]))));

    y_hat_Alg[i] = Mu[i];
    y_hat_Ret[i] = Beta[i] + exp(Tau[i])*log(17) - exp(Tau[i])*log(trial[i]);
  }
 }

model {

  // Top-level priors
  sigma ~ exponential(0.01);

  Alg_M ~ normal(7.6, 0.9); // set as overall mean and sd of data
  Ret_B ~ normal(7.3, 1); // set as mean, sd for retrieval trials
  Ret_T ~ normal(-1, 2); // no idea if this is reasonable ... 
  p_Ret_alpha ~ normal(0,2); // 
  p_Ret_gamma ~ normal(0,2); // 

  // Algorithm strategy priors on component variances

  // sigma_Alg_M_es ~ exponential(0.01);
  // sigma_Alg_M_ei ~ exponential(0.01);
  // sigma_Alg_M_esi ~ exponential(0.01);

  // // Retrieval strategy priors on component variances
  // sigma_Ret_B_es ~ exponential(0.01);
  // sigma_Ret_B_ei ~ exponential(0.01);
  // sigma_Ret_B_esi ~ exponential(0.01);

  // sigma_Ret_T_es ~ exponential(0.01);
  // sigma_Ret_T_ei ~ exponential(0.01);
  // sigma_Ret_T_esi ~ exponential(0.01);

  // // P(retrieval strategy chosen -- priors for variances of logisitc function params)

  // sigma_p_Ret_alpha_es ~ exponential(0.01);
  // sigma_p_Ret_alpha_ei ~ exponential(0.01);
  // sigma_p_Ret_alpha_esi ~ exponential(0.01);

  // sigma_p_Ret_gamma_es ~ exponential(0.01);
  // sigma_p_Ret_gamma_ei ~ exponential(0.01);
  // sigma_p_Ret_gamma_esi ~ exponential(0.01);

  // // Algorithm strategy parameters
  
  // Alg_M_es ~ normal(0, sigma_Alg_M_es);
  // Alg_M_ei ~ normal(0, sigma_Alg_M_ei);
  // Alg_M_esi ~ normal(0, sigma_Alg_M_esi);

  // // Algorithm strategy parameters

  // Ret_B_es ~ normal(0, sigma_Ret_B_es);
  // Ret_B_ei ~ normal(0, sigma_Ret_B_ei);
  // Ret_B_esi ~ normal(0, sigma_Ret_B_esi);

  // Ret_T_es ~ normal(0, sigma_Ret_T_es);
  // Ret_T_ei ~ normal(0, sigma_Ret_T_ei);
  // Ret_T_esi ~ normal(0, sigma_Ret_T_esi);

  // // P(strat == retrieval)

  // p_Ret_alpha_es ~ normal(0, sigma_p_Ret_alpha_es);
  // p_Ret_alpha_ei ~ normal(0, sigma_p_Ret_alpha_ei);
  // p_Ret_alpha_esi ~ normal(0, sigma_p_Ret_alpha_esi);

  // p_Ret_gamma_es ~ normal(0, sigma_p_Ret_gamma_es);
  // p_Ret_gamma_ei ~ normal(0, sigma_p_Ret_gamma_ei);
  // p_Ret_gamma_esi ~ normal(0, sigma_p_Ret_gamma_esi);

  for (n in 1:N){
    if(isRet[n]){
      target += normal_lpdf(y[n] | y_hat_Ret[n], sigma );
      target += log(p_Ret[n]);
    } else if (isAlg[n]){
        target += normal_lpdf(y[n] | y_hat_Alg[n], sigma);
        target += log(1-p_Ret[n]);
    } else {
        target += log_mix(p_Ret[n],
                          normal_lpdf(y[n] | y_hat_Ret[n], sigma ) , // do I need to add in the prior log(p_Ret[n]) ...
                          normal_lpdf(y[n] | y_hat_Alg[n], sigma)); // ... and log(1-p_Ret[n]) here?
    }
  }
}
// generate some data that conforms to the model and then see if we can recover the parameters 


generated quantities {
  vector[N] logLik; 
  for(n in 1:N){
    if(isRet[n]){
      logLik[n] = normal_lpdf(y[n] | y_hat_Ret[n], sigma ) + log(p_Ret[n]);
    } else if (isAlg[n]){
        logLik[n] = normal_lpdf(y[n] | y_hat_Alg[n], sigma) + log(1-p_Ret[n]);
    } else {
        logLik[n] = log_mix(p_Ret[n],
                          normal_lpdf(y[n] | y_hat_Ret[n], sigma ),
                          normal_lpdf(y[n] | y_hat_Alg[n], sigma));
    }
  }
}

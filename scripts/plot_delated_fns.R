library(tidyverse)

delPow <- function(alpha, beta, r, tau, N){
  frac <- (tau + 1) / (tau+(N^r))
  alpha + beta*frac
}

pow <- function(alpha, beta, r, N){
  frac <- N^(-r)
  alpha + beta*frac
}
expon <- function(alpha, beta, r, N){
  frac <- exp(-r*N)
  alpha + beta*frac
}

delExp <- function(alpha, beta, r, tau, N){
  frac <- (tau + 1) / (tau+exp(r*N))
  alpha + beta*frac
}

n <- 1:50
r <- .7
alpha <- 1
beta <- 2
tau <- c(#0,1,5,10,15,20,
  50,200,500,1000,10000)

tries <- expand.grid(n,r,alpha,beta,tau)
colnames(tries) <- c('n','r','alpha','beta','tau')
tries <- tries %>% 
  #rowwise() %>%
  mutate(delPow = delPow(alpha,beta,r,tau,n),
         delExp = delExp(alpha,beta,r,tau,n)) %>%
  gather(pred_type, y_hat, delPow:delExp)

# um... I don't see the characteristic delay... poor parameter choices?
tries %>%
  ggplot(aes(x = n, y = y_hat, group = tau))+
  geom_line()+
  facet_grid(tau~pred_type)
  
# non-delay
dat <- data_frame(n=n, r= r, alpha=alpha, beta = beta)
dat %>% 
  mutate(pow = pow(alpha,beta,r,n),
         expon = expon(alpha,beta,r,n)) %>%
  gather(pred_type, y_hat, pow:expon) %>%
  ggplot(aes(x = n, y = y_hat, color = pred_type))+
  geom_line()

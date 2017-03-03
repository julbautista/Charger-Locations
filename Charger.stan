data{
  // int<lower = 0> N;
  real rem[100];
  real use[100];
}

parameters{
  real a1;
  real b1;
  real a_i;
  real b_i;
}

transformed parameters{
  real<lower = 0> alpha[100];
  real<lower = 0> beta[100];
  for(i in 1:100){
  alpha[i] = a_i + rem[i]*a1;
  beta[i] = b_i + rem[i]*b1;
}
}

model{
  // a1 ~ normal(0,2);
  // b1 ~ normal(0,2);
  a_i ~ normal(0.7,1);
  b_i ~ normal(0.7,1);
  target+= beta_lpdf(use|alpha, beta);
}

generated quantities{
  real use_est[100];
  for (i in 1:100)
    use_est[i] = beta_rng(alpha[i],beta[i]);
}


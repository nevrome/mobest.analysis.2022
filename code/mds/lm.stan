data {
  int<lower=0> N;
  vector[N] x1;
  vector[N] x2;
  vector[N] y;
  real alpha_mean;
  real alpha_sd;
  real beta_x1_mean;
  real beta_x1_sd;
  real beta_x2_mean;
  real beta_x2_sd;
}
parameters {
  real alpha;
  real beta_x1;
  real beta_x2;
  real<lower=0> sigma;
}
model {
  y ~ normal(
    alpha + 
    beta_x1 * x1 + 
    beta_x2 * x2, 
    sigma
  );
  alpha ~ normal(alpha_mean, alpha_sd);
  beta_x1 ~ normal(beta_x1_mean, beta_x2_sd);
  beta_x2 ~ normal(beta_x1_mean, beta_x2_sd);
}

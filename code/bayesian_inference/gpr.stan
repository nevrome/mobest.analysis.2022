functions {
  
  matrix cov(
    vector[] x,
    real alpha,
    vector theta,
    real delta
  ) {
    
    // prepare variables
    int N = size(x);
    matrix[N, N] K;
    matrix[N, N] K_cholesky;
    real sq_alpha = square(alpha);
    
    // theta_1 and theta_2 (the spatial dimensions) should be equal
    vector[3] theta2;
    theta2[1] = theta[1];
    theta2[2] = theta[1];
    theta2[3] = theta[2];

    // calculate covariance matrix
    for (i in 1:(N-1)) {
      K[i, i] = sq_alpha + delta;
      for (j in (i + 1):N) {
        K[i, j] = sq_alpha * exp((-1) * dot_self((x[i] - x[j]) ./ theta2));
        K[j, i] = K[i, j];
      }
    }
    K[N, N] = sq_alpha + delta;
    
    // apply cholesky decomposition on covariance matrix
    K_cholesky = cholesky_decompose(K);
    
    return K_cholesky;
    
  }
  
}


data {
  int<lower=1> N;
  vector[3] x[N];
  vector[N] y;
}

// transformed data {
//   real delta = 1e-9;
// }

parameters {
  real<lower=0> alpha;
  vector<lower=0>[2] theta;
  real<lower=0> delta;
  real<lower=0> sigma;
  vector[N] eta;
}

transformed parameters {
  vector[N] f;
  {
    matrix[N, N] L_K = cov(x, alpha, theta, delta);
    f = L_K * eta;
  }
}

model {
  
  theta ~ normal(500, 500);
  sigma ~ normal(0, 0.0001);
  delta ~ normal(0, 0.01);
  eta ~ std_normal();
  // alpha: no prior at all, which in Stan is equivalent to a noninformative uniform prior on the parameter

  //print(f);

  y ~ normal(f, sigma);
  
}

// generate simulated observations
generated quantities {
  real y_sim[N];
  y_sim = normal_rng(f, sigma);
}







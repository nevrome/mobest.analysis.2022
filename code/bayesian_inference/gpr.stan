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
    
    return K;
    
  }
  
}

data {
  int<lower=1> N;
  vector[3] x[N];
  vector[N] y;
}

parameters {
  real<lower=0> alpha;
  vector<lower=0>[2] theta;
  real<lower=0> delta;
}

transformed parameters {
  matrix[N, N] Sigma = cov(x, alpha, theta, delta);
}

model {
  theta ~ normal(0, 50);
  delta ~ normal(0, 0.1);
  target += -log(alpha);
  y ~ multi_normal(rep_vector(0, N), Sigma);
}

// generate simulated observations
// generated quantities {
//   vector[N] y_sim;
//   y_sim = multi_normal_rng(rep_vector(0, N), Sigma);
// }

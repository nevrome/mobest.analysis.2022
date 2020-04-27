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

    // calculate covariance matrix
    for (i in 1:(N-1)) {
      K[i, i] = sq_alpha + delta;
      for (j in (i + 1):N) {
        K[i, j] = sq_alpha * exp((-1) * dot_self((x[i] - x[j]) ./ theta));
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
  int<lower=1> D;
  vector[D] x[N];
  vector[N] y;
}

transformed data {
  real delta = 1e-9;
}

parameters {
  real<lower=0> alpha;
  vector<lower=0>[D] theta;
  real<lower=0> sigma;
  vector[N] eta;
}

model {
  
  vector[N] f;
  {
    matrix[N, N] L_K = cov(x, alpha, theta, delta);
    f = L_K * eta;
  }

  theta ~ normal(0, 1.5);
  sigma ~ std_normal();
  eta ~ std_normal();

  y ~ normal(f, sigma);
  
}

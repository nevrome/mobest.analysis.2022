functions {
  
  matrix cov(
    vector[] x,
    vector theta,
    real nugget
  ) {
    
    // prepare variables
    int N = size(x);
    matrix[N, N] K;
    matrix[N, N] K_cholesky;

    // calculate covariance matrix
    for (i in 1:(N-1)) {
      K[i, i] = nugget;
      for (j in (i + 1):N) {
        K[i, j] = exp((-1) * dot_self((x[i] - x[j]) ./ theta));
        K[j, i] = K[i, j];
      }
    }
    K[N, N] = nugget;
    
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


parameters {
  vector<lower=0>[D] theta;
  real<lower=0> sigma;
  real <lower=1e-9, upper=1> nugget;
  vector[N] eta;
}


model {
  vector[N] f;
  {
    matrix[N, N] L_K = cov(x, theta, nugget);
    f = L_K * eta;
  }

  theta ~ uniform(1, 2000);
  sigma ~ std_normal();
  eta ~ std_normal();
  nugget ~ normal(0, 0.2);

  y ~ normal(f, sigma);
}

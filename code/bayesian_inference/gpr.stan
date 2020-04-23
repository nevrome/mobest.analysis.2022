functions {
  
  matrix L_cov_exp_quad_ARD(
    vector[] x,
    real alpha,
    vector rho,
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
        K[i, j] = exp(-dot_self((x[i] - x[j]) ./ rho));
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

parameters {
  vector<lower=0>[D] rho;
  real<lower=0> alpha;
  real<lower=0> sigma;
  real <lower=0, upper=1> delta;
  vector[N] eta;
}


model {
  vector[N] f;
  {
    matrix[N, N] L_K = L_cov_exp_quad_ARD(x, alpha, rho, delta);
    f = L_K * eta;
  }

  rho ~ inv_gamma(5, 5);
  alpha ~ std_normal();
  sigma ~ std_normal();
  eta ~ std_normal();

  y ~ normal(f, sigma);
}

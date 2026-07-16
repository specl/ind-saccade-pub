data {
  int<lower=0> N;
  int<lower=0> I;
  array[N] int<lower=0,upper=1> Y;
  vector[N] logDur;
  array[N] int sub;
  array[N] int cond;
  real s0;
}

parameters {
  matrix[I, 2] theta;
  vector[2] nu;
  vector<lower=0>[2] sigma;
  real<lower=-1,upper=1> rho;
}

transformed parameters {
  cov_matrix[2] Sigma;
  Sigma[1,1] = sigma[1]^2;
  Sigma[2,2] = sigma[2]^2;
  Sigma[1,2] = rho * sigma[1] * sigma[2];
  Sigma[2,1] = Sigma[1,2];
}

model {
  target += normal_lpdf(nu | 0, 10);
  target += normal_lpdf(sigma | 0, s0);
  for (i in 1:I)
    target += multi_normal_lpdf(theta[i] | nu, Sigma);
  for (n in 1:N)
    target += bernoulli_lpmf(Y[n] | Phi(logDur[n] - theta[sub[n], cond[n]]));
}

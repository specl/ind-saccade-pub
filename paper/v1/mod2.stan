data {
  int<lower=1> I;
  int<lower=1> J;
  int<lower=1> F;
  array[I] vector[J] Y;   // <-- changed from matrix[I, J] Y
  matrix[J, F] free;
}

parameters {
  vector<lower=0>[J] lam;
  real<lower=-1, upper=1> rho;
  vector<lower=0>[J] sigma;
}

transformed parameters {
  matrix[J, J] L;   // Cholesky factor of Sigma
  {
    matrix[J, F] Lambda = rep_matrix(0.0, J, F);
    matrix[F, F] Phi;
    int k = 1;

    for (f in 1:F)
      for (j in 1:J)
        if (free[j, f] == 1) {
          Lambda[j, f] = lam[k];
          k += 1;
        }

    Phi[1,1] = 1.0;  Phi[2,2] = 1.0;
    Phi[1,2] = rho;  Phi[2,1] = rho;

    L = cholesky_decompose(Lambda * Phi * Lambda' + diag_matrix(square(sigma)));
  }
}

model {
  lam   ~ normal(0.7, 0.5);
  sigma ~ normal(0.5, 0.5);

  target += multi_normal_cholesky_lpdf(Y | rep_vector(0.0, J), L);
}
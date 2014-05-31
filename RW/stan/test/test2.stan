data {
  int<lower=0> n;
  real y[n];
}
parameters {
  real x[n];
  real<lower=0> sigma_x;
  real<lower=0> sigma_y;
  real y1;
}
model {
  x[1] ~ normal(y1, sigma_x);
  y[1] ~ normal(x[1], sigma_y);

  for (i in 2:n) {
    x[i] ~ normal(x[i - 1], sigma_x);
    y[i] ~ normal(x[i], sigma_y);
  }

  y1 ~ normal(0.0, 1.0e+4);
  sigma_x ~ uniform(0.0, 1.0e+4);
  sigma_y ~ uniform(0.0, 1.0e+4);
}
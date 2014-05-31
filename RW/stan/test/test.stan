data {
	int<lower=0> N;
	int<lower=0> n[N]; // total beetle number
	int<lower=0> y[N]; // dead beetle number
	real x[N]; // dead beetle number
}
parameters {
	real alpha;
	real beta;
}
transformed parameters {
	real q[N];
	real m;
	m <- mean(x);
	for (i in 1:N)
		q[i] <- inv_logit(alpha+beta*(x[i]-m));
}
model {
	for (i in 1:N)
		y[i]~binomial(n[i],q[i]);
	alpha~normal(0,1.0e+4);
	beta~normal(0,1.0e+4);
}
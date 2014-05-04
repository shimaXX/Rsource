# kubo book chapter 10
data {
	int<lower=0> N; #sample size
	int<lower=0> Y[N]; # response variable
}
parameters {
	real beta;
	real r[N];
	real<lower=0> sigma;
}
transformed parameters{
	real q[N];

	for(i in 1:N){
		q[i]<-inv_logit(beta + r[i]); # survival ratio
	}
}
model {
	for(i in 1:N){
		Y[i]~binomial(8, q[i]);
	}
	beta~normal(0,100); # non-informative prior distribution
	r~normal(0, sigma); # hierarchical prior distribution
	sigma~uniform(0, 1.0e+4); # non-infomative prior distribution
}

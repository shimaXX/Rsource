# kubo book chapter 10
data {
    int<lower=0> N; #sample size
    int<lower=0> N_col; #colmun size
    int<lower=0> Y[N]; # response variable
    int<lower=0> minutes[N];
    vector[N_col] X[N];
    vector[N] X_M;
    matrix[N_col, N_col] v0;
	cov_matrix[N_col] sigma0;
}
transformed data { # hyperparameter
    vector[N_col] mu;
#    cov_matrix[N_col] sigma0;
    for(i in 1:N_col){
#        sigma0[i,i] <- 100;
        mu[i] <- 0;
	}
}
parameters {
    vector[N_col] beta;
    real r[N];
    real<lower=0> sigma;

	vector[N_col] delta;
	cov_matrix[N_col] V_B;
}
transformed parameters{
     real q[N];
     real<lower=0> beta_i[N];
     for(i in 1:N){
		beta_i[i] <- exp( X[i]'*beta );
        q[i]<-inv_logit(X_M[i]*beta_i[i]+r[i]); # viewing ratio
	}
}
model {
	# likelihood
    for(i in 1:N)
        Y[i]~binomial(minutes[i], q[i]);
	
	#priors:
    beta~multi_normal(delta, V_B); # non-informative prior distribution
    r~normal(0, sigma); # hierarchical prior distribution
    sigma~uniform(0, 1.0e+4); # non-infomative prior distribution

	#hyper-priors:
	delta~multi_normal(mu,sigma0);
	V_B~inv_wishart(N_col, v0);
}
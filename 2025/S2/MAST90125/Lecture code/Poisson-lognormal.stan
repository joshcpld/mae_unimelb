//
// This Stan program defines a Poisson-lognormal regression
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

data {
  int<lower=0> n;   //number of observations
  int<lower=0> P;   //number of parameters
  int<lower=0> y[n];          //response vector
  matrix[n,P] X;     //design matrix (includes intercept)
}


// The parameters accepted by the model. 
// accepts two sets of parameters 'beta', and 'sigma'.
parameters {
  vector[P] beta; //vector of fixed effects of length P.
  vector[n] llambda; //vector of link function.
  real<lower=0> tau; //residual precision
}

transformed parameters {
  real<lower=0> sigma;
sigma = pow(tau, -0.5); //residual standard deviation
}

// The model to be estimated. We model the output
// 'llambda' to be normal with mean X*beta and variance sigma.
// We assume y is Poisson with parameter exp(llambda)
// and  a vague gamma prior for tau = 1/sigma^2.
model {
  llambda ~ normal(X*beta,sigma); //augmented variable
       y ~ poisson(exp(llambda));  //likelihood
     tau ~ gamma(0.001,0.001); //prior
}


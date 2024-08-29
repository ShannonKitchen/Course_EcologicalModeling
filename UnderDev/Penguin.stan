//
// This Stan program defines a simple model, with a
// vector of values 'y' modeled as normally distributed
// with mean 'mu' and standard deviation 'sigma'.
//
// Learn more about model development with Stan at:
//
//    http://mc-stan.org/users/interfaces/rstan.html
//    https://github.com/stan-dev/rstan/wiki/RStan-Getting-Started
//

// The input data is a vector 'y' of length 'N'.
data {
  int<lower=0> N;
  vector[N] y;
  int<lower=1> n_island;
  int<lower=1> n_species;
  int<lower=1> n_sex;
  int<lower=1, upper=n_island> island[N];
  int<lower=1, upper=n_species> species[N];
  int<lower=1, upper=n_sex> sex[N];
  vector[N] billLength;
  vector[N] flipperLength;

}

// The parameters accepted by the model. Our model
// accepts two parameters 'mu' and 'sigma'.
parameters {
 real alpha_mu; //intercept
 real<lower=0> alpha_sig;
 real beta_bill;
 real beta_flipper;
 real beta_e;
 matrix[3, n_sex] u; //sex intercepts, slopes
 vector[n_species] w; //species intercepts, slopes
 // real z; //island
 vector<lower=0>[3] sigma_u; //subj sd
 real<lower=0> sigma_w; //subj sd
 // real<lower=0> sigma_z; //island
}

// The model to be estimated. We model the output
// 'y' to be normally distributed with mean 'mu'
// and standard deviation 'sigma'.
model {
 real mu;
 real sigma_e;
//priors
u[1] ~ normal(0, sigma_u[1]); //sex intercepts
u[2] ~ normal(0, sigma_u[2]);
u[3] ~ normal(0, sigma_u[3]);
w ~ normal(0, sigma_w); //island intercepts
// z ~ normal(0, sigma_z)
 // likelihood
 for (i in 1:N){
   mu = alpha_mu + u[1, sex[i]]  +
   (beta_bill + u[2, sex[i]]) * billLength[i] +
   (beta_flipper + u[3, sex[i]]) * flipperLength[i];
   sigma_e = alpha_sig + beta_e * w[species[i]];
   y[i] ~ lognormal(mu, sigma_e);
 }
}



// data {
// int<lower=1> N; //number of data points
// real rt[N]; //reading time
// real<lower=-1,upper=1> so[N]; //predictor
// int<lower=1> J; //number of subjects
// int<lower=1> K; //number of items
// int<lower=1, upper=J> subj[N]; //subject id
// int<lower=1, upper=K> item[N]; //item id
// }
// parameters {
// vector[2] beta; //intercept and slope
// real<lower=0> sigma_e; //error sd
// matrix[2,J] u; //subj intercepts, slopes
// vector<lower=0>[2] sigma_u; //subj sd
// matrix[2,K] w; //item intercepts, slopes
// vector<lower=0>[2] sigma_w; //item sd
// }
// model {
// real mu;
// //priors
// u[1] ~ normal(0,sigma_u[1]); //subj intercepts
// u[2] ~ normal(0,sigma_u[2]); //subj slopes
// w[1] ~ normal(0,sigma_w[1]); //item intercepts
// w[2] ~ normal(0,sigma_w[2]); //item slopes
// //likelihood
// for (i in 1:N){
// mu = beta[1] + u[1,subj[i]] + w[1,item[i]]
// + (beta[2] + u[2,subj[i]] + w[2,item[i]])*so[i];
// rt[i] ~ lognormal(mu,sigma_e);
// }
// }
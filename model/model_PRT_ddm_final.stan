data {
  int<lower=0> nS; //number of subjects
  int<lower=0> nB; //number of block
  int<lower=0> nT; //number of trials
  int<lower=0> subnum[nT]; //subject number
  int<lower=0> blocknum[nT]; //block of subjects
  real<lower=0> stim[nT]; //stimulus identity (1=rich, 2=lean)
  real rt[nT]; //reaction time
  int<lower=0, upper=1> corr[nT]; //correct or incorrect response
}

parameters {

  //group level prios
  
  // boundary separation
  real<lower=0> mu_alpha[nB]; 
  real<lower=0> sd_alpha;
  
  // drift rate
  real mu_delta[nB];
  real<lower=0> sd_delta;
  
  // non-decision time
  real<lower=0,upper=1> mu_tau[nB]; 
  real<lower=0> sd_tau;
  
  // starting bias
  real<lower=-4,upper=4> mu_zeta[nB]; 
  real<lower=0> sd_zeta;
  
  // subjects level priors
  matrix<lower=0>[nB,nS] alpha1; //rich 
  matrix<lower=0>[nB,nS] alpha2; //lean
  matrix[nB,nS] delta1; //rich
  matrix[nB,nS] delta2; //lean
  matrix<lower=0,upper=1>[nB,nS] tau1; //rich
  matrix<lower=0,upper=1>[nB,nS] tau2; //lean
  matrix<lower=-4,upper=4>[nB,nS] zeta;
}

model {
  
  int currentS;
  int currentB;
  real currentA1;
  real currentA2;
  real currentD1;
  real currentD2;
  real currentT1;
  real currentT2;
  real currentZ;
  
  sd_alpha ~ cauchy(0,2.5);
  sd_delta ~ cauchy(0,2.5);
  sd_tau ~ cauchy(0,0.05);
  sd_zeta ~ cauchy(0,2.5);

  for (iB in 1:nB) {
	mu_alpha[iB] ~ uniform(0.01,5);
	mu_delta[iB] ~ normal(0,10);
	mu_tau[iB] ~ uniform(0.1,1);    
	mu_zeta[iB] ~ uniform(-4,4);
	
	for (iS in 1:nS) {
		alpha1[iB,iS] ~ normal(mu_alpha[iB], sd_alpha)T[0,]; //truncated at zero
		alpha2[iB,iS] ~ normal(mu_alpha[iB], sd_alpha)T[0,]; //truncated at zero
		delta1[iB,iS] ~ normal(mu_delta[iB], sd_delta);
		delta2[iB,iS] ~ normal(mu_delta[iB], sd_delta);
		tau1[iB,iS] ~ normal(mu_tau[iB], sd_tau)T[0,]; //truncated at zero
		tau2[iB,iS] ~ normal(mu_tau[iB], sd_tau)T[0,]; //truncated at zero
		zeta[iB,iS] ~ normal(mu_zeta[iB], sd_zeta);	
	}
  }	

  
  for (iT in 1:nT) {
  
	// get current block and subject
    currentS = subnum[iT];
    currentB = blocknum[iT];
    
    //print("current subject: ", currentS);
	currentA1 = alpha1[currentB,currentS];
	currentA2 = alpha2[currentB,currentS];
	currentD1 = delta1[currentB,currentS];
	currentD2 = delta2[currentB,currentS];
	currentT1 = tau1[currentB,currentS];
	currentT2 = tau2[currentB,currentS];
	currentZ = zeta[currentB,currentS];
    if (stim[iT] == 1) {
      if (corr[iT] == 1) {
        rt[iT] ~ wiener(currentA1, currentT1, Phi(currentZ), currentD1);
      }
      else {
        rt[iT] ~ wiener(currentA1, currentT1, 1-Phi(currentZ), -currentD1);
      }
    }
    else {
      if (corr[iT] == 1) {
        rt[iT] ~ wiener(currentA2, currentT2, 1-Phi(currentZ), currentD2);
      }
      else {
        rt[iT] ~ wiener(currentA2, currentT2, Phi(currentZ), -currentD2);
      }
    }
  } // end of for loop
} // end of model block
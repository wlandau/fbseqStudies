#' @title Function \code{get_hyperparameters}
#' @description internal function
#' @export
#' @return internal object
#' @param counts RNA-seq count dataset
#' @param design design matrix
get_hyperparameters = function(counts,  design) {
  fit = fit_edgeR(counts, design)
  list(eta_1   = mean(fit$phi),
       eta_alpha = mean(fit$alp),
       eta_delta = mean(fit$del),
       eta_psi   = mean(log(fit$disp)),
       sigma_phi   = sd(fit$phi),
       sigma_alpha = sd(fit$alp)/sqrt(2), # Fix sd for Laplace priors
       sigma_delta = sd(fit$del)/sqrt(2), # Fix sd for Laplace priors
       sigma_psi   = sd(log(fit$disp)),
       c = log(fit$norm_factors)) # fit$offset[1,] - mean(fit$offset[1,]))
}

#' @title Function \code{single_gene_analysis}
#' @description Fit rstan to a single gene
#' @export
#' @return internal object
#' @param x RNA-seq counts for a single gene
#' @param group genetic varieties of columns of \code{counts}
#' @param hyperparameters list of hyperparameters provided by \code{get_hyperparameters}
#' @param model compiled \code{rstan} model
single_gene_analysis = function(x, group, hyperparameters, model) {
  diverge = TRUE
  attempt = 1

  while (diverge) {
    print(paste("Attempt", attempt))
    r = sampling(model, 
                 data = c(list(S = length(x), 
                               count   = as.numeric(as.matrix(x)),
                               group = group),
                          hyperparameters),
                 pars = c("phi","alpha","delta","psi","LPH","HPH"),
                 iter = 2000*attempt,
                 thin = attempt)
    s = summary(r)$summary
    diverge = any(s[,"n_eff"] < 1000)
    attempt = attempt + 1
  }
  
  alpha_hat = s[rownames(s) == "alpha","mean"]
  delta_hat = s[rownames(s) == "delta","mean"]
  effectiveSize = 
    (delta_hat >  abs(alpha_hat))*(delta_hat - abs(alpha_hat)) + 
    (delta_hat < -abs(alpha_hat))*(delta_hat + abs(alpha_hat))  
    
  data.frame(
    phi      = s[rownames(s) == "phi",  "mean"],
    alpha    = s[rownames(s) == "alpha","mean"],
    delta    = s[rownames(s) == "delta","mean"],
    psi      = s[rownames(s) == "psi",  "mean"],
    prob_LPH = s[rownames(s) == "LPH",  "mean"],
    prob_HPH = s[rownames(s) == "HPH",  "mean"],
    effectiveSize = effectiveSize)
}

#' @title Function \code{fit_Niemi}
#' @description Fit a dataset using the method in Niemi et al.
#' @export
#' @return \code{rstan} output list
#' @param counts RNA-seq count datset
#' @param group group vector
#' @param design design matrix
#' @param ncores number of cores for parallel execution
fit_Niemi = function(counts, group, design, ncores = 1){
  logs = mysink()
  t = my.proc.time()
  hyperparameters = get_hyperparameters(counts, group)

  model = stan_model(model_code = "data {
    int<lower=1> S;
    int<lower=0> count[S]; 
    int<lower=1,upper=3> group[S];
    real eta_phi;
    real eta_alpha;
    real eta_delta;
    real eta_psi;
    real sigma_phi;
    real sigma_alpha;
    real sigma_delta;
    real sigma_psi;
    vector[S] c;                     // lane sequencing depth
  }
  transformed data {
    matrix[S,3] X;
    for (s in 1:S) {
      if (group[s] == 1) { X[s,1] <- 1; X[s,2] <-  1; X[s,3] <- -1; X[s,4] <-  0; X[s,5] <-  1;}
      if (group[s] == 2) { X[s,1] <- 1; X[s,2] <-  1; X[s,3] <- -1; X[s,4] <-  0; X[s,5] <- -1;}
      if (group[s] == 2) { X[s,1] <- 1; X[s,2] <- -1; X[s,3] <-  1; X[s,4] <-  0; X[s,5] <-  1;}
      if (group[s] == 4) { X[s,1] <- 1; X[s,2] <- -1; X[s,3] <-  1; X[s,4] <-  0; X[s,5] <- -1;}
      if (group[s] == 5) { X[s,1] <- 1; X[s,2] <-  1; X[s,3] <-  1; X[s,4] <-  1; X[s,5] <-  1;}
      if (group[s] == 6) { X[s,1] <- 1; X[s,2] <-  1; X[s,3] <-  1; X[s,4] <-  1; X[s,5] <- -1;}
      if (group[s] == 7) { X[s,1] <- 1; X[s,2] <-  1; X[s,3] <-  1; X[s,4] <- -1; X[s,5] <-  1;}
      if (group[s] == 8) { X[s,1] <- 1; X[s,2] <-  1; X[s,3] <-  1; X[s,4] <- -1; X[s,5] <- -1;}
    }
  }
  parameters {
    real beta_1;
    real beta_2;
    real beta_3;
    real beta_4;
    real beta_5;
    real psi;          
  }
  transformed parameters {
    vector[5] pad;

    pad[1] <- beta_1;
    pad[2] <- beta_2;
    pad[3] <- beta_3;
    pad[4] <- beta_4;
    pad[5] <- beta_5;
  }
  model {
    beta_1  ~ normal(eta_1, sigma_1);
    beta_2 ~ double_exponential(eta_2, sigma_2); // Laplace
    beta_3 ~ double_exponential(eta_3, sigma_3); // Laplace
    beta_4 ~ double_exponential(eta_4, sigma_4); // Laplace
    beta_5 ~ double_exponential(eta_5, sigma_5); // Laplace
    psi   ~ normal(eta_psi, sigma_psi);

    count ~ neg_binomial_2_log(X*pad+c, 1/exp(psi));
  }
  generated quantities {
    int<lower=0, upper=1> LPH;
    int<lower=0, upper=1> HPH;
  
    LPH <- delta < -fabs(alpha);
    HPH <- delta > fabs(alpha);
  }")

  print("About to call single_gene_analysis().")
  registerDoMC(cores = ncores)
  out = adply(as.matrix(counts), 1, 
    function(x){single_gene_analysis(x, group, hyperparameters, model)},
                  .parallel = T,
                  .paropts = list(.export=c("single_gene_analysis", "group", "hyperparameters", "model"), 
                                          .packages='rstan'))
  registerDoSEQ()
  unsink(logs)
  out
}




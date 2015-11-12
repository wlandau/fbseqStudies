#' @title Function \code{get_hyperparameters}
#' @description internal function
#' @export
#' @return internal object
#' @param counts RNA-seq count dataset
#' @param design design matrix
get_hyperparameters = function(counts, design) {
  fit = fit_edgeR(counts, design)
  beta = fit$estimates[,grep("beta_", colnames(fit$estimates))]
  norm_factors = fit$norm_factors

  list(eta_1 = mean(beta[,1]),
        eta_2 = mean(beta[,2]),
        eta_3 = mean(beta[,3]),
        eta_4 = mean(beta[,4]),
        eta_5 = mean(beta[,5]),
        eta_psi = mean(log(norm_factors)),
        sigma_1 = sd(beta[,1]),
        sigma_2 = sd(beta[,2])/sqrt(2), # Fix sd for Laplace priors
        sigma_3 = sd(beta[,3])/sqrt(2), # Fix sd for Laplace priors
        sigma_4 = sd(beta[,4])/sqrt(2), # Fix sd for Laplace priors
        sigma_5 = sd(beta[,5])/sqrt(2), # Fix sd for Laplace priors
        sigma_psi = sd(log(norm_factors)),
        c = log(norm_factors))
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
  pars = c(paste0("beta_", 1:5), "psi", "hph", "lph", "hph1", "lph1", "hph2", "lph2")

  logs = mysink()
  while (diverge) {
    print(paste("Attempt", attempt))
    r = sampling(model, 
                 data = c(list(S = length(x), 
                               count = as.numeric(as.matrix(x)),
                               group = group),
                          hyperparameters),
                 pars = pars,
                 iter = 2000*attempt,
                 thin = attempt)
    s = summary(r)$summary
    diverge = any(s[,"n_eff"] < 1000)
    attempt = attempt + 1
  }
  unsink(logs)  

  data(paschold)
  chain = Chain(get("paschold"))
  chain@betaPostMean = s[paste0("beta_", 1:5), "mean"]
  chain@G = as.integer(1)
  chain@gene_names = character(0)
  effectSizes = effect_sizes(chain)

  out = data.frame(
    attempt = attempt,
    beta_1 = s["beta_1", "mean"],
    beta_2 = s["beta_2", "mean"],
    beta_3 = s["beta_3", "mean"],
    beta_4 = s["beta_4", "mean"],
    beta_5 = s["beta_5", "mean"],
    prob_high_parent_hybrids = s["hph", "mean"],
    prob_low_parent_hybrids  = s["lph", "mean"],
    prob_high_parent_hybrid1 = s["hph1", "mean"],
    prob_low_parent_hybrid1  = s["lph1", "mean"],
    prob_high_parent_hybrid2 = s["hph2", "mean"],
    prob_low_parent_hybrid2  = s["lph2", "mean"],
    effect_high_parent_hybrids = effectSizes[1],
    effect_low_parent_hybrids  = effectSizes[2],
    effect_high_parent_hybrid1 = effectSizes[3],
    effect_low_parent_hybrid1  = effectSizes[4],
    effect_high_parent_hybrid2 = effectSizes[5],
    effect_low_parent_hybrid2  = effectSizes[6])
  out
}

#' @title Function \code{fit_Niemi}
#' @description Fit a dataset using the method in Niemi et al.
#' @export
#' @return \code{rstan} output list
#' @param counts RNA-seq count datset
#' @param design design matrix
#' @param group group vector
#' @param ncores number of cores for parallel execution
fit_Niemi = function(counts, design, group, ncores = 1){
  logs = mysink()
  t = my.proc.time()
  hyperparameters = get_hyperparameters(counts, design)

  model = stan_model(model_code = "data {
    int<lower=1> S;
    int<lower=0> count[S]; 
    int<lower=1,upper=8> group[S];
    real eta_1;
    real eta_2;
    real eta_3;
    real eta_4;
    real eta_5;
    real eta_psi;
    real sigma_1;
    real sigma_2;
    real sigma_3;
    real sigma_4;
    real sigma_5;
    real sigma_psi;
    vector[S] c;                     // lane sequencing depth
  }
  transformed data {
    matrix[S,5] X;
    for (s in 1:S) {
      if (group[s] == 1) { X[s,1] <- 1; X[s,2] <-  1; X[s,3] <- -1; X[s,4] <-  0; X[s,5] <-  1;}
      if (group[s] == 2) { X[s,1] <- 1; X[s,2] <-  1; X[s,3] <- -1; X[s,4] <-  0; X[s,5] <- -1;}
      if (group[s] == 3) { X[s,1] <- 1; X[s,2] <- -1; X[s,3] <-  1; X[s,4] <-  0; X[s,5] <-  1;}
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
    psi ~ normal(eta_psi, sigma_psi);

    count ~ neg_binomial_2_log(X*pad+c, 1/exp(psi));
  }
  generated quantities {
    int<lower=0, upper=1> hph;
    int<lower=0, upper=1> lph;
    int<lower=0, upper=1> hph1;
    int<lower=0, upper=1> lph1;
    int<lower=0, upper=1> hph2;
    int<lower=0, upper=1> lph2;
  
    hph  <- ( beta_2 > 0)                    && ( beta_3 > 0);
    lph   <- (-beta_2 > 0)                    && (-beta_3 > 0);
    hph1 <- (2*beta_2 + beta_4 > 0)  && ( 2*beta_3 + beta_4 > 0);
    lph1 <- (-2*beta_2  - beta_4 > 0)  && (-2*beta_3 - beta_4 > 0);
    hph2 <- (2*beta_2  - beta_4 > 0)  && ( 2*beta_3 - beta_4 > 0);
    lph2 <- (-2*beta_2 + beta_4 > 0)  && (-2*beta_3 + beta_4 > 0);
  }")
  unsink(logs)

  registerDoMC(cores = ncores)
  out = adply(as.matrix(counts), 1, 
    function(x){single_gene_analysis(x, group, hyperparameters, model)},
                     .parallel = T,
                     .paropts = list(.export=c("single_gene_analysis", "group", "hyperparameters", "model"), 
                     .packages='rstan'))
  attempts = out$attempt
  out = out[,colnames(out) != "X1" & colnames(out) != "attempt"]
  registerDoSEQ()
  list(analysis = "Niemi", attempts = attempts, estimates = out, runtime = my.proc.time() - t)
}

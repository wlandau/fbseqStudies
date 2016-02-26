#' @include study-priors_init.R
NULL

#' @title Function \code{priors_mcmc}
#' @description MCMC of the priors study (credible intervals)
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
#' @param zeronormfactors TRUE/FALSE value: option to set normalization constants h to 0 
priors_mcmc = function(path = newdir(), zeronormfactors = T){
  path = newdir(path)
  priors_init(path)
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes", zeronormfactors = zeronormfactors, 
    priors = c("normal", special_beta_priors()[special_beta_priors() != "horseshoe"]))
  path
}

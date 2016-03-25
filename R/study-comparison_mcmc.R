#' @include study-comparison_init.R
NULL

#' @title Function \code{comparison_mcmc}
#' @description MCMC for the comparison study
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
comparison_mcmc = function(path = newdir()){
  path = newdir(path)
  comparison_init(path)
  fit(path, benchmarks = NULL, priors = "normal")
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes", priors = c("normal", special_beta_priors()[special_beta_priors() != "horseshoe"]))  
  fit(path, fbseq_methods = NULL)
  path
}

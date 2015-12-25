#' @include study-real_init.R
NULL

#' @title Function \code{real_mcmc}
#' @description MCMC of the real data analysis
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
real_mcmc = function(path = newdir()){
  path = newdir(path)
  real_init(path)
  fit(path, benchmarks = NULL, priors = "normal")
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes")
  fit(path, fbseq_methods = NULL)
  path
}

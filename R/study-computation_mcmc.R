#' @include study-computation_init.R
NULL

#' @title Function \code{computation_mcmc}
#' @description MCMC of the computation simulation study
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
computation_mcmc = function(path = newdir()){
  path = newdir(path)
  computation_init(path)
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes", priors = "normal")
  path
}

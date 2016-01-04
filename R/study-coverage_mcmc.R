#' @include study-coverage_init.R
NULL

#' @title Function \code{coverage_mcmc}
#' @description MCMC of the coverage study (credible intervals)
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
coverage_mcmc = function(path = newdir()){
  path = newdir(path)
  coverage_init(path)
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes")
  path
}

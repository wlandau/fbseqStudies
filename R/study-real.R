#' @include study-real_analyze.R study-real_init.R
NULL

#' @title Function \code{real}
#' @description workflow of the real data analysis
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
real = function(path = newdir()){
  path = newdir(path)
  real_init(path)
  fit(path, benchmarks = NULL, priors = "normal")
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes")
  real_analyze(path)
  path
}

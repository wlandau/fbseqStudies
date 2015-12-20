#' @include study-computation_analyze.R study-computation_init.R
NULL

#' @title Function \code{computation}
#' @description workflow of the computation study paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
computation = function(path = newdir()){
  path = newdir(path)
  real("real")
  computation_init(path)
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes", priors = "normal")
  computation_analyze(path, paste0("results_", path))
  path
}

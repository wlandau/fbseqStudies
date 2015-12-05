#' @title Function \code{case_coverage_fit}
#' @description Fit for part of the case study paper that looks at the coverage of credible intervals
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
case_coverage_fit = function(path = newdir()){
  path = newdir(path)
  fit(path, priors = "normal", benchmarks = NULL, fbseq_methods = "fullybayes")
}

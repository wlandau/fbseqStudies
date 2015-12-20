#' @include study-coverage_analyze.R study-coverage_init.R
NULL

#' @title Function \code{coverage}
#' @description Workflow for the part of the case study paper that looks at the coverage of credible intervals
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
coverage = function(path = newdir()){
  path = newdir(path)
  coverage_init(path)
  fit(path,  benchmarks = NULL, priors = "normal", fbseq_methods = "fullybayes")
  coverage_analyze(path, paste0("results_", path))
  path
}

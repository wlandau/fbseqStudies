#' @include case_coverage_analyze.R case_coverage_fit.R case_coverage_init.R
NULL

#' @title Function \code{case_coverage}
#' @description Workflow for the part of the case study paper that looks at the coverage of credible intervals
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
case_coverage = function(path = newdir()){
  path = newdir(path)
  case_coverage_init(path)
  case_coverage_fit(path)
  fit(path, fbseq_methods = NULL)
  case_coverage_analyze(path)
  path
}

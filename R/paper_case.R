#' @include study-comparison_mcmc.R study-coverage_mcmc.R study-real_mcmc.R
NULL

#' @title Function \code{case}
#' @description Workflow for the case study paper
#' @export
paper_case = function(){
  path = real_init("real_mcmc")
  fit(path, benchmarks = NULL, priors = "normal")
  real_analyze(path, "real_analyze")
  coverage_mcmc("coverage_mcmc")
  coverage_analyze("coverage_mcmc", "coverage_analyze")
  path = comparison_init("comparison_mcmc")
  fit(path, benchmarks = NULL, priors = "normal")
  comparison_analyze(path, "comparison_analyze")
}

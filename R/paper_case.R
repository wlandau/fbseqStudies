#' @include study-comparison.R study-coverage.R study-real.R
NULL

#' @title Function \code{case}
#' @description Workflow for the case study paper
#' @export
paper_case = function(){
  path = real_init("real")
  fit(path, benchmarks = NULL, priors = "normal")
  real_analyze(path, paste0("results_", path))
  coverage("coverage")
  path = comparison_init("comparison")
  fit(path, benchmarks = NULL, priors = "normal")
  comparison_analyze(path, paste0("results_", path))
}

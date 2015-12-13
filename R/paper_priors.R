#' @include study-comparison.R study-real.R
NULL

#' @title Function \code{paper_priors}
#' @description Workflow for the priors comparison study paper
#' @export
paper_priors = function(){
  path = real_init("real")
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes")
  real_analyze(path)
  path = comparison_init("comparison")
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes")
  comparison_analyze(path)
}

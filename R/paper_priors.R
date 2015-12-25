#' @include study-comparison_mcmc.R study-real_mcmc.R
NULL

#' @title Function \code{paper_priors}
#' @description Workflow for the priors comparison study paper
#' @export
paper_priors = function(){
  path = real_init("real_mcmc")
  fit(path, fbseq_methods = "fullybayes")
  real_analyze(path, "real_analyze")
  path = comparison_init("comparison_analyze")
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes")
  comparison_analyze(path, "comparison_analyze")
}

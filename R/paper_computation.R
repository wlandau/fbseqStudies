#' @include study-computation_mcmc.R study-real_mcmc.R 
NULL

#' @title Function \code{paper_computation}
#' @description Workflow for the computation paper
#' @export
paper_computation = function(){
  path = real_init("real_mcmc")
  fit(path, benchmarks = NULL, priors = "normal", fbseq_methods = "fullybayes")
  computation_mcmc("computation_mcmc")
  real_analyze(path, "real_analyze")
  computation_analyze("computation_mcmc", "computation_analyze")
}

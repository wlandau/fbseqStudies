#' @include study-computation.R study-real.R 
NULL

#' @title Function \code{paper_computation}
#' @description Workflow for the computation paper
#' @export
paper_computation = function(){
  path = real_init("real")
  fit(path, benchmarks = NULL, priors = "normal", fbseq_methods = "fullybayes")
  real_analyze(path, paste0("results_", path))
  computation("computation")
}

#' @include study-comparison_analyze.R study-comparison_init.R
NULL

#' @title Function \code{comparison}
#' @description Workflow for the comparison study paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
comparison = function(path = newdir()){
  path = newdir(path)
  comparison_init(path)
  fit(path, benchmarks = NULL, priors = "normal")
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes")  
  fit(path, fbseq_methods = NULL)
  comparison_analyze(path, paste0("results_", path))
  path
}

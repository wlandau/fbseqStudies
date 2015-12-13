#' @include priors_analyze.R priors_fit.R priors_init.R
NULL

#' @title Function \code{priors}
#' @description Workflow for the priors study paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
priors = function(path = newdir()){
  path = newdir(path)
  real("real")
  priors_init(path)
  priors_fit(path)
  fit(path, fbseq_methods = NULL)
  priors_analyze(path)
  path
}

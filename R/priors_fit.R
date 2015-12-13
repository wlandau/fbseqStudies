#' @title Function \code{priors_fit}
#' @description Fit for part of the case study paper that looks at gene detection priors
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
priors_fit = function(path = newdir()){
  path = newdir(path)
  fit(path, fbseq_methods = "fullybayes", benchmarks = NULL)
}

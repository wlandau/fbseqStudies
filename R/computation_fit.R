#' @title Function \code{computation_fit}
#' @description Run the main methods on the simulated data
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
computation_fit = function(path = newdir()){
  path = newdir(path)
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes", priors = "normal")
  path
}

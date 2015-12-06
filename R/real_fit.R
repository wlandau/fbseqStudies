#' @title Function \code{real_fit}
#' @description Run the main methods on the real data analysis
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
real_fit = function(path = newdir()){
  path = newdir(path)
  fit(path, benchmarks = NULL, priors = "normal")
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes")
  path
}

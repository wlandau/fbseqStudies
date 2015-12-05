#' @title Function \code{case_power_fit}
#' @description Fit for part of the case study paper that looks at gene detection power
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
case_power_fit = function(path = newdir()){
  path = newdir(path)
  fit(path, priors = "normal", benchmarks = NULL)
}

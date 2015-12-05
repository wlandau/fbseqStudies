#' @title Function \code{case_power_analyze}
#' @description Plot and summarize results from simulation study
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
case_power_analyze = function(path = newdir()){
  path = newdir(path)
  gelman(path)
  path
}

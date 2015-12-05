#' @title Function \code{power_analyze}
#' @description Plot and summarize results from comparison study
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
comparison_analyze = function(path = newdir()){
  path = newdir(path)
  gelman(path)
  path
}

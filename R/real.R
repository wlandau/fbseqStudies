#' @include real_analyze.R real_fit.R real_init.R
NULL

#' @title Function \code{real}
#' @description workflow of the real data analysis
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
real = function(path = newdir()){
  path = newdir(path)
  real_init(path)
  real_fit(path)
  real_analyze(path)
  path
}

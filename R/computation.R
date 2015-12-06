#' @include computation_analyze.R computation_fit.R computation_init.R
NULL

#' @title Function \code{computation}
#' @description workflow of the computation study paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
computation = function(path = newdir()){
  path = newdir(path)
  real("real")
  computation_init(path)
  computation_fit(path)
  computation_analyze(path)
  path
}

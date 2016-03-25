#' @title Function \code{priors_analyze}
#' @description Plot and summarize results from simulation studies
#' @export
#' @param from to directory to save simulations and results
#' @param to output directory
priors_analyze = function(from, to){
  from = newdir(from)
  to = newdir(to)
  comparison_analyze(from, to)
}

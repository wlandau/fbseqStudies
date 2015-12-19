#' @title Function \code{computation_analyze}
#' @description Plot and summarize results from simulation study
#' @export
#' @param from to directory to save simulations and results
#' @param to output directory
computation_analyze = function(from, to){
  path = newdir(path)
  from = newdir(from)
  to = newdir(to)
  gelman(from, paste0(to, "gelman"))
  ess(from, paste0(to, "ess"))
  runtime(from, paste0(to, "runtime"))
}

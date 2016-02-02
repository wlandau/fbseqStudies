#' @title Function \code{real_analyze}
#' @description Plot and summarize results from the real data analysis
#' @export
#' @param from to directory to save simulations and results
#' @param to output directory
real_analyze = function(from, to){
  computation_analyze(from, to)
  explore_real(from, paste0(to, "explore_real"))
  comparehprobs(from, paste0(to, "comparehprobs"))
}

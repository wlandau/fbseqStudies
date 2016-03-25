#' @title Function \code{computation_analyze}
#' @description Plot and summarize results from simulation study
#' @export
#' @param from to directory to save simulations and results
#' @param to output directory
computation_analyze = function(from, to){
  from = newdir(from)
  to = newdir(to)
  gelman(from, paste0(to, "gelman"))
  ess(from, paste0(to, "ess"))
  runtime(from, paste0(to, "runtime"))
  tryCatch({
    plot_runtime(paste0(to, "runtime"), paste0(to, "plot_runtime"))
  }, error = function(e) print("Could not plot runtime."))
}

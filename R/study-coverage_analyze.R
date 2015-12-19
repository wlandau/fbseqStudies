#' @title Function \code{coverage_analyze}
#' @description Plot and summarize results from simulation study
#' @export
#' @param from to directory to save simulations and results
#' @param to output directory
coverage_analyze = function(from, to){
  from = newdir(from)
  to = newdir(to)
  gelman(from, paste0(to, "gelman"))
  ess(from, paste0(to, "ess"))
  ci_hyper(from, paste0(to, "ci_hyper"))
  ci_ci_beta(from, paste0(to, "ci_beta"))
  roc(from, paste0(to, "roc"))
  plot_roc(from, paste0(to, "plot_roc"))
  calibration(from, paste0(to, "calibration"))
  plot_calibration(from, paste0(to, "plot_calibration"))
}

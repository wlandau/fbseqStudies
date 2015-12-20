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
  ci_hyper_list(from, paste0(to, "ci_hyper_list"), 0.5)
  ci_hyper(paste0(to, "ci_hyper_list"), paste0(to, "ci_hyper"))
  ci_beta_list(from, paste0(to, "ci_beta_list"), level = 0.5)
  ci_beta_list(from, paste0(to, "ci_beta_list"), level = 0.95)
  ci_beta(paste0(to, "ci_beta_list"), paste0(to, "ci_beta"))
  rocs(from, paste0(to, "roc"))
  plot_roc(paste0(to, "roc"), paste0(to, "plot_roc"))
  calibrations(from, paste0(to, "calibration"))
  plot_calibration(paste0(to, "calibration"), paste0(to, "plot_calibration"))
}

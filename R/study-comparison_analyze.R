#' @title Function \code{comparison_analyze}
#' @description Plot and summarize results from simulation studies
#' @export
#' @param from to directory to save simulations and results
#' @param to output directory
comparison_analyze = function(from, to){
  from = newdir(from)
  to = newdir(to)
  gelman(from, paste0(to, "gelman"))
  ess(from, paste0(to, "ess"))
  ci_hyper(from, paste0(to, "ci_hyper"))
  plot_ci_hyper(paste0(to, "ci_hyper"), paste0(to, "plot_ci_hyper"))
  ci_beta(from, paste0(to, "ci_beta"))
  plot_ci_beta(paste0(to, "ci_beta"), paste0(to, "plot_ci_beta"))
  rocs(from, paste0(to, "roc"))
  plot_roc(paste0(to, "roc"), paste0(to, "plot_roc"))
  plot_auc(paste0(to, "roc"), paste0(to, "plot_auc"))
  calibrations(from, paste0(to, "calibration"))
  plot_calibration(paste0(to, "calibration"), paste0(to, "plot_calibration"))
}

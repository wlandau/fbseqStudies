#' @title Function \code{comparison_analyze}
#' @description Plot and summarize results from comparison study
#' @export
#' @param from to directory to roc information
#' @param to output directory
comparison_analyze = function(from, to){
  from = newdir(from)
  to = newdir(to)
  gelman(from, paste0(to, "gelman"))
  ess(from, paste0(to, "ess"))
  rocs(from, paste0(to, "roc"))
  plot_roc(paste0(to, "roc"), paste0(to, "plot_roc"))
  auc_df(paste0(to, "roc"), paste0(to, "auc"))
  auc_df(paste0(to, "roc"), paste0(to, "auc"))
  calibrations(from, paste0(to, "calibration"))
  plot_calibration(paste0(to, "calibration"), paste0(to, "plot_calibration"))
}

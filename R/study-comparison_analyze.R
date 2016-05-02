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
  runtime(from, paste0(to, "runtime"))
  mse(from, paste0(to, "mse"))
  ci(from, paste0(to, "ci"))
  rocs(from, paste0(to, "roc"))
  roc_long(paste0(to, "roc"), paste0(to, "roc_long"))
  auc_long(paste0(to, "roc"), paste0(to, "auc_long"))
  calibrations(from, paste0(to, "calibration"))
  calibration_long(paste0(to, "calibration"), paste0(to, "calibration_long"))
}

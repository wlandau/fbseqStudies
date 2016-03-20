#' @title Function \code{calibration_long}
#' @description plot calibration curves using rds files extracted from simulation lists
#' @export
#' @param from directory with calibration information
#' @param to directory to save plots
calibration_long = function(from, to){
  from = newdir(from)
  to = newdir(to)
  df = ggplot2_df(from)
  saveRDS(df, paste0(to, "calibration_long.rds"))
}

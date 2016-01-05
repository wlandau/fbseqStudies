#' @include analyze-plot_calibration_df.R
NULL

#' @title Function \code{plot_calibration}
#' @description plot calibration curves using rds files extracted from simulation lists
#' @export
#' @param from directory with calibration information
#' @param to directory to save plots
#' @param analysis analysis methods to plot
#' @param reps reps to plot
plot_calibration = function(from, to, analysis = analyses(), reps = 1:10){
  from = newdir(from)
  to = newdir(to)
  df = ggplot2_df(from)
  reps = unique(df$rep)
  plot_calibration_df(df = df, to = to, analysis = analysis, reps = reps)
  if(length(reps) > 1) for(r in reps) plot_calibration_df(df = df, to = to, analysis = analysis, reps = r)
}

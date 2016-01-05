#' @include analyze-plot_roc_df.R
NULL

#' @title Function \code{plot_roc}
#' @description plot roc curves using rds files extracted from simulation lists
#' @export
#' @param from directory with roc information
#' @param to directory to save plots
#' @param cutoff for fpr
#' @param analysis analysis methods to plot
#' @param reps reps to plot
plot_roc = function(from, to, cutoff = 0.1, analysis = analyses(), reps = 1:10){
  from = newdir(from)
  to = newdir(to)
  df = ggplot2_df(from)
  reps = unique(df$rep)
  plot_roc_df(df = df, to = to, cutoff = cutoff, analysis = analysis, reps = reps)
  if(length(reps) > 1) for(r in reps) plot_roc_df(df = df, to = to, cutoff = cutoff, analysis = analysis, reps = r)
}

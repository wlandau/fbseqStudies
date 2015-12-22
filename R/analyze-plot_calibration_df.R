#' @include analyze-mytheme.R util-myrelevel.R
NULL

#' @title Function \code{plot_calibration_df}
#' @description plot posterior probability calibration curves using rds files extracted from simulation lists
#' @export
#' @param df output of ggplot2_df
#' @param to directory to save plots
#' @param analysis analysis methods to plot
#' @param reps reps to plot
plot_calibration_df = function(df, to, analysis = analyses(), reps = 1:10){
  to = newdir(to)
  df = df[df$analysis %in% analysis,]
  df = df[as.integer(df$rep) %in% reps,]
  df$analysis = myrelevel(df$analysis)
  for(h in levels(df$heterosis)){
    d = df[df$heterosis == h,]
    pl = ggplot(d) + mytheme() + 
      geom_line(aes_string(x = "probability", y = "proportion", group = "file", color = "analysis", linetype = "analysis")) + 
      geom_abline(slope = 1, intercept = 0, alpha = 0.25) + 
      facet_grid(libraries ~ simulation)
    ggsave(paste0(to, h, "_", paste0(reps, collapse = "_"), ".pdf"), pl, height = 8, width = 8)
  }
}

#' @include analyze-mytheme.R util-myrelevel.R
NULL

#' @title Function \code{plot_roc_df}
#' @description plot roc curves using rds files extracted from simulation lists
#' @export
#' @param df output of ggplot2_df
#' @param to directory to save plots
#' @param cutoff for fpr
#' @param analysis analysis methods to plot
#' @param reps reps to plot
plot_roc_df = function(df, to, cutoff = 0.1, analysis = analyses(), reps = 1:10){
  to = newdir(to)
  df = df[df$fpr < cutoff,]
  df = df[df$analysis %in% analysis,]
  df = df[as.integer(df$rep) %in% reps,]
  df$analysis = myrelevel(df$analysis)
  for(h in levels(df$heterosis)){
    d = df[df$heterosis == h,]
    pl = ggplot(d) + mytheme() + xlim(c(0, cutoff)) + 
      geom_line(aes_string(x = "fpr", y = "tpr", group = "file", color = "analysis", linetype = "analysis")) + 
      geom_abline(slope = 1, intercept = 0, alpha = 0.25) + 
      facet_grid(libraries ~ simulation)
    suppressMessages(ggsave(paste0(to, h, "_",  paste0(reps, collapse = "_"), ".pdf"), pl))
  }
}

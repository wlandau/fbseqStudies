#' @include util-relevel_analyses.R util-relevel_simulations.R util-relevel_heterosis.R util-mytheme.R
NULL

#' @title Function \code{plot_roc_df}
#' @description plot roc curves using rds files extracted from simulation lists
#' @export
#' @param df output of ggplot2_df
#' @param to directory to save plots
#' @param cutoff for fpr
#' @param analysis analysis methods to plot
#' @param reps reps to plot
plot_roc_df = function(df, to, cutoff = 1, analysis = analyses(), reps = 1:10){
  to = newdir(to)
  df = df[df$fpr < cutoff,]
  df = df[as.integer(df$rep) %in% reps,]
  df$analysis = relevel_analyses(df$analysis)
  df$simulation = relevel_simulations(df$simulation)
  df$heterosis = relevel_heterosis(df$heterosis)
  df = df[df$analysis %in% analysis,]
  for(h in unique(df$heterosis)){
    d = df[df$heterosis == h,]
    pl = ggplot(d) + mytheme() + xlim(c(0, cutoff)) + 
      geom_line(aes_string(x = "fpr", y = "tpr", group = "file", color = "analysis", linetype = "analysis")) + 
      geom_abline(slope = 1, intercept = 0, alpha = 0.25) + 
      facet_grid(libraries ~ simulation)
    ggsave(paste0(to, h, "_",  paste0(reps, collapse = "_"), ".pdf"), pl, height = 8, width = 8)
  }
}

#' @include analyze-mytheme.R util-myrelevel.R
NULL

#' @title Function \code{plot_roc}
#' @description plot roc curves using rds files extracted from simulation lists
#' @export
#' @param from directory with roc information
#' @param to directory to save plots
#' @param cutoff for fpr
#' @param analysis analysis methods to plot
plot_roc = function(from, to, cutoff = 0.1, analysis = analyses()){
  from = newdir(from)
  to = newdir(to)
  df = ggplot2_df(from)
  df = df[df$fpr < cutoff,]
  df = df[df$analysis %in% analysis,]
  df$analysis = myrelevel(df$analysis)
  for(h in levels(df$heterosis)){
    d = df[df$heterosis == h,]
    pl = ggplot(d) + mytheme() + xlim(c(0, cutoff)) + 
      geom_line(aes_string(x = "fpr", y = "tpr", group = "file", color = "analysis", linetype = "analysis")) + 
      geom_abline(slope = 1, intercept = 0, alpha = 0.25) + 
      facet_grid(libraries ~ simulation)
    suppressMessages(ggsave(paste0(to, h, ".pdf"), pl))
  }
}

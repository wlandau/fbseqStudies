#' @include analyze-mytheme.R
NULL

#' @title Function \code{plot_roc}
#' @description plot roc curves using rds files extracted from simulation lists
#' @export
#' @param from directory with roc information
#' @param to directory to save plots
plot_roc = function(from, to){
  from = newdir(from)
  to = newdir(to)
  df = ggplot2_df(from)

  for(h in levels(df$heterosis)){
    d = df[df$heterosis == h,]
    pl = ggplot(d) + mytheme() + xlim(c(0, 0.1)) + 
      geom_line(aes_string(x = "fpr", y = "tpr", group = "file", color = "analysis", linetype = "analysis")) + 
      geom_abline(slope = 1, intercept = 0, alpha = 0.25) + 
      facet_grid(libraries ~ simulation)
    suppressMessages(ggsave(paste0(to, h, ".pdf"), pl))
  }
}

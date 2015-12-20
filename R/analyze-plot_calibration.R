#' @title Function \code{plot_calibration}
#' @description plot posterior probability calibration curves using rds files extracted from simulation lists
#' @export
#' @param from directory of extracted results files
#' @param to directory to save plots
#' @param analysis analysis methods to plot
plot_calibration = function(from, to, analysis = analyses()){
  from = newdir(from)
  to = newdir(to)
  df = ggplot2_df(from)
  df = df[df$analysis %in% analysis,]
  for(h in levels(df$heterosis)){
    d = df[df$heterosis == h,]
    pl = ggplot(d) + mytheme() + 
      geom_line(aes_string(x = "probability", y = "proportion", group = "file", color = "analysis", linetype = "analysis")) + 
      geom_abline(slope = 1, intercept = 0, alpha = 0.25) + 
      facet_grid(libraries ~ simulation)
    suppressMessages(ggsave(paste0(to, h, ".pdf"), pl))
  }
}

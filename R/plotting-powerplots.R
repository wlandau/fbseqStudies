#' @title Function \code{powerplots}
#' @description plot results in power study using rds files extracted from simulation lists
#' @export
#' @param from directory of extracted results files
#' @param to directory to save plots
powerplots = function(from, to){
  from = newdir(from)
  to = newdir(to)
  df = ggplot2_df(from)
  t = theme(axis.text = element_text(family = "Helvetica", colour = 'black'),
                      panel.background = element_rect(fill='white'),
                      panel.border = element_rect(color="black", fill = NA),
                      panel.grid.major = element_line(color="lightgray"),
                      text = element_text(family = "Helvetica", colour= "black"))

  if("tpr" %in% colnames(df)){
    xvar = "fpr"
    yvar = "tpr"
  } else if ("fdr" %in% colnames(df)) {
    xvar = "fdr"
    yvar = "fdp"
  }

  for(h in levels(df$heterosis)){
    d = df[df$heterosis == h,]
    pl = ggplot(d) + t + 
      geom_line(aes_string(x = xvar, y = yvar, group = "file", color = "analysis", linetype = "analysis")) + 
      geom_abline(slope = 1, intercept = 0, alpha = 0.25) + 
      facet_grid(libraries ~ simulation)
    suppressMessages(ggsave(paste0(to, h, ".pdf"), pl))
  }
}

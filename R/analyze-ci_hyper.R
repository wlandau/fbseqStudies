#' @include analyze-mytheme.R
NULL

#' @title Function \code{ci_hyper}
#' @description plot roc curves using rds files extracted from simulation lists
#' @export
#' @param from directory containing ci information
#' @param to directory to save plots
ci_hyper = function(from, to){
  from = newdir(from)
  l = readRDS(paste0(from, "ci_hyper_list_0.5.rds"))
  to = newdir(to)
  for(n in rownames(l$truth)){
    d = data.frame(lower = l$lower[n,] - l$truth[n,], upper = l$upper[n,] - l$truth[n,], rep = 1:length(l$truth[n,]))
    pl = ggplot(d) + mytheme() + 
      geom_segment(aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
      geom_abline(slope = 0, intercept = 0)
    ggsave(paste0(to, n, ".pdf"), pl)
  }
  out = rowMeans(l$lower < l$truth & l$truth < l$upper)
  saveRDS(out, paste0(to, "coverage.rds"))
}

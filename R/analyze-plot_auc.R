#' @include analyze-mytheme.R
NULL

#' @title Function \code{plot_auc}
#' @description plot areas under roc curves using rds files extracted from simulation lists
#' @export
#' @param from directory of extracted results files
#' @param to directory to save plots
plot_auc = function(from, to){
  from = newdir(from)
  to = newdir(to)
  df = auc_df(from)
  write.table(df, file = paste0(to, "auc.txt"), row.names = F)
  df$group = paste(df$genes, df$libraries, df$rep, sep = "_")
  cutoffs = colnames(df)[grepl("auc_", colnames(df))]
  for(h in levels(df$heterosis)) for(cutoff in cutoffs){
    d = df[df$heterosis == h,]
    pl = ggplot(d) + mytheme() + 
      geom_line(aes_string(x = "analysis", y = cutoff, group = "group")) +
      facet_grid(libraries ~ simulation)
    suppressMessages(ggsave(paste0(to, h, "_", cutoff, ".pdf"), pl))
  }
}

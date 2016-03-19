#' @include util-relevel_analyses.R util-relevel_simulations.R util-relevel_heterosis.R util-mytheme.R
NULL

#' @title Function \code{plot_auc}
#' @description plot areas under roc curves using rds files extracted from simulation lists
#' @export
#' @param from directory of extracted results files
#' @param to directory to save plots
#' @param analysis analysis methods to plot
plot_auc = function(from, to, analysis = analyses()){
  from = newdir(from)
  to = newdir(to)
  df = auc_df(from)
  saveRDS(df, paste0(to, "auc.rds"))
  df = df[df$analysis %in% analysis,]
  df$analysis = relevel_analyses(df$analysis)
  df$simulation = relevel_simulations(df$simulation)
  df$heterosis = relevel_heterosis(df$heterosis)
  df$group = paste(df$genes, df$libraries, df$rep, sep = "_")
  cutoffs = colnames(df)[grepl("auc_", colnames(df))]
  for(h in levels(df$heterosis)) for(cutoff in cutoffs){
    d = df[df$heterosis == h,]
    pl = ggplot(d) + mytheme() + 
      geom_line(aes_string(x = "analysis", y = cutoff, group = "group")) +
      geom_point(aes_string(x = "analysis", y = cutoff, shape = 4)) + 
      scale_shape_identity() +
      facet_grid(libraries ~ simulation)
    ggsave(paste0(to, h, "_", cutoff, ".pdf"), pl, height = 8, width = 8)
  }
}

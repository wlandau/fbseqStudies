#' @include util-relevel_analyses.R util-relevel_simulations.R util-relevel_heterosis.R util-mytheme.R
NULL

#' @title Function \code{auc_long}
#' @description plot areas under roc curves using rds files extracted from simulation lists
#' @export
#' @param from directory of extracted results files
#' @param to directory to save plots
auc_long = function(from, to){
  from = newdir(from)
  to = newdir(to)
  df = auc_df(from)
  saveRDS(df, paste0(to, "auc_long.rds"))
}

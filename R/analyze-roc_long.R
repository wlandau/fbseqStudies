#' @title Function \code{roc_long}
#' @description plot roc curves using rds files extracted from simulation lists
#' @export
#' @param from directory with roc information
#' @param to directory to save plots
roc_long = function(from, to){
  from = newdir(from)
  to = newdir(to)
  df = ggplot2_df(from)
  saveRDS(df, paste0(to, "roc_long.rds"))
}

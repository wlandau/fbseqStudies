#' @title Function \code{analyses}
#' @description analysis to show in plots
#' @export
#' @return analysis analyses
analyses = function(){
  out = c("fully Bayes", "eBayes (Oracle)", "eBayes (Means)")
  ordered(out, levels = out)
}

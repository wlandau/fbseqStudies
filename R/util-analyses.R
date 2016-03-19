#' @title Function \code{analyses}
#' @description analysis to show in plots
#' @export
#' @return analysis analyses
analyses = function(){
  out = c("eBayes (Oracle)", "eBayes (Means)", "fully Bayes")
  ordered(out, levels = out)
}

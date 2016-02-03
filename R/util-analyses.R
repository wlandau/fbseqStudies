#' @title Function \code{analyses}
#' @description analysis analyses
#' @export
#' @return analysis analyses
analyses = function(){
  out = c(
     "ebayesFromTruth+normal", "ebayesFromStarts+normal", "ebayesFromFullybayes+normal",
    "fullybayes+normal", "fullybayes+Laplace", "fullybayes+t", "fullybayes+horseshoe", "edgeR", "Niemi")
  ordered(out, levels = out)
}

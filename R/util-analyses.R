#' @title Function \code{analyses}
#' @description analysis analyses
#' @export
#' @return analysis analyses
analyses = function(){
  out = c("edgeR", "Niemi", 
     "ebayesFromTruth+normal", "ebayesFromStarts+normal", "ebayesFromFullybayes+normal",
    "fullybayes+normal", "fullybayes+Laplace", "fullybayes+t", "fullybayes+horseshoe")
  ordered(out, levels = out)
}

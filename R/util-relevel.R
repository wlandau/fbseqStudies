#' @title Function \code{relevel}
#' @description relevel analysis vector for plotting labels
#' @export
#' @param x factor to relevel
#' @return releveled factor
relevel = function(x){
  out = as.character(x)
  out["ebayesFromTruth+normal"] = "eBayes (oracle)"
  out["ebayesFromStarts+normal"] = "eBayes (naive)"
  out["ebayesFromFullybayes+normal"] = "eBayes (posterior)"
  out["fullybayes+normal"] = "fully Bayes (normal)"
  out["fullybayes+Laplace"] = "fully Bayes (Laplace)"
  out["fullybayes+t"] = "fully Bayes (t)"
  out["fullybayes+horseshoe"] = "fully Bayes (horseshoe)"
  ordered(out, levels = out)
}

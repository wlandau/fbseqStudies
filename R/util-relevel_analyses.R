#' @title Function \code{relevel_analyses}
#' @description relevel analysis vector for plotting labels
#' @export
#' @param x factor to relevel
#' @return releveled factor
relevel_analyses = function(x){
  out = as.character(x)
  out[out == "ebayesFromTruth+normal"] = "eBayes (Oracle)"
  out[out == "ebayesFromStarts+normal"] = "eBayes (Naive)"
  out[out == "ebayesFromFullybayes+normal"] = "eBayes (Means)"
  out[out == "fullybayes+normal"] = "fully Bayes"
  out[out == "fullybayes+Laplace"] = "fully Bayes (Laplace)"
  out[out == "fullybayes+t"] = "fully Bayes (t)"
  out[out == "fullybayes+horseshoe"] = "fully Bayes (horseshoe)"
  ordered(out, levels = c(
    "eBayes (Oracle)", 
    "eBayes (Naive)",
    "eBayes (Means)",
    "fully Bayes",
    "fully Bayes (Laplace)",
    "fully Bayes (t)",
    "fully Bayes (horseshoe)",
    "edgeR",
    "Niemi"))
}

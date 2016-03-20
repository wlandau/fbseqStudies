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
    "fully Bayes",
    "fully Bayes (Laplace)",
    "fully Bayes (t)",
    "fully Bayes (horseshoe)",
    "edgeR",
    "Niemi",
    "eBayes (Naive)",
    "eBayes (Means)",
    "eBayes (Oracle)"))
}

#' @title Function \code{priors_relevel_analyses}
#' @description relevel analysis vector for plotting labels
#' @export
#' @param x factor to relevel
#' @return releveled factor
priors_relevel_analyses = function(x){
  out = as.character(x)
  out[out == "ebayesFromTruth+normal"] = "eBayes (Oracle)"
  out[out == "ebayesFromStarts+normal"] = "eBayes (Naive)"
  out[out == "ebayesFromFullybayes+normal"] = "eBayes (Means)"
  out[out == "fullybayes+normal"] = "normal"
  out[out == "fullybayes+Laplace"] = "Laplace"
  out[out == "fullybayes+t"] = "t"
  out[out == "fullybayes+horseshoe"] = "horseshoe"
  ordered(out, levels = c(
    "normal",
    "Laplace",
    "t",
    "horseshoe",
    "edgeR",
    "Niemi",
    "eBayes (Naive)",
    "eBayes (Means)",
    "eBayes (Oracle)"))
}

#' @title Function \code{analyses}
#' @description analysis to show in plots
#' @export
#' @return analysis analyses
analyses = function(){
  out =  c(
    "fully Bayes",
    "fully Bayes (t)",
    "fully Bayes (Laplace)",
    "fully Bayes (horseshoe)",
    "edgeR",
 #   "independence",
    "Niemi",
    "eBayes (Naive)",
    "eBayes (Means)",
    "eBayes (Oracle)")
  ordered(out, levels = out)
}

#' @title Function \code{case_analyses}
#' @description analysis to show in plots (case study)
#' @export
#' @return analysis analyses
case_analyses = function(){
  out = c("fully Bayes", # "independence", 
    "eBayes (Means)", "eBayes (Oracle)")
  ordered(out, levels = out)
}

#' @title Function \code{analyses_priors}
#' @description analysis to show in plots for priors study
#' @export
#' @return analysis analyses
priors_analyses = function(){
  out = c("normal", "t", "Laplace", "horseshoe")
  ordered(out, levels = out)
}

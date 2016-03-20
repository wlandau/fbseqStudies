#' @title Function \code{analyses}
#' @description analysis to show in plots
#' @export
#' @return analysis analyses
analyses = function(){
  out =  c(
    "fully Bayes",
    "fully Bayes (Laplace)",
    "fully Bayes (t)",
    "fully Bayes (horseshoe)",
    "eBayes (Oracle)", 
    "eBayes (Naive)",
    "eBayes (Means)",
    "edgeR",
    "Niemi")
  ordered(out, levels = out)
}

#' @title Function \code{case_analyses}
#' @description analysis to show in plots (case study)
#' @export
#' @return analysis analyses
case_analyses = function(){
  out = c("fully Bayes", "eBayes (Oracle)", "eBayes (Means)")
  ordered(out, levels = out)
}

#' @title Function \code{analyses_priors}
#' @description analysis to show in plots for priors study
#' @export
#' @return analysis analyses
priors_analyses = function(){
  out = c("fully Bayes", "fully Bayes (Laplace)", "fully Bayes (t)")
  ordered(out, levels = out)
}

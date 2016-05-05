#' @include study-comparison_mcmc.R study-coverage_mcmc.R study-real_mcmc.R study-serial_mcmc.R
NULL

#' @title Function \code{paper_case}
#' @description Workflow for the case study paper
#' @export
paper_case = function(){
  real_init("real_mcmc")
  fit("real_mcmc", priors = "normal", fbseq_methods = c("fullybayes", "ibayes", "ebayesFromFullybayes"))
  coverage_init("coverage_mcmc")
  fit("coverage_mcmc", benchmarks = "edgeR", zeronormfactors = T, fbseq_methods = "fullybayes", priors = "normal")
  coverage_init("coverage_norm_mcmc")
  fit("coverage_norm_mcmc", benchmarks = "edgeR", zeronormfactors = F, fbseq_methods = "fullybayes", priors = "normal")
  comparison_init("comparison_mcmc")
  fit("comparison_mcmc", benchmarks = "edgeR", priors = "normal")
  serial_mcmc("serial_mcmc")

  real_analyze("real_mcmc", "real_analyze")
  coverage_analyze("coverage_mcmc", "coverage_analyze")
  coverage_analyze("coverage_norm_mcmc", "coverage_norm_analyze")
  comparison_analyze("comparison_mcmc", "comparison_analyze")

  paper_case_figures()
}

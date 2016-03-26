#' @include study-comparison_mcmc.R study-coverage_mcmc.R study-real_mcmc.R
NULL

#' @title Function \code{paper_case}
#' @description Workflow for the case study paper
#' @export
paper_case = function(){
  path = real_init("real_mcmc")
  fit(path, priors = "normal", fbseq_methods = "fullybayes")
  coverage_init("coverage_mcmc")
  fit("coverage_mcmc", benchmarks = NULL, zeronormfactors = T, fbseq_methods = "fullybayes", priors = "normal")
  coverage_init("coverage_norm_mcmc")
  fit("coverage_norm_mcmc", benchmarks = NULL, zeronormfactors = F, fbseq_methods = "fullybayes", priors = "normal")
  path = comparison_init("comparison_mcmc")
  fit(path, benchmarks = NULL, priors = "normal")

  real_analyze(path, "real_analyze")
  coverage_analyze("coverage_mcmc", "coverage_analyze")
  coverage_analyze("coverage_norm_mcmc", "coverage_norm_analyze")
  comparison_analyze(path, "comparison_analyze")

  paper_case_figures()
}

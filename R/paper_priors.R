#' @include study-comparison_mcmc.R study-real_mcmc.R study-priors_mcmc.R
NULL

#' @title Function \code{paper_priors}
#' @description Workflow for the priors paper
#' @export
paper_priors = function(){
  path = real_init("real_mcmc")
  fit(path, fbseq_methods = "fullybayes")
  real_analyze(path, "real_analyze")
  coverage_mcmc("coverage_mcmc", zeronormfactors = T)
  fit("coverage_mcmc", benchmarks = NULL, fbseq_methods = "fullybayes")
  coverage_analyze("coverage_mcmc", "coverage_analyze")
  coverage_mcmc("coverage_norm_mcmc", zeronormfactors = F)
  fit("coverage_norm_mcmc", benchmarks = NULL, fbseq_methods = "fullybayes")
  coverage_analyze("coverage_norm_mcmc", "coverage_norm_analyze")
  path = comparison_init("comparison_analyze")
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes")
  comparison_analyze(path, "comparison_analyze")
  priors_init("priors_mcmc")
  priors_mcmc("priors_mcmc")
  priors_analyze("priors_mcmc", "priors_analyze")
  paper_priors_figures()
}

 #' @include study-comparison_mcmc.R study-real_mcmc.R study-priors_mcmc.R
NULL

#' @title Function \code{paper_priors}
#' @description Workflow for the priors paper
#' @export
paper_priors = function(){
  real_init("real_mcmc")
  fit("real_mcmc", priors = c("normal", special_beta_priors()), fbseq_methods = "fullybayes")
  coverage_mcmc("coverage_mcmc", zeronormfactors = T)  
  coverage_mcmc("coverage_norm_mcmc", zeronormfactors = F)
  comparison_init("comparison_mcmc")
  fit("comparison_mcmc", benchmarks = "edgeR", fbseq_methods = "fullybayes", priors = c("normal", special_beta_priors()))  
  priors_mcmc("priors_mcmc")

  real_analyze("real_mcmc", "real_analyze")
  coverage_analyze("coverage_mcmc", "coverage_analyze")
  coverage_analyze("coverage_norm_mcmc", "coverage_norm_analyze")
  comparison_analyze("comparison_mcmc", "comparison_analyze")
  priors_analyze("priors_mcmc", "priors_analyze")

  paper_priors_figures()
}

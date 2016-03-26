 #' @include study-comparison_mcmc.R study-real_mcmc.R study-priors_mcmc.R
NULL

#' @title Function \code{paper_priors}
#' @description Workflow for the priors paper
#' @export
paper_priors = function(){
  path = real_init("real_mcmc")
  fit(path, priors = c("normal", special_beta_priors()[special_beta_priors() != "horseshoe"]), fbseq_methods = "fullybayes")
  coverage_mcmc("coverage_mcmc", zeronormfactors = T)  
  coverage_mcmc("coverage_norm_mcmc", zeronormfactors = F)
  path = comparison_init("comparison_mcmc")
  fit(path, benchmarks = NULL, fbseq_methods = "fullybayes", priors = c("normal", special_beta_priors()[special_beta_priors() != "horseshoe"]))  
  priors_mcmc("priors_mcmc")

  real_analyze(path, "real_analyze")
  coverage_analyze("coverage_mcmc", "coverage_analyze")
  coverage_analyze("coverage_norm_mcmc", "coverage_norm_analyze")
  comparison_analyze("comparison_mcmc", "comparison_analyze")
  priors_analyze("priors_mcmc", "priors_analyze")

  paper_priors_figures()
}

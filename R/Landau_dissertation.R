#' @include paper_computation.R paper_case.R paper_priors.R
NULL

#' @title Function \code{Landau_dissertation}
#' @description Reproduce all the computation, figures, tables, etc. of the Statistics PhD
#' dissertation of Will Landau (http://will-landau.com, will.landau@@gmail.com).
#' @export
Landau_dissertation = function(){
  real_mcmc("real_mcmc")
  computation_mcmc("computation_mcmc")
  coverage_mcmc("coverage_mcmc", zeronormfactors = T)  
  coverage_mcmc("coverage_norm_mcmc", zeronormfactors = F)
  comparison_mcmc("comparison_mcmc")

  real_analyze("real_mcmc", "real_analyze")
  computation_analyze("computation_mcmc", "computation_analyze")
  coverage_analyze("coverage_mcmc", "coverage_analyze")
  coverage_analyze("coverage_norm_mcmc", "coverage_norm_analyze")
  comparison_analyze("comparison_mcmc", "comparison_analyze")

  paper_case_figures()
  paper_priors_figures()
}

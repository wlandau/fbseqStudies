#' @include case_power_analyze.R case_power_fit.R case_power_init.R
NULL

#' @title Function \code{case_power}
#' @description Workflow for the part of the case study paper that looks at gene detection power
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
case_power = function(path = newdir()){
  path = newdir(path)
  case_power_init(path)
  case_power_fit(path)
  fit(path, fbseq_methods = NULL)
  case_power_analyze(path)
  path
}

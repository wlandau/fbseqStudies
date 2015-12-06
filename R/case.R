#' @include case_coverage.R case_power.R real.R
NULL

#' @title Function \code{case}
#' @description Whole orkflow for the part of the case study paper that looks at gene detection power
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
case_power = function(){
  real("real")
  case_coverage("case_coverage")
  case_power("case_power")
}

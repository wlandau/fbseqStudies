#' @include case_coverage.R case_power.R real.R
NULL

#' @title Function \code{case}
#' @description Whole orkflow for the part of the case study paper that looks at gene detection power
#' @export
case = function(){
  real("real")
  case_coverage("case_coverage")
  case_power("case_power")
}

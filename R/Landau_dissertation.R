#' @include paper_computation.R paper_case.R paper_priors.R
NULL

#' @title Function \code{Landau_dissertation}
#' @description Reproduce all the computation, figures, tables, etc. of the Statistics PhD
#' dissertation of Will Landau (http://will-landau.com, will.landau@@gmail.com).
#' @export
Landau_dissertation = function(){
  paper_computation()
  paper_case()
  paper_priors()
}

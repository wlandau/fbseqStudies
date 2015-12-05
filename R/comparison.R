#' @include comparison_analyze.R comparison_fit.R comparison_init.R
NULL

#' @title Function \code{comparison}
#' @description Workflow for the comparison study paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
comparison = function(path = newdir()){
  path = newdir(path)
  comparison_init(path)
  comparison_fit(path)
  comparison_analyze(path)
  path
}

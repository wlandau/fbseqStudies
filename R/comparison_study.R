#' @title Function \code{comparison_study}
#' @description workflow of the comparison study paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
comparison_study = function(path = newdir()){
  path = newdir(path)
  simulations(path)
  fit(path, fbseq_methods = "fullybayes")
}

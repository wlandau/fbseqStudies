#' @title Function \code{case_study}
#' @description workflow of the case study paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
case_study = function(path = newdir()){
  path = newdir(path)
#  simulations(path)
  fit(path, priors = "normal")
}

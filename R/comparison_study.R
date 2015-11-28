#' @title Function \code{comparison_study}
#' @description workflow of the comparison study paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
#' @param genes number of genes. Can be a vector.
#' @param libraries number of libraries. Can be a vector.
#' @param reps number of reps for each #genes/#libraries combination. Should be a scalar.
#' @param paschold_too TRUE/FALSE, include Paschold data
comparison_study = function(path = newdir(), genes = 3e4, libraries = c(16, 32), reps = 5, paschold_too = T){
  path = newdir(path)
  simulations(path, genes, libraries, reps, paschold_too)
  fit(path, fbseq_methods = "fullybayes")
}

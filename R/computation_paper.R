#' @include simulation_model.R
NULL

#' @title Function \code{simulations}
#' @description workflow of the computation paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
#' @param genes number of genes. Can be a vector.
#' @param libraries number of libraries. Can be a vector.
#' @param reps number of reps for each #genes/#libraries combination. Should be a scalar.
computation_paper = function(path = newdir(), genes = c(1024, 4096, 16384), libraries = c(16, 32, 64), reps = 10){
  path = newdir(path)
  for(g in genes) for(n in libraries) for(r in 1:reps)
    saveRDS(simulation_model(genes = g, libraries = n), paste0(path, "model_", g, "_", n, "_", r, ".rds"))
  
# runtime plots

  path
}

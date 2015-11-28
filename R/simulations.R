#' @include simulation_edgeR.R simulation_model.R simulation_paschold.R simulation_simple.R
NULL

#' @title Function \code{simulations}
#' @description create simulation scenarios
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
#' @param genes number of genes. Can be a vector.
#' @param libraries number of libraries. Can be a vector.
#' @param reps number of reps for each #genes/#libraries combination. Should be a scalar.
#' @param paschold_too TRUE/FALSE, include Paschold data
simulations = function(path = newdir(), genes = 3e4, libraries = c(16, 32), reps = 5, paschold_too = T){
  path = newdir(path)
  data(paschold)
  paschold = get("paschold")
  fit = fit_edgeR(paschold@counts, paschold@design)
  if(paschold_too) if(!file.exists(f <- paste0(path, "paschold_39656_16_1.rds"))) saveRDS(simulation_paschold(), f)
  for(g in genes) for(n in libraries) for(r in 1:reps) {
    if(!file.exists(f <- paste0(path, "edgeR_", g, "_", n, "_", r, ".rds"))) saveRDS(simulation_edgeR(genes = g, libraries = n, fit = fit), f)
    if(!file.exists(f <- paste0(path, "model_", g, "_", n, "_", r, ".rds"))) saveRDS(simulation_model(genes = g, libraries = n), f)
    if(!file.exists(f <- paste0(path, "simple_", g, "_", n, "_", r, ".rds"))) saveRDS(simulation_simple(genes = g, libraries = n), f)
  }
  path
}

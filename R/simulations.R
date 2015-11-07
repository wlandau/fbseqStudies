#' @title Function \code{simulations}
#' @description create simulation scenarios
#' @export
#' @return a list if pertinent scenario information
#' @param path to directory to save simulations and results
#' @param genes number of genes. Can be a vector.
#' @param libraries number of libraries. Can be a vector.
#' @param reps number of reps for each #genes/#libraries combination. Should be a scalar.
simulations = function(path = newpath(), genes = 3.5e4, libraries = c(16, 64, 256), reps = 10){
  data(paschold)
  paschold = get("paschold")
  saveRDS(list(scenario = paschold, analyses = list()), paste0(path, "paschold.rds"))
  fit = fit_edgeR(paschold@counts, paschold@design)
  for(g in genes) for(n in libraries) for(r in 1:reps){
    saveRDS(simulation_simple(genes = g, libraries = n), paste0(path, "simple_", g, "_", n, "_", r, ".rds"))
    saveRDS(simulation_edgeR(fit, genes = g, libraries = n), paste0(path, "edgeR_", g, "_", n, "_", r, ".rds"))
    saveRDS(list(scenario = scenario_heterosis_model(genes = g, libraries = n), analyses = list()), 
      paste0(path, "edgeR_", g, "_", n, "_", r, ".rds"))
  }
}
#' @title Function \code{case_coverage_init}
#' @description Initialize the part of the case study paper that looks at the coverage of credible intervals
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
case_coverage_init = function(path = newdir()){
  path = newdir(path)
  genes = 3e4
  libraries = 16
  reps = 10
  for(g in genes) for(n in libraries) for(r in 1:reps)
    if(!file.exists(f <- paste0(path, "model_", g, "_", n, "_", r, ".rds"))) 
      saveRDS(simulation_model(genes = g, libraries = n), f)
  path
}

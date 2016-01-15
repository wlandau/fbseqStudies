#' @title Function \code{computation_init}
#' @description create the data files for the computation study
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
computation_init = function(path = newdir()){
  path = newdir(path)
  genes = c(8192, 16384, 32768, 65536)
  libraries = c(16, 32, 48, 64)
  reps = 1
  for(g in genes) for(n in libraries) for(r in 1:reps)
    if(!file.exists(f <- paste0(path, "duplicated_", g, "_", n, "_", r, ".rds"))) 
      saveRDS(simulation_duplicated(genes = g, libraries = n), f)
  path
}

#' @title Function \code{priors_init}
#' @description Initialize the small simulation study in the priors paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
priors_init = function(path = newdir()){
  path = newdir(path)
  genes = 1e4
  libraries = 8 #c(8, 16)
  reps = 10
  scaledown()
  for(g in genes) for(n in libraries) for(r in 1:reps) for(prior in c("normal", "Laplace", "t"))
    if(!file.exists(f <- paste0(path, "priors", prior, "_", g, "_", n, "_", r, ".rds"))) 
      saveRDS(simulation_priors(genes = g, libraries = n, prior = prior), f)
  path
}

#' @title Function \code{comparison_init}
#' @description Initialize the part of the case study paper that looks at gene detection comparison
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
#' @param ncores number of cores for \code{fit_Niemi()}
comparison_init = function(path = newdir(), ncores = detectCores()){
  path = newdir(path)
  genes = 3e4
  libraries = c(16, 32)
  reps = 1
  data(paschold)
  paschold = get("paschold")
  fit = fit_edgeR(paschold@counts, paschold@design)
  l = simulation_paschold()
#  fit2 = fit_Niemi(counts = paschold@counts, design = paschold@design, group = l$scenario@supplement$group, 
#    ncores = ncores)
  for(g in genes) for(n in libraries) for(r in 1:reps) {
    if(!file.exists(f <- paste0(path, "edgeR_", g, "_", n, "_", r, ".rds"))) 
      saveRDS(simulation_edgeR(genes = g, libraries = n, fit = fit), f)
#    if(!file.exists(f <- paste0(path, "Niemi_", g, "_", n, "_", r, ".rds"))) 
#      saveRDS(simulation_Niemi(genes = g, libraries = n, fit = fit2), f)
    if(!file.exists(f <- paste0(path, "model_", g, "_", n, "_", r, ".rds"))) 
      saveRDS(simulation_model(genes = g, libraries = n), f)
    if(!file.exists(f <- paste0(path, "simple_", g, "_", n, "_", r, ".rds"))) 
      saveRDS(simulation_simple(genes = g, libraries = n), f)
  }
  path
}

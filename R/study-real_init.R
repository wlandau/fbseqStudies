#' @title Function \code{real_init}
#' @description create the data files for the real data analysis
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
real_init = function(path = newdir()){
  path = newdir(path)
  data(paschold)
  paschold = get("paschold")
  scaledown()
  fname = paste0("paschold_", nrow(paschold@counts), "_", 
    ncol(paschold@counts), "_1.rds")
  if(!file.exists(f <- paste0(path, fname))) 
    saveRDS(simulation_paschold(), f)
  path
}

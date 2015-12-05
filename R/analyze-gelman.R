#' @title Function \code{gelman}
#' @description list of gelman factors
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
gelman = function(path){
  path = newdir(path)
  d = list()
  for(f in list.files(path)){
    print(f)
    l = readRDS(paste0(path, f))
    ch = l$analyses[[1]]$chain
    p = ch@psrf_important
    p = p[p > ch@psrf_tol]
    d[[f]] = p
  }
  saveRDS(d, "gelman.rds")
}

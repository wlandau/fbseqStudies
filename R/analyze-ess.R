#' @title Function \code{ess}
#' @description list effective sample sizes
#' @export
#' @param path to directory to save simulations and results
ess = function(path){
  path = newdir(path)
  out = list()
  for(f in list.files(path)){
    l = readRDS(paste0(path, f))
    for(a in names(l$analyses)){
      n = paste(f, a, sep = "_")
      out[[n]] = sort(l$analyses[[a]]$ess)
    }
  }
  out
}

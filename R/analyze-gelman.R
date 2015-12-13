#' @title Function \code{gelman}
#' @description list of gelman factors
#' @export
#' @param path to directory to save simulations and results
gelman = function(path){
  path = newdir(path)
  d = list()
  for(f in list.files(path)){
    l = readRDS(paste0(path, f))
    for(a in names(l$analyses))
      print(f, a, l$analyses[[a]]$psrf)
  }
}

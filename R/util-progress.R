#' @title Function \code{progress}
#' @description check on progress of simulation study
#' @export
#' @return path to simulated objects
#' @param path path to simulated objects
progress = function(path){
  path = newdir(path)
  n = NULL
  i = 0
  for(f in list.files(path)){
    i = i + 1
    s = readRDS(paste0(path, f))
    print(c(i, f, names(s$analyses)))
    n = c(n, names(s$analyses))
  }
  print(table(n))
  path
}

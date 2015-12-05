#' @title Function \code{meta}
#' @description get metadata from a file name
#' @export
#' @return metadata from a file name
#' @param s file name
meta = function(s){
  s = gsub(".rds", "", s)
  p = strsplit(s, "_")[[1]]
  names(p) = c("simulation", "genes", "libraries", "rep", "analysis")[1:length(p)]
  p
}

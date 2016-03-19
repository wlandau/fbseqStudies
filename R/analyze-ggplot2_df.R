#' @title Function \code{ggplot2_df}
#' @description make a ggplot2 data frame out of rds files extracted from simulation lists
#' @export
#' @param path to directory of extracted results files
ggplot2_df = function(path){
  path = newdir(path)
  out = NULL
  for(f in list.files(path)){
    x = readRDS(paste0(path, f))
    x = lapply(x, function(y){do.call(cbind, y)})
    for(n in names(x)) x[[n]] = data.frame(x[[n]], heterosis = n)
    mt = c(file = f, meta(f))
    m = as.data.frame(matrix(mt, nrow = nrow(x[[1]]), ncol = length(mt), byrow = T))
    colnames(m) = names(mt)
    for(n in names(x)) x[[n]] = cbind(x[[n]], m)
    for(n in names(x)) out = rbind(out, x[[n]])
  }
  out$analysis = gsub("normalnormal", "normal", out$analysis)
  out$analysis = gsub("normalLaplace", "Laplace", out$analysis)
  out$analysis = gsub("normalt", "t", out$analysis)
  out
}

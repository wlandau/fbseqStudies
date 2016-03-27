#' @title Function \code{gelman}
#' @description list of gelman factors
#' @export
#' @param from to directory to save simulations and results
#' @param to output directory
gelman = function(from, to){
  from = newdir(from)
  to = newdir(to)
  out = list()
  for(f in list.files(from)){
    l = readRDS(paste0(from, f))
    for(a in names(l$analyses)) if("psrf" %in% names(l$analyses[[a]])){
      n = paste(f, a, sep = "_")
      out[[n]] = sort(l$analyses[[a]]$psrf, decreasing = T)
    }
  }
  saveRDS(out, paste0(to, "gelman.rds"))
  gelman_important = lapply(out, function(x){
    x = x[x > 1.1]
    x = x[!grepl("epsilon", names(x))]
    x = sort(x, decreasing = TRUE)
  })
  saveRDS(gelman_important, paste0(to, "gelman_important.rds"))
}

#' @title Function \code{ess}
#' @description list effective sample sizes
#' @export
#' @param from to directory to save simulations and results
#' @param to output directory
ess = function(from, to){
  from = newdir(from)
  to = newdir(to)
  out = list()
  for(f in list.files(from)){
    l = readRDS(paste0(from, f))
    for(a in names(l$analyses)) if("ess" %in% names(l$analyses[[a]])){
      n = paste(f, a, sep = "_")
      out[[n]] = sort(l$analyses[[a]]$ess)
    }
  }
  saveRDS(out, paste0(to, "ess.rds"))
}

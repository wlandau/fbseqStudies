#' @title Function \code{rocs}
#' @description make repairs to names, etc. in a single simulation list
#' @export
#' @param l simulation list
repair = function(l){
  for(a in names(l$analyses)){
    if(!any(grepl("prob_", colnames(l$analyses[[a]]$estimates))))
      colnames(l$analyses[[a]]$estimates)[6:10] = paste0("prob_", colnames(l$analyses[[a]]$estimates)[6:10])
    if(!any(grepl("effect_", colnames(l$analyses[[a]]$estimates))))
      colnames(l$analyses[[a]]$estimates)[11:15] = paste0("effect_", colnames(l$analyses[[a]]$estimates)[11:15])
    if("chains" %in% names(l$analyses[[a]])){
      l$analyses[[a]]$psrf = psrf(l$analyses[[a]]$chains)
      l$analyses[[a]]$ess = effectiveSize(mcmc_samples(l$analyses[[a]]$chains))
    }
  }
  l
}

#' @title Function \code{rocs}
#' @description make repairs to names, etc. to simulation lists
#' @export
#' @param from directory with simulation lists
#' @param to output directory
repairs = function(from, to){
  over_sims(from, to, repair)
}

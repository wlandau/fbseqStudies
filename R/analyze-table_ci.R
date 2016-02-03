#' @include util-mytheme.R
NULL

#' @title Function \code{table_ci}
#' @description table information about coverage of parameters in credible intervals
#' @export
#' @param from directory containing ci information
#' @param to directory to save plots
table_ci = function(from, to){
  from = newdir(from)
  if(!file.exists(paste0(from, "ci.rds"))) return()
  l = as.data.frame(readRDS(paste0(from, "ci.rds")))
  if(is.null(l)) return()
  to = newdir(to)

  for(level in unique(l$level)){
    coverage_over_genes = ddply(l, c("type", "simulation", "libraries", "rep", "analysis"), function(x){
      data.frame(parameter = x$type[1], simulation = x$simulation[1], libraries = x$libraries[1], rep = x$rep[1],
        analysis = x$analysis[1], cover = mean(x[x$level == level,]$cover))
    }, .progress = "text")
    saveRDS(coverage_over_genes, paste0(to, "coverage_over_genes_", level, ".rds"))
  }

  reps = unique(l$rep)
  for(v in unique(l$level)){
    k = list()
    for(r in reps) k[[r]] = l[l$rep == r & l$level == v,]
    cover_df = k[[1]][, c("parameter", "simulation", "libraries", "analysis")]
    cover = sapply(k, function(x) x$cover)
    cover_df$cover = rowMeans(cover)
    saveRDS(cover_df, paste0(to, "coverage_over_reps_", v, ".rds"))
  }
}

#' @include analyze-mytheme.R
NULL

#' @title Function \code{table_ci_beta}
#' @description table information about coverage of beta parameters in credible intervals
#' @export
#' @param from directory containing ci information
#' @param to directory to save plots
table_ci_beta = function(from, to){
  from = newdir(from)
  if(!file.exists(paste0(from, "ci_beta.rds"))) return()
  l = as.data.frame(readRDS(paste0(from, "ci_beta.rds")))
  if(is.null(l)) return()
  to = newdir(to)

  l$beta = gsub("_[0-9]*$", "", l$parameter)
  coverage_by_beta = ddply(l, c("beta", "simulation", "libraries", "rep", "analysis"), function(x){
    data.frame(beta = x$beta[1], simulation = x$simulation[1], libraries = x$libraries[1], rep = x$rep[1],
      analysis = x$analysis[1], cover50 = mean(x[x$level == 0.5,]$cover), cover95 = mean(x[x$level == 0.5,]$cover))
  }, .progress = "text")
  saveRDS(coverage_by_beta, paste0(to, "cover_by_beta.rds"))

  reps = unique(l$rep)
  for(v in unique(l$level)){
    k = list()
    for(r in reps){
      k[[r]] = l[l$rep == r & l$level == v,]
      cover_df = k[[1]]
      cover = sapply(k, function(x) x$cover)
      cover_df$cover = rowMeans(cover)
      saveRDS(cover_df, paste0(to, "coverage_by_gene_", v, ".rds"))
    }
  }
}

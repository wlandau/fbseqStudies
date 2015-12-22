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

  coverage_by_gene = ddply(l, c("parameter", "simulation", "libraries", "analysis"), function(x){
    data.frame(parameter = x$parameter[1], simulation = x$simulation[1], libraries = x$libraries[1],
      analysis = x$analysis[1], cover50 = mean(x[x$level == 0.5,]$cover), cover95 = mean(x[x$level == 0.5,]$cover))
  }, .progress = "text")
  saveRDS(coverage_by_gene, paste0(to, "cover_by_gene.rds"))

  l$beta = gsub("_[0-9]*$", "", l$parameter)
  coverage_by_beta = ddply(l, c("beta", "simulation", "libraries", "rep", "analysis"), function(x){
    data.frame(beta = x$beta[1], simulation = x$simulation[1], libraries = x$libraries[1], rep = x$rep[1],
      analysis = x$analysis[1], cover50 = mean(x[x$level == 0.5,]$cover), cover95 = mean(x[x$level == 0.5,]$cover))
  }, .progress = "text")
  saveRDS(coverage_by_beta, paste0(to, "cover_by_beta.rds"))
}

#' @title Function \code{runtime_tables}
#' @description table of runtimes for computation paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
runtime_tables = function(path){
  path = newdir(path)
  l = list()
  e = list()
  hyper = "theta|sigmaSquared|nu|tau"

  for(f in list.files(path)){
    print(f)
    r = readRDS(paste0(path, f))
    flat = flatten(r$analyses[[1]]$chain)
    flat = flat[,grep(hyper, colnames(flat))]
    ess = effectiveSize(flat)
    l[[f]] = r$analyses[[1]]$runtime
    e[[f]] = ess
  }

  alltimes = do.call("rbind", l)
  alless = do.call("rbind", e)
  miness = apply(alless, 1, min)
  medianess = apply(alless, 1, median)

  meta = do.call("rbind", strsplit(rownames(alltimes), split = "_|\\."))
  G = as.numeric(meta[,2])
  N = as.numeric(meta[,3])
  rep = as.numeric(meta[,4])

  d = data.frame(G = as.integer(G), N = as.integer(N), rep = rep, minutes = signif(alltimes[,"elapsed"]/60, 3), min_ess = signif(miness, 3), median_ess =  signif(medianess, 3))
  avg = ddply(d, c("G", "N"), function(x){
    data.frame(G = x$G[1], N = x$N[1], total_minutes = signif(mean(x$minutes), 3), seconds_per_min_ess = signif(mean(60*x$minutes/x$min_ess), 3), seconds_per_median_ess = signif(mean(60*x$minutes/x$median_ess), 3))
  })

  sds = ddply(d, c("G", "N"), function(x){
    data.frame(G = x$G[1], N = x$N[1], total_minutes = signif(sd(x$minutes), 3), seconds_per_min_ess = signif(sd(60*x$minutes/x$min_ess), 3), seconds_per_median_ess = signif(sd(60*x$minutes/x$median_ess), 3))
  })

  write.table(print(xtable(avg), include.rownames = F), row.names = F, file = "mean_runtimes.txt")
  write.table(print(xtable(sds), include.rownames = F), row.names = F, file = "sd_runtimes.txt")
}

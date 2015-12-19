#' @title Function \code{runtime}
#' @description Runtimes in computation study
#' @export
#' @return runtime table
#' @param path directory with simulation lists
runtime = function(path){
  path = newdir(path)
  long = NULL
  for(f in list.files(path)){
    print(f)
    l = readRDS(paste0(path, f))
    a = l$analyses[["fullybayes+normal"]]
    runtime = a$runtime["elapsed"]/3600
    names(runtime) = NULL
    min_ess = min(a$ess[c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5))])
    median_ess = median(a$ess[c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5))])
    runtime_per_min_ess_100 = 100* runtime/min_ess
    runtime_per_median_ess_100 = 100* runtime/median_ess
    names(runtime_per_min_ess_100) = NULL
    names(runtime_per_median_ess_100) = NULL
    long = rbind(long, c(G = a$chains[[1]]@G, N = a$chains[[1]]@N,
      runtime = runtime, min_ess = min_ess, median_ess = median_ess,
      runtime_per_min_ess_100 =  runtime_per_min_ess_100,
      runtime_per_median_ess_100 = runtime_per_median_ess_100))
  }
  long = as.data.frame(long)
  out = list(long = long)
  for(n in colnames(long)[3:7]){
    out[[n]] = round(t(matrix(long[,n], nrow = length(unique(long$G)), ncol = length(unique(long$N)))), 3)
    rownames(out[[n]]) = unique(long$G)
    colnames(out[[n]]) = unique(long$N)
  }
  out
}


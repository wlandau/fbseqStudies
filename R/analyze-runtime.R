#' @title Function \code{runtime}
#' @description Runtimes in computation study
#' @export
#' @return runtime table
#' @param from directory with simulation lists
#' @param to output directory
runtime = function(from, to){
  from = newdir(from)
  to = newdir(to)
  long = NULL
  for(f in list.files(from)){
    print(f)
    l = readRDS(paste0(from, f))
   G = dim(l$scenario@counts)[1]
   N = dim(l$scenario@counts)[2]

    for(a in l$analyses){
      runtime = a$runtime["elapsed"]/3600
      names(runtime) = NULL

      if("ess" %in% names(a) & grepl("fully", a$analysis)){
        min_ess = min(a$ess[c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5))])
        which_min_ess = names(which.min(a$ess[c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5))]))
        median_ess = median(a$ess[c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5))])
        runtime_per_min_ess_100 = 100* runtime/min_ess
        runtime_per_median_ess_100 = 100* runtime/median_ess
      } else {
        median_ess = min_ess = runtime_per_min_ess_100 = runtime_per_median_ess_100 = -1
        which_min_ess = "-1"
      }

      names(runtime_per_min_ess_100) = NULL
      names(runtime_per_median_ess_100) = NULL
      long = rbind(long, data.frame(file = f, simulation = l$simulation, analysis = a$analysis, rep = meta(f)["rep"], G = G, N = N,
        runtime = runtime, min_ess = min_ess, which_min_ess = which_min_ess, median_ess = median_ess,
        runtime_per_min_ess_100 =  runtime_per_min_ess_100,
        runtime_per_median_ess_100 = runtime_per_median_ess_100))
    }
  }
  long[long == -1] = NA
  saveRDS(long, paste0(to, "runtime.rds"))
}

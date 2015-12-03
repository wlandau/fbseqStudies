#' @include simulation_model.R
NULL

#' @title Function \code{computation_study}
#' @description workflow of the computation study paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
#' @param genes number of genes. Can be a vector.
#' @param libraries number of libraries. Can be a vector.
#' @param reps number of reps for each #genes/#libraries combination. Should be a scalar.
computation_study = function(path = newdir(), genes = c(8192, 16384, 32768), libraries = c(16, 32, 64), reps = 5){
  path = newdir(path)
#  for(g in genes) for(n in libraries) for(r in 1:reps)
#    if(!file.exists(f <- paste0(path, "model_", g, "_", n, "_", r, ".rds"))) saveRDS(simulation_model(genes = g, libraries = n), f)
  fit(path, benchmarks = NULL,  fbseq_methods = "fullybayes", priors = "normal", 
    configs = Configs(burnin = 4e4, thin = 40, iterations = 4e3, max_attempts_diag = 1, max_attempts_ess = 0))  
  computation_study_gelman(path)
  computation_study_runtime_plots(path)
  computation_study_runtime_table(path)
  path
}

#' @title Function \code{computation_study_gelman}
#' @description list of gelman factors
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
computation_study_gelman = function(path){
  path = newdir(path)
  d = list()
  for(f in list.files(path)){
    print(f)
    l = readRDS(paste0(path, f))
    ch = l$analyses[[1]]$chain
    p = ch@psrf_important
    p = p[p > ch@psrf_tol]
    d[[f]] = p
  }
  saveRDS(d, "gelman.rds")
}

#' @title Function \code{computation_study_runtime_plots}
#' @description plot runtimes for computation paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
computation_study_runtime_plots = function(path){
  path = newdir(path)
  d = NULL
  for(f in list.files(path)){
    print(f)
    l = readRDS(paste0(path, f))
    d = rbind(d, c(l$analyses[[1]]$runtime/60, N = ncol(l$scenario@counts), G = nrow(l$scenario@counts), file = f))
  }
  d = as.data.frame(d)
  d$rep = rep(1:10, times = 9)
  for(n in c("elapsed", "G", "N"))
    d[[n]] = as.numeric(as.character(d[[n]]))

  pl = ggplot(d) + 
    geom_point(aes_string(x = "G", y = "elapsed")) +
    geom_line(aes_string(x = "G", y = "elapsed", group = "rep")) + 
    facet_grid(~N) + 
    xlab("\nNumber of genes") +
    ylab("Elapsed time (minutes)\n") +
    theme(legend.position="none",
               panel.backgsignif = element_rect(fill='white'),
               panel.border = element_rect(color="black", fill = NA),
               panel.grid.major = element_line(color="lightgray"),
               panel.grid.minor = element_blank())
  ggsave(filename = "runtimes1.pdf", pl)

  pl = ggplot(d) + 
    geom_point(aes_string(x = "N", y = "elapsed")) +
    geom_line(aes_string(x = "N", y = "elapsed", group = "rep")) + 
    facet_grid(~G) + 
    xlab("\nNumber of libraries") +
    ylab("Elapsed time (minutes)\n") +
    theme(legend.position="none",
               panel.backgsignif = element_rect(fill='white'),
               panel.border = element_rect(color="black", fill = NA),
               panel.grid.major = element_line(color="lightgray"),
               panel.grid.minor = element_blank())
  ggsave(filename = "runtimes2.pdf", pl)
}

#' @title Function \code{computation_study_runtime_table}
#' @description table of runtimes for computation paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
computation_study_runtime_table = function(path){
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

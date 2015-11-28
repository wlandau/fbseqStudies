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
  for(g in genes) for(n in libraries) for(r in 1:reps)
    if(!file.exists(f <- paste0(path, "model_", g, "_", n, "_", r, ".rds"))) saveRDS(simulation_model(genes = g, libraries = n), f)
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
               panel.background = element_rect(fill='white'),
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
               panel.background = element_rect(fill='white'),
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
  d = NULL
  for(f in list.files(path)){
    print(f)
    l = readRDS(paste0(path, f))
    d = rbind(d, c(l$analyses[[1]]$runtime/60, N = ncol(l$scenario@counts), G = nrow(l$scenario@counts), file = f))
  }
  d = as.data.frame(d)
  for(n in c("elapsed", "G", "N"))
    d[[n]] = as.numeric(as.character(d[[n]]))
  d2 = ddply(d, c("N", "G"), function(df){
    min = mean(df$elapsed)
    sec = (min - floor(min)) * 60
    data.frame(N = df$N[1], G = df$G[1], min = floor(min), sec = round(sec))
  })
  write.table(d2, row.names = F, file = "mean_runtimes.txt")
  d3 = ddply(d, c("N", "G"), function(df){
    data.frame(N = df$N[1], G = df$G[1], sd_runtime = sd(df$elapsed))
  })
  write.table(d3, row.names = F, file = "sd_runtimes.txt")
}

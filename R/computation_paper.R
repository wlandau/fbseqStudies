#' @include simulation_model.R
NULL

#' @title Function \code{computation_paper}
#' @description workflow of the computation paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
#' @param genes number of genes. Can be a vector.
#' @param libraries number of libraries. Can be a vector.
#' @param reps number of reps for each #genes/#libraries combination. Should be a scalar.
computation_paper = function(path = newdir(), genes = c(1024, 4096, 16384), libraries = c(16, 32, 64), reps = 10){
  path = newdir(path)
  for(g in genes) for(n in libraries) for(r in 1:reps)
    saveRDS(simulation_model(genes = g, libraries = n), paste0(path, "model_", g, "_", n, "_", r, ".rds"))
  fit(path, benchmarks = NULL,  fbseq_methods = "fullybayes", priors = "normal", 
    configs = Configs(max_attempts_diag = 0, max_attempts_ess = 0))  
  computation_paper_runtimes(path)
  path
}

#' @title Function \code{computation_paper_runtimes}
#' @description plot runtimes for computation paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
computation_paper_runtimes = function(path){
  for(f in list.files(path)){
    l = readRDS(paste0(path, f))
    d = rbind(d, c(l$runtime/60, N = ncol(l$scenario@counts), G = nrow(l$scenario@counts), file = f))
  }
  d = as.data.frame(d)
  pl = ggplot(d) + geom_line(aes_string(x = "G", y = "elapsed", group = "file")) + 
    facet_grid(~N) + 
    xlab("\nNumber of genes") +
    ylab("Elapsed time (minutes)\n") +
    theme(panel.background = element_rect(fill='white'),
               panel.border = element_rect(color="black", fill = NA),
               panel.grid.major = element_line(color="lightgray"),
               panel.grid.minor = element_blank())
  ggsave(filename = "runtimes.pdf", pl)
}

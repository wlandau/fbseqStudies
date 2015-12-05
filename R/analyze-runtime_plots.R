#' @title Function \code{runtime_plots}
#' @description plot runtimes for computation paper
#' @export
#' @return path to simulated objects
#' @param path to directory to save simulations and results
runtime_plots = function(path){
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

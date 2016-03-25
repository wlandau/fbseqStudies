#' @title Function \code{plot_runtime}
#' @description Plots runtimes in computation study
#' @export
#' @param from directory with runtime table
#' @param to output directory
plot_runtime = function(from, to){
  from = newdir(from)
  to = newdir(to)
  long = readRDS(paste0(from, "runtime.rds"))
  d = long[,-(1:4)]
  dub = sapply(d, is.double)
  dub[c("min_ess", "median_ess")] = F
  d[,dub] = round(d[,dub], 2)
  d$min_ess = as.integer(round(d$min_ess))
  d$median_ess = as.integer(round(d$median_ess))
  d = d[order(d$G),]
  d$which_min_ess = as.character(d$which_min_ess)
  for(i in 1:5)
    d$which_min_ess = gsub(paste0("sigmaSquared_", i), paste0("sigma_", i, "^2"), d$which_min_ess)
  d$which_min_ess = paste0("$\\", d$which_min_ess, "$")
  d = d[,c(1, 2, 3, 6, 8, 4, 7, 5)]
  colnames(d) = gsub("_", " ", colnames(d))
  h = c(0, 1:3*4)
  h = h[h <= nrow(d)]
  str = print(xtable(d), include.rownames=F, sanitize.text.function=function(x){x}, 
    hline.after = h)
  write(str, file = paste0(to, "runtime.tex"))

  long1 = long2 = long
  long1$N = ordered(long1$N, levels = sort(unique(long1$N), decreasing = T))
  long2$G = ordered(long2$G, levels = sort(unique(long2$G), decreasing = T))

  p1 = ggplot(long1) +
    geom_line(aes_string(x = "G", y = "runtime", group = "N", linetype = "N")) + theme_few() + 
    scale_x_continuous(breaks = sort(unique(long$G))) +  
    theme(axis.text.x = element_text(angle = -80, hjust = 0)) + ylab("Total elapsed hours")
ggsave(plot = p1, file = paste0(to, "runtimeG.pdf"))

  p2 = ggplot(long2) +
    geom_line(aes_string(x = "N", y = "runtime", group = "G", linetype = "G")) + theme_few() + 
    scale_x_continuous(breaks = sort(unique(long$N), decreasing = T)) + ylab("")
ggsave(plot = p1, file = paste0(to, "runtimeN.pdf"))

  tryCatch({
   pdf(paste0(to, "runtime.pdf"), width= 8,height=3)
    grid.arrange(p1, p2, nrow = 1)
    dev.off()

    postscript(paste0(to, "runtime.ps"), width= 8,height=3)
    grid.arrange(p1, p2, nrow = 1)
    dev.off()

    setEPS()
    postscript(paste0(to, "runtime.eps"), width= 8,height=3)
    grid.arrange(p1, p2, nrow = 1)
    dev.off()
  }, error = function(e) print("Failed to make grid.arrange'ed runtime plots."))
}

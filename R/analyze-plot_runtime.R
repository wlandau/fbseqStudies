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
  d[,dub] = round(d[,dub], 3)
  d = d[order(d$G),]
  d$which_min_ess = as.character(d$which_min_ess)
  for(i in 1:5)
    d$which_min_ess = gsub(paste0("sigmaSquared_", i), paste0("sigma_", i, "^2"), d$which_min_ess)
  d$which_min_ess = paste0("$\\", d$which_min_ess, "$")
  colnames(d) = c("G", "N", "runtime", "min ess", "min ess param", "median ess", "ratio A", "ratio B")
  h = c(0, 1:4*4)
  h = h[h <= nrow(d)]
  str = print(xtable(d), include.rownames=F, sanitize.text.function=function(x){x}, 
    hline.after = h)
  write(str, file = paste0(to, "runtime.tex"))

  p1 = ggplot(long) +
    geom_line(aes_string(x = "G", y = "runtime", group = "N", linetype = "as.factor(N)")) + 
    mytheme_straight() + 
    labs(linetype = "N")

  p2 = ggplot(long) +
    geom_line(aes_string(x = "N", y = "runtime", group = "G", linetype = "as.factor(G)")) + 
    mytheme_straight() + 
    labs(linetype = "G")

  pdf(paste0(to, "runtime.pdf"), width= 7,height=3)
  grid.arrange(p1, p2, nrow = 1)
  dev.off()
}

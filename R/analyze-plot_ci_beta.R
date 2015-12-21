#' @include analyze-mytheme.R
NULL

#' @title Function \code{plot_ci_beta}
#' @description plot roc curves using rds files extracted from simulation lists
#' @export
#' @param from directory containing ci information
#' @param to directory to save plots
plot_ci_beta = function(from, to){
  from = newdir(from)
  if(!file.exists(paste0(from, "ci_beta_0.5.rds"))) return()
  l = readRDS(paste0(from, "ci_beta_0.5.rds"))
  if(is.null(l$truth)) return()
  to = newdir(to)
  G = dim(l$truth)/5
  parms = paste0("beta_", rep(1:5, each = 2), "_", sample.int(G, 1))
  for(n in parms){
    d = data.frame(lower = l$lower[n,] - l$truth[n,], upper = l$upper[n,] - l$truth[n,], rep = 1:length(l$truth[n,]))
    pl = ggplot(d) + mytheme_straight() + 
      geom_segment(aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
      geom_abline(slope = 0, intercept = 0)
    ggsave(paste0(to, n, "_5.pdf"), pl)
  }
  out = list(ci_0.5 = rowMeans(l$lower < l$truth & l$truth < l$upper))
  
  l = readRDS(paste0(from, "ci_beta_0.95.rds"))
  for(ell in 1:5) for(rep in 1:ncol(l$lower)){
    n = grep(paste0("beta_", ell), rownames(l$truth))
    d = data.frame(lower = l$lower[n,rep], upper = l$upper[n,rep], truth = l$truth[n,rep],
      cover = l$lower[n,rep] < l$truth[n,rep] & l$truth[n,rep] < l$upper[n,rep])
    d = d[order(d$truth),]
    dcover = d[d$cover,]
    dmiss = d[!d$cover,]
    dcover$index = 1:dim(dcover)[1]
    dmiss$index = 1:dim(dmiss)[1]
    pl = ggplot(dcover) + mytheme_straight() + 
      geom_segment(aes_string(x = "index", xend = "index", y = "lower", yend = "upper"), color = "darkGray") + 
      geom_point(aes_string(x = "index", y = "truth"), size = I(0.5))
    ggsave(paste0(to, "beta_", ell, "_cover_95_", rep, ".pdf"), pl)
    pl = ggplot(dmiss) + mytheme_straight() + 
      geom_segment(aes_string(x = "index", xend = "index", y = "lower", yend = "upper"), alpha = 0.5, color = "darkGray") + 
      geom_point(aes_string(x = "index", y = "truth"), size = I(0.5))
    ggsave(paste0(to, "beta_", ell, "_miss_95_", rep, ".pdf"), pl)
    out[["ci_0.95"]][paste0("beta_", ell)] = dim(dcover)[1]/(dim(dcover)[1] + dim(dmiss)[1])
  }
  saveRDS(out, paste0(to, "coverage.rds"))
}

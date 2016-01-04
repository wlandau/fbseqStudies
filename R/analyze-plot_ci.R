#' @include analyze-mytheme.R
NULL

#' @title Function \code{plot_ci}
#' @description plot information about coverage of beta parameters in credible intervals
#' @export
#' @param from directory containing ci information
#' @param to directory to save plots
plot_ci = function(from, to){
  from = newdir(from)
  if(!file.exists(paste0(from, "ci.rds"))) return()
  l = as.data.frame(readRDS(paste0(from, "ci.rds")))
  if(is.null(l)) return()
  to = newdir(to)

  for(level in unique(l$level)){
    to_coverage_rep = newdir(paste0(to, "ci", level))
    gs = as.integer(gsub("beta_[0-9]_", "", l[grepl("beta", l$parameter),"parameter"]))
    G = max(gs)
    parms = c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5), 
      paste0("beta_", rep(1:5, each = 2), "_", sample.int(G, 1)))
    l0 = l[l$parameter %in% parms & l$level == level,]
    tmp = ddply(l0, c("parameter", "simulation", "libraries", "analysis"), function(x){
      iden = paste(x$simulation[1], x$libraries[1], x$analysis[1], x$parameter[1], sep = "_")
      d = data.frame(lower = x$lower - x$truth, upper = x$upper - x$truth, rep = ordered(unique(x$rep), sort(unique(x$rep))))
      pl = ggplot(d) + mytheme_straight() + 
        geom_segment(aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
        geom_abline(slope = 0, intercept = 0)
      ggsave(paste0(to_coverage_rep, iden, "_ci", level, ".pdf"), pl, height = 8, width = 8)
    }, .progress = "text")
  }

  for(level in unique(l$level)){
    to_cover = newdir(paste0(to, "ci_beta", level, "_cover"))
    to_miss = newdir(paste0(to, "ci_beta", level, "_miss"))

    for(ell in 1:5){
      l0 = l[grepl(paste0("beta_", ell), l$parameter) & l$level == level & l$rep == unique(l$rep)[1],]
      tmp =  ddply(l0, c("simulation", "libraries", "rep", "analysis"), function(x){
        iden = paste(x$simulation[1], x$libraries[1], x$rep[1], x$analysis[1], sep = "_")
        x = x[order(x$truth),]
        ds = list(cover = x[x$cover,], miss = x[!x$cover,])
      
        for(i in 1:length(ds)){
          if(!nrow(ds[[i]])){ 
            ds[[i]] = rbind(ds[[i]], rep(0, ncol(ds[[i]])))
            colnames(ds[[i]]) = colnames(x)
          }
          ds[[i]]$index = 1:dim(ds[[i]])[1]
        }

        cover = ds[["cover"]]
        miss = ds[["miss"]]

        pl = ggplot(cover) + mytheme_straight() + 
          geom_segment(aes_string(x = "index", xend = "index", y = "lower", yend = "upper"), color = "darkGray") + 
          geom_point(aes_string(x = "index", y = "truth"), size = I(0.5))
        ggsave(paste0(to_cover, iden, "_beta_", ell, "_cover_", level, " .pdf"), pl, height = 8, width = 8)

        pl = ggplot(miss) + mytheme_straight() + 
          geom_segment(aes_string(x = "index", xend = "index", y = "lower", yend = "upper"), color = "darkGray") + 
          geom_point(aes_string(x = "index", y = "truth"), size = I(0.5))
        ggsave(paste0(to_miss, iden, "_beta_", ell, "_miss_", level, ".pdf"), pl, height = 8, width = 8)
      }, .progress = "text")
    }
  }
}

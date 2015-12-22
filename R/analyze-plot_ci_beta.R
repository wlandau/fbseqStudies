#' @include analyze-mytheme.R
NULL

#' @title Function \code{plot_ci_beta}
#' @description plot information about coverage of beta parameters in credible intervals
#' @export
#' @param from directory containing ci information
#' @param to directory to save plots
plot_ci_beta = function(from, to){
  from = newdir(from)
  if(!file.exists(paste0(from, "ci_beta.rds"))) return()
  l = as.data.frame(readRDS(paste0(from, "ci_beta.rds")))
  if(is.null(l)) return()
  to = newdir(to)

  to50 = newdir(paste0(to, "ci50"))
  gs = as.integer(gsub("beta_[0-9]_", "", l[,"parameter"]))
  G = max(gs)
  parms = paste0("beta_", rep(1:5, each = 2), "_", sample.int(G, 1))
  l0 = l[l$parameter %in% parms & l$level == "0.5",]
  tmp = ddply(l0, c("parameter", "simulation", "libraries", "analysis"), function(x){
    iden = paste(x$simulation[1], x$libraries[1], x$analysis[1], x$parameter[1], sep = "_")
    d = data.frame(lower = x$lower - x$truth, upper = x$upper - x$truth, rep = 1:length(x$truth))
    pl = ggplot(d) + mytheme_straight() + 
      geom_segment(aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
      geom_abline(slope = 0, intercept = 0)
    ggsave(paste0(to50, iden, "_ci50.pdf"), pl)
  })
  
  to_cover = newdir(paste0(to, "ci95_cover"))
  to_miss = newdir(paste0(to, "ci95_miss"))

  for(ell in 1:5){
    l0 = l[grepl(paste0("beta_", ell), l$parameter) & l$level == 0.95 & l$rep == 1,]
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
      ggsave(paste0(to_cover, iden, "_beta_", ell, "_cover_95.pdf"), pl)

      pl = ggplot(miss) + mytheme_straight() + 
        geom_segment(aes_string(x = "index", xend = "index", y = "lower", yend = "upper"), color = "darkGray") + 
        geom_point(aes_string(x = "index", y = "truth"), size = I(0.5))
      ggsave(paste0(to_miss, iden, "_beta_", ell, "_miss_95.pdf"), pl)
    })
  }

  to_tables = newdir(paste0(to, "tables"))
  coverage_by_gene = ddply(l, c("parameter", "simulation", "libraries", "analysis"), function(x){
    data.frame(parameter = x$parameter[1], simulation = x$simulation[1], libraries = x$libraries[1],
      analysis = x$analysis[1], cover50 = mean(x[x$level == 0.5,]$cover), cover95 = mean(x[x$level == 0.5,]$cover))
  })
  saveRDS(coverage_by_gene, paste0(to_tables, "cover_by_gene.rds"))

  l$beta = gsub("_[0-9]*$", "", l$parameter)
  coverage_by_beta = ddply(l, c("beta", "simulation", "libraries", "rep", "analysis"), function(x){
    data.frame(beta = x$beta[1], simulation = x$simulation[1], libraries = x$libraries[1], rep = x$rep[1],
      analysis = x$analysis[1], cover50 = mean(x[x$level == 0.5,]$cover), cover95 = mean(x[x$level == 0.5,]$cover))
  })
  saveRDS(coverage_by_beta, paste0(to_tables, "cover_by_beta.rds"))
}

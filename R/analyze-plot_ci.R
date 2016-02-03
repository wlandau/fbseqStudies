#' @include util-mytheme.R
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
    parms = c("nu", "tau", paste0("theta_", 1:5), paste0("sigmaSquared_", 1:5))
    x = sort(unique(as.integer(l$rep)))
    l$rep = ordered(l$rep, levels = x)
    l0 = l[l$parameter %in% parms & l$level == level,]

    l1 = ddply(l0, c("parameter", "simulation", "libraries", "analysis"), function(x){
      if(mean(x$cover) >= level) return(NULL)
      x
    }, .progress = "text")

    tmp = ddply(l1, c("simulation", "libraries", "analysis"), function(x){
      iden = paste("lowcover", level, x$simulation[1], x$libraries[1], x$analysis[1], sep = "-")
      iden = gsub("\\.", "-", iden)
      x$type = as.character(x$type)
      for(i in 1:5){
        x$type = gsub(paste0("sigmaSquared_", i), paste0("sigma[",i, "]^2"), x$type)
        x$type = gsub(paste0("theta_", i), paste0("theta[",i, "]"), x$type)
        x$type = gsub(paste0("beta_", i), paste0("beta[",i, "]"), x$type)
      }
      pl = ggplot(x) + 
        geom_segment(aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
        facet_wrap(as.formula("~type"), scales = "free_y", labeller = label_parsed) + 
        geom_abline(aes(slope = 0, intercept = x$truth)) + 
        xlab("simulated dataset") +
        ylab("credible interval") + 
        theme_few() + theme(strip.text.x = element_text(size = 14))
      ggsave(paste0(to_coverage_rep, iden, ".pdf"), pl, height = 4, width = 5, dpi = 1200)
      ggsave(paste0(to_coverage_rep, iden, ".ps"), pl, height = 4, width = 5, dpi = 1200)
      ggsave(paste0(to_coverage_rep, iden, ".eps"), pl, height = 4, width = 5, dpi = 1200)
      ggsave(paste0(to_coverage_rep, iden, ".tiff"), pl, height = 4, width = 5, dpi = 1200)
    }, .progress = "text")

    tmp = ddply(l0, c("parameter", "simulation", "libraries", "analysis"), function(x){
      iden = paste(x$simulation[1], x$libraries[1], x$analysis[1], x$parameter[1], sep = "_")
      d = data.frame(lower = x$lower, upper = x$upper, rep = ordered(unique(x$rep), sort(unique(x$rep))))
      pl = ggplot(d) + mytheme_straight() + 
        geom_segment(aes_string(x = "rep", xend = "rep", y = "lower", yend = "upper")) + 
        geom_abline(slope = 0, intercept = x$truth)
      ggsave(paste0(to_coverage_rep, iden, "_ci", level, ".pdf"), pl, height = 8, width = 8)
    }, .progress = "text")
  }

  for(level in unique(l$level)){
    to_cover = newdir(paste0(to, "ci_beta", level, "_cover"))
    to_miss = newdir(paste0(to, "ci_beta", level, "_miss"))
    to_both = newdir(paste0(to, "ci_beta", level))
    to_trend = newdir(paste0(to, "ci_beta_trend", level))

    tmp =  ddply(l, c("simulation", "libraries", "rep", "analysis"), function(x){
      iden = paste(x$simulation[1], x$libraries[1], x$rep[1], x$analysis[1], sep = "-")
      print(paste0("        ", iden))
      x = x[grepl("beta_", x$parameter) & x$level == level,]
      x = x[order(x$truth),]
      x$index = 1:dim(x)[1]
      pl = ggplot() + mytheme_straight() +
        geom_segment(data = x[x$cover,], mapping = aes_string(x = "index", xend = "index", 
          y = "lower", yend = "upper")) +      
        geom_segment(data = x[!x$cover,], mapping = aes_string(x = "index", xend = "index", y = "lower", yend = "upper"), 
          color = "red") + 
        geom_point(data = x, mapping = aes_string(x = "index", y = "truth"), size = I(0.5)) + 
        facet_wrap(as.formula("~type"), scales = "free")
      ggsave(paste0(to_both, iden, "_beta_", level, " .png"), pl, height = 8, width = 8)

      x$type = as.character(x$type)
      for(i in 1:5){
        x$type = gsub(paste0("sigmaSquared_", i), paste0("sigma[",i, "]^2"), x$type)
        x$type = gsub(paste0("theta_", i), paste0("theta[",i, "]"), x$type)
        x$type = gsub(paste0("beta_", i), paste0("beta[",i, "]"), x$type)
      }

      y = ddply(x, "type", function(z){
        k = ksmooth(x = z$truth, y = z$cover, bandwidth = 0.1)
        fn = stepfun(x = k$x, y = c(0, k$y))
        xs = seq(from = min(k$x), to = max(k$x), length.out = 4e2)
        ys = fn(xs)
        data.frame(truth = xs, cover = ys, type = z$type[1], rep = z$rep[[1]])
      })

      pl = ggplot(y) + geom_line(aes_string(x = "truth", y = "cover")) + geom_abline(slope = 0, intercept = level) +
        facet_wrap(as.formula("~type"), scales = "free", labeller = label_parsed) + mytheme_straight()
      ggsave(paste0(to_trend, iden, "_beta_", level, " .png"), pl, height = 8, width = 8)
    })

    tmp =  ddply(l, c("simulation", "libraries", "analysis"), function(x){
      iden = paste("beta", level, x$simulation[1], x$libraries[1], x$analysis[1], sep = "-")
      iden = gsub("\\.", "-", iden)
      print(paste0("        ", iden))
      x = x[grepl("beta_", x$parameter) & x$level == level,]
      x = x[order(x$truth),]
      x$index = 1:dim(x)[1]

      x$type = as.character(x$type)
      for(i in 1:5){
        x$type = gsub(paste0("sigmaSquared_", i), paste0("sigma[",i, "]^2"), x$type)
        x$type = gsub(paste0("theta_", i), paste0("theta[",i, "]"), x$type)
        x$type = gsub(paste0("beta_", i), paste0("beta[",i, "]"), x$type)
      }

      y = ddply(x, c("rep", "type"), function(z){
        k = ksmooth(x = z$truth, y = z$cover, bandwidth = 0.1)
        fn = stepfun(x = k$x, y = c(0, k$y))
        xs = seq(from = min(k$x), to = max(k$x), length.out = 4e2)
        ys = fn(xs)
        data.frame(truth = xs, cover = ys, type = z$type[1], rep = z$rep[[1]])
      })

      pl = ggplot(y) + geom_line(aes_string(x = "truth", y = "cover", group = "rep")) + 
        geom_abline(slope = 0, intercept = level) +
        facet_wrap(as.formula("~type"), scales = "free_x", labeller = label_parsed) + 
        theme_few() + theme(strip.text.x = element_text(size = 14)) + 
        ylab("coverage") + xlab("parameter value")

      ggsave(paste0(to_trend, iden, ".pdf"), pl, height = 7, width = 7, dpi = 1200)
      ggsave(paste0(to_trend, iden, ".ps"), pl, height = 7, width = 7, dpi = 1200)
      ggsave(paste0(to_trend, iden, ".eps"), pl, height = 7, width = 7, dpi = 1200)
      ggsave(paste0(to_trend, iden, ".tiff"), pl, height = 7, width = 7, dpi = 1200)
    })

    l0 = l[grepl("beta", l$parameter) & l$level == level & l$rep == unique(l$rep)[1],]
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

      pl = ggplot(cover) + mytheme_straight() + facet_wrap(as.formula("~type"), scales = "free") +
        geom_segment(aes_string(x = "index", xend = "index", y = "lower", yend = "upper"), color = "darkGray") + 
        geom_point(aes_string(x = "index", y = "truth"), size = I(0.5))
      ggsave(paste0(to_cover, iden, "_beta_cover_", level, " .pdf"), pl, height = 8, width = 8)

      pl = ggplot(miss) + mytheme_straight() + facet_wrap(as.formula("~type"), scales = "free") +
        geom_segment(aes_string(x = "index", xend = "index", y = "lower", yend = "upper"), color = "darkGray") + 
        geom_point(aes_string(x = "index", y = "truth"), size = I(0.5))
      ggsave(paste0(to_miss, iden, "_beta_miss_", level, ".pdf"), pl, height = 8, width = 8)
    }, .progress = "text")
  }
}

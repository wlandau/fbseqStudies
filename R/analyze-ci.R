#' @title Function \code{ci}
#' @description Info to assesses coverage of beta parameters by credible intervals.
#' @export
#' @return Info to assesses coverage of beta parameters by credible intervals.
#' @param from directory with simulation lists
#' @param to where to put ci information
#' @param levels credible levels
ci = function(from, to, levels = c(0.5, 0.95)){
  from = newdir(from)
  to = newdir(to)
  out = NULL
  for(f in list.files(from)) {
    print(f)
    l = readRDS(paste0(from, f))
    for(a in l$analyses) for(level in levels){
      if(is.null(a$chains)) next()
      print(paste0("  ", a$analysis, " ", level))
      truth = lower = upper = NULL
      est = estimates(a$chains, level = level)
      pattern = "beta"
      if(l$simulation == "model" & grepl("fullybayes", a$analysis)) pattern = paste0(pattern, "|nu|tau|sigma|theta")
      est = est[grep(pattern, rownames(est)),]
      ns = rownames(est)
      t0 = l$scenario@supplement$truth
      
      if(l$simulation == "model"){
        truth = flatten(t0)[ns]
      } else {
        truth = as.numeric(as.matrix(t0$beta))
      }

      lower = est[, grep("lower", colnames(est))]
      upper = est[, grep("upper", colnames(est))]
      analysis = a$analysis
      simulation = l$simulation
      libraries =  meta(f)["libraries"]
      rep = meta(f)["rep"]

      names(truth) = NULL
      names(level) = NULL
      names(ns) = NULL
      names(lower) = NULL
      names(upper) = NULL
      names(analysis) = NULL      
      names(simulation) = NULL
      names(libraries) = NULL
      names(rep) = NULL

      d = data.frame(truth = truth, lower = lower, upper = upper, level = level, parameter = ns,
        simulation = simulation, libraries = libraries, analysis = analysis,
        cover = lower < truth & truth < upper, rep = rep)
      out = rbind(out, d)
    }
  }
  out$type = as.character(out$parameter)
  out$type[grepl("beta", out$type)] = gsub("_[0-9]*$", "", out$type[grepl("beta", out$type)])
  out$type = as.factor(out$type)
  if(is.null(out)) return()
  saveRDS(out, paste0(to, "ci.rds"))
}

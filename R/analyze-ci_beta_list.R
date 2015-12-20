#' @title Function \code{ci_beta_list}
#' @description Info to assesses coverage of beta parameters by credible intervals.
#' @export
#' @return Info to assesses coverage of beta parameters by credible intervals.
#' @param from directory with simulation lists
#' @param to where to put ci information
#' @param level credible level
ci_beta_list = function(from, to, level = 0.5){
  from = newdir(from)
  to = newdir(to)
  lower = upper = truth = NULL
  for(f in list.files(from)){
    print(f)
    l = readRDS(paste0(from, f))
    a = l$analyses[["fullybayes+normal"]]
    est = estimates(a$chains, level = level)
    est = est[grep("beta", rownames(est)),]
    ns = rownames(est)
    t0 = l$scenario@supplement$truth
    truth = cbind(truth, c(t0@beta))
    lower = cbind(lower, est[, grep("lower", colnames(est))])
    upper = cbind(upper, est[, grep("upper", colnames(est))])
  }
  rownames(truth) = rownames(lower) = rownames(upper) = ns
  out = list(truth = truth, lower = lower, upper = upper, level = level)
  saveRDS(out, paste0(to, "ci_beta_list_", level, ".rds"))
}

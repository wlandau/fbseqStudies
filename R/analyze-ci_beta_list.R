#' @title Function \code{ci_beta_list}
#' @description Info to assesses coverage of beta parameters by credible intervals.
#' @export
#' @return Info to assesses coverage of beta parameters by credible intervals.
#' @param path directory with simulation lists
#' @param level credible level
ci_beta_list = function(path, level = 0.5){
  path = newdir(path)
  lower = upper = truth = NULL
  for(f in list.files(path)){
    print(f)
    l = readRDS(paste0(path, f))
    a = l$analyses[["fullybayes+normal"]]
    est = estimates(a$chains, level = level)
    est = est[grep("beta", rownames(est)),]
    t0 = l$scenario@supplement$truth
    truth = cbind(truth, c(t0@beta))
    lower = cbind(lower, est[, grep("lower", colnames(est))])
    upper = cbind(upper, est[, grep("upper", colnames(est))])
  }
  rownames(truth) = rownames(lower)
  list(truth = truth, lower = lower, upper = upper, level = level)
}

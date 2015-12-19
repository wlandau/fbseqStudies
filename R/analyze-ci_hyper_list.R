#' @title Function \code{ci_hyper_list}
#' @description Info to assesses coverage of hyperparameters by credible intervals.
#' @export
#' @return Info to assesses coverage of hyperparameters by credible intervals.
#' @param path directory with simulation lists
#' @param level credible level
ci_hyper_list = function(path, level = 0.5){
  path = newdir(path)
  lower = upper = truth = NULL
  for(f in list.files(path)){
    print(f)
    l = readRDS(paste0(path, f))
    a = l$analyses[["fullybayes+normal"]]
    m = mcmc_samples(a$chains)
    m = m[,grep("nu|tau|sigma|theta", colnames(m))]
    t0 = l$scenario@supplement$truth
    truth = cbind(truth, c(t0@nu, t0@tau, t0@sigmaSquared, t0@theta))
    lower = cbind(lower, apply(m, 2, quantile, probs = (1 - level)/2))
    upper = cbind(upper, apply(m, 2, quantile, probs = 1 - (1 - level)/2))
  }
  rownames(truth) = rownames(lower)
  list(truth = truth, lower = lower, upper = upper, level = level)
}

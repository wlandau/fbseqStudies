#' @title Function \code{ci_hyper_level}
#' @description Info to assesses coverage of hyperparameters by credible intervals.
#' @export
#' @return Info to assesses coverage of hyperparameters by credible intervals.
#' @param from directory with simulation lists
#' @param to where to put ci information
#' @param level credible level
ci_hyper_level = function(from, to, level = 0.5){
  from = newdir(from)
  to = newdir(to)
  lower = upper = truth = NULL
  for(f in list.files(from)) if(meta(f)["simulation"] == "model"){
    print(f)
    l = readRDS(paste0(from, f))
    a = l$analyses[["fullybayes+normal"]]
    m = mcmc_samples(a$chains)
    m = m[,grep("nu|tau|sigma|theta", colnames(m))]
    t0 = l$scenario@supplement$truth
    truth = cbind(truth, c(t0@nu, t0@tau, t0@sigmaSquared, t0@theta))
    lower = cbind(lower, apply(m, 2, quantile, probs = (1 - level)/2))
    upper = cbind(upper, apply(m, 2, quantile, probs = 1 - (1 - level)/2))
  }
  rownames(truth) = rownames(lower)
  out = list(truth = truth, lower = lower, upper = upper, level = level)
  saveRDS(out, paste0(to, "ci_hyper_", level, ".rds"))
}

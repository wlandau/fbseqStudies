#' @title Function \code{roc}
#' @description function for calculating ROC curves
#' @export
#' @return data frame with an roc curve
#' @param probs probabilities
#' @param truth logical or 0/1 vector of classifications
#' @param cutoffs cutoffs for calculating the area below
roc = function(probs, truth, cutoffs = c(0.05, 0.1, 0.15, 0.2, 0.5, 1)){
  probs[is.na(probs)] = 0
  truth = truth[order(probs, decreasing = T)]
  fp = cumsum(!truth)
  tp = cumsum(truth)
  fpr = fp/max(fp)
  tpr = tp/max(tp)
  tpr[is.na(tpr)] = 0
  fn = stepfun(x = fpr, y = c(0, tpr))
  xs = seq(from = 0, to = 1, length.out = 4e2)
  ys = fn(xs)
  out = list(fpr = xs, tpr = ys)
  for(ct in cutoffs)
    out[[paste0("auc_", ct)]] = trapz(x = xs[xs < ct], y = ys[xs < ct])
  out
}

#' @title Function \code{roc_over}
#' @description use with \code{over_ans} to calculate ROC curves on analysis elements of simulation lists
#' @export
#' @param l simulation list
#' @param a analysis list
roc_over = function(l, a){
  if(l$simulation == "paschold") return(NULL)
  ch = Chain(l$scenario)
  
  tr = l$scenario@supplement$truth
  if(class(tr) == "Starts"){
    beta = l$scenario@supplement$truth@beta
  } else {
    beta = as.numeric(l$scenario@supplement$truth$beta)
  }

  ch@betaPostMean = beta
  ch@betaPostMeanSquare = 2*beta^2
  ef = effect_sizes(ch)
  truth = ef > 0

  n = colnames(ef)
  n = gsub("B73xMo17_Mo17xB73", "hybrids", n)
  n = gsub("B73xMo17", "hybrid1", n)
  n = gsub("Mo17xB73", "hybrid2", n)
  colnames(ef) = n

  e = a$estimates
  probs = e[,grep("prob", colnames(e))]
  ROC = list()
  for(i in 1:ncol(probs))
    ROC[[n[i]]] = roc(probs[,i], truth[,i])
  ROC
}

#' @title Function \code{rocs}
#' @description Compute roc curves for every \code{analyses} element of every simulation list in a directory
#' @export
#' @param from directory with simulation lists
#' @param to output directory
rocs = function(from, to){
  over_ans(from = from, to = to, fun = roc_over)
}

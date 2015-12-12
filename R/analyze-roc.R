#' @title Function \code{fpr_point}
#' @description internal function for computing ROC curves
#' @export
#' @return false positive rate
#' @param cutoff cutoff for fpr calculation
#' @param probs probabilities
#' @param n number of probabilities, not necessarily in \code{probs}
fpr_point = Vectorize(function(cutoff, probs, n){
  sum(probs >= cutoff)/n
}, "cutoff")

#' @title Function \code{roc}
#' @description function for calculating ROC curves
#' @export
#' @return data frame with an roc curve
#' @param probs probabilities
#' @param truth logical or 0/1 vector of classifications
roc = function(probs, truth){
  probs[is.na(probs)] = 0
  index = order(probs, decreasing = T)
  truth = truth[index]
  probs = probs[index]
  fpr = fpr_point(probs, probs[!truth], length(truth) - sum(truth))
  tpr = fpr_point(probs, probs[truth], sum(truth))
  fpr[!is.finite(fpr)] = 0
  tpr[!is.finite(tpr)] = 0
  data.frame(fpr = fpr, tpr = tpr)
}

#' @title Function \code{auc}
#' @description calculates areas under ROC curves
#' @export
#' @return area under an ROC curve
#' @param roc data frame with columns named \code{fpr} and \code{tpr}
#' @param cutoff fpr cutoff below which area will be calculated
auc = function(roc, cutoff = 0.1){
  fpr = roc$fpr
  tpr = roc$tpr
  fn = stepfun(x = fpr, y = c(0, tpr))
  xs = seq(from = 0, to = cutoff, length.out = 1e3)
  trapz(x = xs, y = fn(xs))
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

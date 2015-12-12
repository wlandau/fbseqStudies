#' @title Function \code{fdr}
#' @description function for calculating fdr curves
#' @export
#' @return data frame with an fdr curve
#' @param probs probabilities
#' @param truth logical or 0/1 vector of classifications
fdr = function(probs, truth){
  index = order(probs, decreasing = T)
  probs = probs[index]
  truth = truth[index]
  pH0 = 1 - probs
  bayesian_fdr = cumsum(pH0)/(1:length(pH0))
  fdp = cumsum(!truth)/(1:length(truth))
  fn = stepfun(x = bayesian_fdr, y = c(0, fdp))
  xs = seq(from = 0, to = max(bayesian_fdr), length.out = 4e2)
  ys = fn(xs)
  data.frame(bayesian_fdr = xs, fdp = ys, fdp_minus_fdr = ys - xs) # plot vs fdr
}

#' @title Function \code{fdr_over}
#' @description use with \code{over_ans} to calculate fdr curves on analysis elements of simulation lists
#' @export
#' @param l simulation list
#' @param a analysis list
fdr_over = function(l, a){
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
  fdr = list()
  for(i in 1:ncol(probs))
    fdr[[n[i]]] = fdr(probs[,i], truth[,i])
  fdr
}

#' @title Function \code{fdrs}
#' @description Compute fdr curves for every \code{analyses} element of every simulation list in a directory
#' @export
#' @param from directory with simulation lists
#' @param to output directory
fdrs = function(from, to){
  over_ans(from = from, to = to, fun = fdr_over)
}

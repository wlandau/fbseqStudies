#' @title Function \code{calibration}
#' @description assess calibration of posterior probabilities
#' @export
#' @return data frame with an calibration curve
#' @param probs probabilities
#' @param truth logical or 0/1 vector of classifications
calibration = function(probs, truth){
  index = order(probs)
  probs = probs[index]
  truth = as.numeric(truth[index])
  k = ksmooth(x = probs, y = truth, bandwidth = 0.1)
  fn = stepfun(x = k$x, y = c(0, k$y))
  xs = seq(from = 0, to = 1, length.out = 4e2)
  ys = fn(xs)
  data.frame(fdr = xs, fdp = ys, fdp_minus_fdr = ys - xs) # plot vs fdr
}

#' @title Function \code{calibration_over}
#' @description use with \code{over_ans} to calculate calibration curves on analysis elements of simulation lists
#' @export
#' @param l simulation list
#' @param a analysis list
calibration_over = function(l, a){
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
  calibration = list()
  for(i in 1:ncol(probs))
    calibration[[n[i]]] = calibration(probs[,i], truth[,i])
  calibration
}

#' @title Function \code{calibrations}
#' @description Compute calibration curves for every \code{analyses} element of every simulation list in a directory
#' @export
#' @param from directory with simulation lists
#' @param to output directory
calibrations = function(from, to){
  over_ans(from = from, to = to, fun = calibration_over)
}

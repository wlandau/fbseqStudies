#' @title Function \code{fdr}
#' @description function for calculating fdr curves
#' @export
#' @return data frame with an fdr curve
#' @param probs probabilities
#' @param truth logical or 0/1 vector of classifications
fdr = function(probs, truth){

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
